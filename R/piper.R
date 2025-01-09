#' @title piper
#' @description Deep asymmetric object extraction.
#' @param lock_objects should namespace be locked, DEFAULT: FALSE
#' @importFrom purrr map map_int map_dfr pluck
#' @importFrom stringr str_detect
#' @importFrom dplyr nth
#' @export piper
piper <- R6::R6Class("piper",
    lock_objects = FALSE,
    public = list(
        #' @param dir target directory of computational blocks
        initialize = function(dir = character(0)) {
            self$namespace <- {}
            self$stack <- {}
            self$imports <- {}
            self$dir <- dir
            self$env <- new.env()
        },
        #' @description execute a pipe block from the top of the stack
        #' @param name a computational block to execute
        #' @param .debug should debug mode get triggered, DEFAULT: FALSE
        pop = function(name, .debug = FALSE) {
            if (name %in% names(self$imports)) {
                import <- self$imports[[name]]
                if (.debug) {
                    tmp_path <- paste0(self$dir, "/", basename(tempfile()), ".R")
                    writeLines(deparse(import), tmp_path)
                    private$attach_browser(tmp_path)
                    response <- source(tmp_path)
                    if (file.exists(tmp_path)) file.remove(tmp_path)
                } else {
                    response <- eval(import)
                }
                if (Negate(`%in%`)(name, self$stack)) self$stack <- append(self$stack, name)
            } else {
                msg <- paste("Missing import for requested block:", name)
                stop(msg)
            }
        },
        #' @description insert a pipe block into the top of the stack
        #' @param expr expression block to insert
        #' @param ... a list of other functional arguments
        push = function(expr, ...) {
            local_env <- new.env()
            self$imports <- append(self$imports, list())
            args <- self$memory_map(expr, local_env)
            module <- paste0(args$id, ".__mod__")
            if (args$id %in% names(self$imports)) {
                msg <- paste0("Multiple imports found for <", module, ">")
                self$imports[args$id] <- list(expr)
                warning(msg)
            } else {
                self$imports <- append(self$imports, setNames(list(expr), args$id))
                self$blocks <- c(self$blocks, module)
            }
        },
        #' @description load a module into local memory
        #' @param module a pipe module id
        #' @param from the module root directory
        #' @param .env local pipe environment
        #' @param src binary type
        #' @param dir the module subdirectory
        load = function(module, from, .env = self$get_env(), src = "R", dir = "modules") {
            args <- list(local_env_ = .env)
            file <- paste(module, "R", sep = ".")
            path <- paste(from, src, "modules", file, sep = "/")
            self$dir <- paste(from, src, "modules", sep = "/")
            source(path, local = list2env(args))
            self$map_pipeline()
        },
        #' @description map modules pipeline
        map_pipeline = function() {
            local_env <- new.env()
            types <- c("module", "workflow", "dependencies")
            pipeline <- map_dfr(
                self$imports,
                .f = function(expr) {
                    args <- self$memory_map(expr, local_env)

                    list(
                        module = args$id,
                        workflow = args$description,
                        dependencies = args$depends
                    )
                }
            )

            pretty_print_table(setNames(pipeline, types))
        },
        #' @description pop block from stack
        #' @param name a block id
        pop_stack = function(name) {
            self$stack <- setdiff(self$stack, name)
        },
        #' @description fetch stack namespace
        get_stack = function() {
            return(self$stack)
        },
        #' @description fetch block namespace
        #' @param block_id a block id
        get_namespace = function(block_id) {
            return(self$namespace[[block_id]])
        },
        #' @description update block namespace
        #' @param block_id a block id
        #' @param members block attributes
        update_namespace = function(block_id, members) {
            self$namespace[[block_id]] <- members
        },
        #' @description initialize local environment
        #' @param .env a block id
        set_env = function(.env) {
            self$env <- .env
        },
        #' @description fetch local environment
        get_env = function() {
            return(self$env)
        },
        #' @description shallow copy of local environment to global environment
        #' @param expr an expression block
        #' @param .env local environment
        memory_map = function(expr, .env) {
            eval(expr[-1][[1]], envir = .env)
            local_namespace <- ls(envir = .env)
            args <- mget(local_namespace, envir = .env)

            return(args)
        }
    ),
    private = list(
        build_blocks = function(blocks) {
            imports <- gsub(
                pattern = "\\..*",
                replacement = "",
                blocks
            )
            return(setNames(as.list(blocks), imports))
        },
        extract_calls = function(file_path) {
            code <- parse(file = file_path)
            tokens <- as.list(code)
            calls <- c()

            while (TRUE) {
                any_unpacked <- FALSE

                for (ii in seq_along(tokens)) {
                    part <- tokens[[ii]]

                    if (!inherits(try(eval(part), silent = TRUE), "try-error")) {
                        if (is.call(part)) {
                            fun_token <- part[[1]]
                            calls <- c(calls, deparse(fun_token))
                        }
                        # Expressions have a length
                        if (length(part) > 1) {
                            tokens[[ii]] <- as.list(part)
                            any_unpacked <- TRUE
                        }
                    }
                }

                tokens <- unlist(tokens)
                if (!any_unpacked) break
            }
            unique(calls)
        },
        attach_browser = function(file_path) {
            # Read the entire source file content
            file_content <- readLines(con = file_path)

            # Create a call to browser()
            on_exit <- paste0("on.exit({file.remove('", file_path, "')}, add = TRUE)")

            browser_header <- "tryCatch({"
            browser_footer <- paste0("}, finally = {if (file.exists('", file_path, "')) file.remove('", file_path, "')})")
            # Insert browser() at the beginning of the file content
            file_content[length(file_content)] <- paste0(file_content[length(file_content)], browser_footer)
            new_content <- c(browser_header, file_content)
            pattern <- "%>>%\\s*\\{"
            replacement <- paste0(" %>>% \\{ \nbrowser(); ", on_exit)

            # Replace occurrences of the pattern with the replacement text
            new_content <- gsub(pattern, replacement, new_content, perl = TRUE)

            # Write the modified content back to the file
            writeLines(new_content, file_path)
        }
    )
)

#' @title piper.new
#' @description Initialize a new piper instance.
#' @param .env a target environmehnt hook, DEFAULT: parent.frame()
#' @param ... a list of functional arguments
#' @export piper.new
piper.new <- function(.env = parent.frame(), auto_purge = TRUE, ...) { #nolintr
    .pipe <- "module_"
    if (exists(.pipe) && inherits(get(.pipe), "piper")) {
        if (auto_purge) {
            piper.purge()
            warning(paste(.pipe, " << is already defined. Applying auto-purge to release module.
                Set auto_purge = FALSE to prevent this in the future."))
        } else {
            warning(paste(.pipe, " << is already defined. 
                Consider using piper.purge() to release the module before decleration."))
        }
    } else {
        assign(.pipe, piper$new(...), envir = .env)
    }
}

#' @title piper.load
#' @description Load a newly initialized piper instance.
#' @param module the name of the source asset
#' @param from target directory
#' @param .env a target environmehnt hook, DEFAULT: parent.frame()
#' @export piper.load
piper.load <- function(module, from, .env = rlang::caller_env(), ...) { #nolintr
    module_$set_env(.env = .env)
    module_$load(module, from, ...)
}

#' @title piper.purge
#' @description Purge module pipe.
#' @export piper.purge
piper.purge <- function(.env = parent.frame()) { #nolintr
    .purge <- c(
        unlist(module_$namespace),
        module_$get_stack(),
        "module_"
    )
    rm(list = .purge, envir = .env)
    gc(full = TRUE, reset = TRUE)
}

#' @title module_.push
#' @description Populate piper module stack.
#' @param .this state initializers
#' @param .with expression constructors
#' @param ... a list of functional arguments
#' @export module_.push
module_.push <- function(.this = {}, .with = {}, ...) { #nolintr
    module_$push(substitute(.this %>>% .with), ...)
}

#' @title module_.pop
#' @description Execute a piper module.
#' @param ... a list of language expressions
#' @export module_.pop
module_.pop <- function(...) { #nolintr
    module_$pop(...)
}

#' @title module_.env
#' @description Fetch global module environment.
#' @export module_.env
module_.env <- function() { #nolintr
    module_$get_env()
}
