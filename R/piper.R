#' @title piper
#' @description Deep asymmetric object extraction.
#' @param lock_objects should namespace be locked, DEFAULT: FALSE
#' @importFrom purrr map map_int map_dfr pluck
#' @importFrom stringr str_detect
#' @importFrom dplyr nth
#' @export piper
piper <- R6::R6Class(
    "piper",
    lock_objects = FALSE,
    public = list(
        #' @param dir target directory of computational blocks
        initialize = function(dir = character(0)) {
            self$namespace <- {}
            self$stack <- {}
            self$imports <- {}
            self$dir <- dir
            self$env <- new.env()
            self$capture_enabled <- FALSE
            self$captured_imports <- list()
            self$captured_exports <- list()
            self$watch_reported <- character(0)
        },
        #' @description reset watch state (reported variables) for next pipeline run
        reset_watch = function() {
            self$watch_reported <- character(0)
        },
        #' @description enable or disable capture of block inputs and exports
        #' @param enable if TRUE, enable capture and clear any previous captured data
        set_capture = function(enable = TRUE) {
            self$capture_enabled <- isTRUE(enable)
            if (self$capture_enabled) {
                self$captured_imports <- list()
                self$captured_exports <- list()
            }
        },
        #' @description return captured imports and exports (when capture was enabled)
        get_captured = function() {
            list(
                imports = self$captured_imports,
                exports = self$captured_exports
            )
        },
        #' @description execute a pipe block from the top of the stack
        #' @param name a computational block to execute
        #' @param .debug should debug mode get triggered, DEFAULT: FALSE
        compute = function(name, .debug = FALSE) {
            if (name %in% names(self$imports)) {
                import <- self$imports[[name]]
                local_env <- new.env()
                args <- self$memory_map(import, local_env)

                # Watch: which variables exist before this block. Block runs in parent.frame();
                # we also check self$env so we see vars in the pipeline env (e.g. set by set_env).
                watch_list <- private$get_watch_list()
                run_env <- parent.frame()
                exists_in <- function(v, env, inherits = TRUE) {
                    exists(v, envir = env, inherits = inherits)
                }
                state_before <- if (length(watch_list) > 0) {
                    watch_list[vapply(
                        watch_list,
                        function(v) exists_in(v, run_env) || exists_in(v, self$env),
                        logical(1)
                    )]
                } else {
                    character(0)
                }

                if (self$capture_enabled) {
                    self$captured_imports[[name]] <- private$snapshot_imports(
                        args$imports,
                        self$env
                    )
                }
                if (.debug) {
                    tmp_path <- paste0(
                        self$dir,
                        "/",
                        basename(tempfile()),
                        ".R"
                    )
                    writeLines(deparse(import), tmp_path)
                    private$attach_browser(tmp_path)
                    response <- source(tmp_path)
                    if (file.exists(tmp_path)) file.remove(tmp_path)
                } else {
                    response <- eval(import)
                }
                if (self$capture_enabled) {
                    self$captured_exports[[name]] <- private$snapshot_exports(
                        args$export,
                        self$env
                    )
                }

                # Watch: report when watched variables first become available
                if (length(watch_list) > 0) {
                    state_after <- watch_list[vapply(
                        watch_list,
                        function(v) exists_in(v, run_env) || exists_in(v, self$env),
                        logical(1)
                    )]
                    new_in_block <- setdiff(state_after, state_before)
                    already_but_not_reported <- setdiff(state_before, self$watch_reported)
                    for (v in already_but_not_reported) {
                        message("[piper watch] '", v, "' was already present when block '", name, "' started")
                    }
                    for (v in new_in_block) {
                        message("[piper watch] '", v, "' first became available after block '", name, "'")
                    }
                    self$watch_reported <- c(
                        self$watch_reported,
                        already_but_not_reported,
                        new_in_block
                    )
                }

                if (Negate(`%in%`)(name, self$stack)) {
                    self$stack <- append(self$stack, name)
                }
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

            # Validate imports structure at push time
            if (!is.null(args$imports)) {
                depends <- if (is.null(args$depends)) list() else args$depends
                self$validate_imports_structure(args$imports, depends, args$id)
            }

            module <- paste0(args$id, ".__mod__")
            if (args$id %in% names(self$imports)) {
                msg <- paste0("Multiple imports found for <", module, ">")
                self$imports[args$id] <- list(expr)
                warning(msg)
            } else {
                self$imports <- append(
                    self$imports,
                    setNames(list(expr), args$id)
                )
                self$blocks <- c(self$blocks, module)
            }
        },
        #' @description load a module into local memory
        #' @param module a pipe module id
        #' @param from the module root directory
        #' @param .env local pipe environment
        #' @param header header file name, DEFAULT: "main.r"
        #' @param src binary type, DEFAULT: "R++"
        #' @param pipe the root pipe to use, DEFAULT: "modules"
        #' @param version library version, DEFAULT: ""
        load = function(
            module,
            from,
            .env = self$get_env(),
            header = "main.r",
            src = "R++",
            pipe = "modules",
            version = ""
        ) {
            args <- list(local_env_ = .env)
            file <- paste(module, version, header, sep = "/")
            path <- paste(from, src, pipe, file, sep = "/")
            self$dir <- paste(from, src, pipe, sep = "/")
            source(path, local = list2env(args))
            self$map_pipeline()
        },
        #' @description map modules pipeline
        map_pipeline = function() {
            local_env <- new.env()
            types <- c("module", "workflow", "dependencies", "imports")
            pipeline <- map_dfr(
                self$imports,
                .f = function(expr) {
                    args <- self$memory_map(expr, local_env)

                    # Format imports for display
                    imports_display <- ""
                    if (!is.null(args$imports) && length(args$imports) > 0) {
                        imports_parts <- sapply(
                            names(args$imports),
                            function(source) {
                                vars <- paste(
                                    args$imports[[source]],
                                    collapse = ", "
                                )
                                paste0(source, ": [", vars, "]")
                            }
                        )
                        imports_display <- paste(imports_parts, collapse = "; ")
                    }

                    list(
                        module = args$id,
                        workflow = args$description,
                        dependencies = if (is.null(args$depends) || length(args$depends) == 0) {
                            ""
                        } else {
                            paste(args$depends, collapse = ", ")
                        },
                        imports = imports_display
                    )
                }
            )

            pretty_print_table(setNames(pipeline, types))
        },
        #' @description pop block from stack
        #' @param name a block id
        compute_stack = function(name) {
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
        },
        #' @description validate imports structure
        #' @param imports imports declaration from module metadata
        #' @param depends list of module dependencies
        #' @param block_id module block ID for error messages
        validate_imports_structure = function(imports, depends, block_id) {
            if (is.null(imports) || length(imports) == 0) {
                return(TRUE) # No imports declared, that's fine
            }

            if (!is.list(imports)) {
                stop(paste0(
                    "Block ID: {",
                    block_id,
                    "}, imports must be a list"
                ))
            }

            # Check that all keys in imports are either in depends or are "global"
            import_sources <- names(imports)
            invalid_sources <- setdiff(import_sources, c(depends, "global"))

            if (length(invalid_sources) > 0) {
                msg <- paste0(
                    "Block ID: {",
                    block_id,
                    "}, ",
                    "Invalid import sources: ",
                    paste(invalid_sources, collapse = ", "),
                    ". ",
                    "Import sources must be either in dependencies (",
                    paste(depends, collapse = ", "),
                    ") or 'global'"
                )
                stop(msg)
            }

            # Check that all values are lists
            for (source in import_sources) {
                if (!is.list(imports[[source]])) {
                    stop(paste0(
                        "Block ID: {",
                        block_id,
                        "}, imports from '",
                        source,
                        "' must be a list of variable names"
                    ))
                }
            }

            return(TRUE)
        },
        #' @description validate imports availability
        #' @param imports imports declaration from module metadata
        #' @param env module execution environment
        #' @param stack current execution stack
        #' @param block_id module block ID for error messages
        validate_imports_availability = function(
            imports,
            env,
            stack,
            block_id
        ) {
            if (is.null(imports) || length(imports) == 0) {
                return(TRUE) # No imports declared, that's fine
            }

            missing_vars <- list()

            for (source in names(imports)) {
                var_names <- imports[[source]]

                if (source == "global") {
                    # Check global environment
                    for (var_name in var_names) {
                        if (!exists(var_name, envir = .GlobalEnv)) {
                            missing_vars <- append(
                                missing_vars,
                                list(list(source = "global", var = var_name))
                            )
                        }
                    }
                } else {
                    # Check module dependency - variables should be in the execution environment
                    # after the dependency module has been executed
                    if (!(source %in% stack)) {
                        # Module not yet executed, this should have been caught by dependency resolution
                        # But we'll check anyway
                        msg <- paste0(
                            "Block ID: {",
                            block_id,
                            "}, ",
                            "Dependency module '",
                            source,
                            "' has not been executed yet"
                        )
                        stop(msg)
                    }

                    for (var_name in var_names) {
                        if (!exists(var_name, envir = env)) {
                            missing_vars <- append(
                                missing_vars,
                                list(list(source = source, var = var_name))
                            )
                        }
                    }
                }
            }

            if (length(missing_vars) > 0) {
                missing_msgs <- sapply(missing_vars, function(x) {
                    paste0("'", x$var, "' from '", x$source, "'")
                })
                msg <- paste0(
                    "Block ID: {",
                    block_id,
                    "}, ",
                    "Missing required imports: ",
                    paste(missing_msgs, collapse = ", ")
                )
                stop(msg)
            }

            return(TRUE)
        }
    ),
    private = list(
        get_watch_list = function() {
            w <- getOption("piper.watch", NULL)
            if (is.null(w) || length(w) == 0) {
                return(character(0))
            }
            unique(as.character(unlist(w)))
        },
        snapshot_imports = function(module_imports, env) {
            if (is.null(module_imports) || length(module_imports) == 0L) {
                return(list())
            }
            out <- list()
            for (source in names(module_imports)) {
                var_names <- module_imports[[source]]
                out[[source]] <- list()
                for (var_name in var_names) {
                    if (exists(var_name, envir = env, inherits = FALSE)) {
                        out[[source]][[var_name]] <- get(
                            var_name,
                            envir = env,
                            inherits = FALSE
                        )
                    } else {
                        out[[source]][[var_name]] <- NULL
                    }
                }
            }
            out
        },
        snapshot_exports = function(module_export, env) {
            if (is.null(module_export) || length(module_export) == 0L) {
                return(list())
            }
            out <- list()
            for (export_name in module_export) {
                if (exists(export_name, envir = env, inherits = FALSE)) {
                    out[[export_name]] <- get(
                        export_name,
                        envir = env,
                        inherits = FALSE
                    )
                } else {
                    out[[export_name]] <- NULL
                }
            }
            out
        },
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
            on_exit <- paste0(
                "on.exit({file.remove('",
                file_path,
                "')}, add = TRUE)"
            )

            browser_header <- "tryCatch({"
            browser_footer <- paste0(
                "}, finally = {if (file.exists('",
                file_path,
                "')) file.remove('",
                file_path,
                "')})"
            )
            # Insert browser() at the beginning of the file content
            file_content[length(file_content)] <- paste0(
                file_content[length(file_content)],
                browser_footer
            )
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
#' @param .env a target environment hook, DEFAULT: parent.frame()
#' @param auto_purge should existing module be purged, DEFAULT: TRUE
#' @param ... a list of functional arguments
#' @export piper.new
piper.new <- function(.env = parent.frame(), auto_purge = TRUE, ...) {
    #nolintr
    .pipe <- ".."
    if (
        exists(.pipe, envir = .env) &&
            inherits(get(.pipe, envir = .env), "piper")
    ) {
        if (auto_purge) {
            piper.purge(.env = .env)
            warning(paste(
                .pipe,
                " << is already defined. Applying auto-purge to release module.
                Set auto_purge = FALSE to prevent this in the future."
            ))
            assign(.pipe, piper$new(...), envir = .env)
        } else {
            warning(paste(
                .pipe,
                " << is already defined. 
                Consider using piper.purge() to release the module before declaration."
            ))
        }
    } else {
        assign(.pipe, piper$new(...), envir = .env)
    }
}

#' @title piper.make
#' @description Create new piper root
#' @param pipe the desired sub-directory containing pipe modules
#' @param root the desired parent directory to house module assets
#' @param mode rw+ permission settings
#' @export piper.make
piper.make <- function(pipe, root = "R++", mode = "0755") {
    #nolintr
    # Check if root directory exists
    if (!dir.exists(root)) {
        # Try to create the root directory
        tryCatch(
            {
                dir.create(root, mode = mode, recursive = TRUE)
                cat(paste("Created root directory: '", root, "'\n", sep = ""))
            },
            error = function(e) {
                stop(paste(
                    "Failed to create root directory '",
                    root,
                    "': ",
                    e$message,
                    sep = ""
                ))
            }
        )
    }

    # Create the full path for the subdirectory
    sub_dir_path <- file.path(root, pipe)

    # Check if subdirectory already exists
    if (dir.exists(sub_dir_path)) {
        message(paste(
            "Subdirectory '",
            sub_dir_path,
            "' already exists.",
            sep = ""
        ))
        return(invisible(sub_dir_path))
    }

    # Try to create the subdirectory
    tryCatch(
        {
            dir.create(sub_dir_path, mode = mode, recursive = TRUE)
            cat(paste(
                "Successfully created subdirectory: '",
                sub_dir_path,
                "'\n",
                sep = ""
            ))
            return(invisible(sub_dir_path))
        },
        error = function(e) {
            if (grepl("Permission denied", e$message)) {
                stop(paste(
                    "Permission denied: Cannot create subdirectory '",
                    sub_dir_path,
                    "'. Check your write permissions for the root directory.",
                    sep = ""
                ))
            } else {
                stop(paste(
                    "Failed to create subdirectory '",
                    sub_dir_path,
                    "': ",
                    e$message,
                    sep = ""
                ))
            }
        }
    )
}

#' @title piper.module
#' @description Create new module library
#' @param pipe the root pipe to use
#' @param module a library name
#' @param parent the desired parent directory to house module assets
#' @param mode rw+ permission settings
#' @export piper.module
piper.module <- function(pipe, module, parent = "R++", mode = "0755") {
    # Create the full path for the subdirectory
    sub_pipe_path <- file.path(parent, pipe)
    # Check if parent directory exists
    if (!dir.exists(sub_pipe_path)) {
        stop(paste("Root pipe '", pipe, "' does not exist.", sep = ""))
    }

    # Create the full path for the subdirectory
    sub_dir_path <- file.path(parent, pipe, module)

    # Check if subdirectory already exists
    if (dir.exists(sub_dir_path)) {
        message(paste("Library '", sub_dir_path, "' already exists.", sep = ""))
        return(invisible(sub_dir_path))
    }

    # Try to create the subdirectory
    tryCatch(
        {
            dir.create(sub_dir_path, mode = mode, recursive = TRUE)
            cat(paste(
                "Successfully initialized library: '",
                sub_dir_path,
                "'\n",
                sep = ""
            ))
            return(invisible(sub_dir_path))
        },
        error = function(e) {
            if (grepl("Permission denied", e$message)) {
                stop(paste(
                    "Permission denied: Cannot initialize library '",
                    sub_dir_path,
                    "'. Check your write permissions for the root pipe",
                    sep = ""
                ))
            } else {
                stop(paste(
                    "Failed to initialize library '",
                    sub_dir_path,
                    "': ",
                    e$message,
                    sep = ""
                ))
            }
        }
    )
}

#' @title piper.brew
#' @param pipe module root pipe, DEFAULT: modules
#' @param module module module
#' @param method a method name
#' @param version library version
#' @param header header file name, DEFAULT: "main.r"
#' @param parent the desired parent directory to house module assets
#' @param mode rw+ permission settings
#' @description Create new library method.
#' @export piper.brew
piper.brew <- function(
    pipe,
    module,
    method,
    version,
    header = "main.r",
    parent = "R++",
    mode = "0755"
) {
    #nolintr
    # Check to see if path exists: (eg. modules/accounting)
    # Check if parent directory exists
    sub_pipe_path <- file.path(parent, pipe)
    if (!dir.exists(sub_pipe_path)) {
        stop(paste("Root pipe '", pipe, "' does not exist.", sep = ""))
    }

    version_path <- file.path(parent, pipe, module, version)
    # Try to create the subdirectory
    tryCatch(
        {
            dir.create(version_path, mode = mode, recursive = TRUE)
            cat(paste(
                "Successfully accessed module: '",
                version_path,
                "'\n",
                sep = ""
            ))
            header_path <- file.path(version_path, header)
            file.create(header_path)
            return(invisible(version_path))
        },
        error = function(e) {
            if (grepl("Permission denied", e$message)) {
                stop(paste(
                    "Permission denied: Cannot access module '",
                    version_path,
                    "'. Check your write permissions for the root pipe",
                    sep = ""
                ))
            } else {
                stop(paste(
                    "Failed to access module '",
                    version_path,
                    "': ",
                    e$message,
                    sep = ""
                ))
            }
        }
    )
}

#' @title piper.load
#' @description Load a newly initialized piper instance.
#' @param module the name of the source asset
#' @param from target directory
#' @param .env a target environment hook, DEFAULT: parent.frame()
#' @param ... additional arguments passed to module load
#' @export piper.load
piper.load <- function(module, from = ".", .env = rlang::caller_env(), ...) {
    #nolintr
    ..$set_env(.env = .env)
    ..$load(module, from, ...)
}

#' @title piper.capture
#' @description Enable or disable capture of block inputs and exports for each
#'   subsequent \code{...pipe()} call. When enabled, the piper instance records
#'   each block's imports (before run) and exports (after run) into in-memory
#'   lists. Retrieve them with \code{..$get_captured()} (returns
#'   \code{list(imports = <named list>, exports = <named list>)}).
#' @param enable if TRUE, enable capture and clear any previous captured data;
#'   if FALSE, disable capture.
#' @param .env environment where \code{..} (piper instance) is bound, DEFAULT: .GlobalEnv
#' @export piper.capture
piper.capture <- function(enable = TRUE, .env = .GlobalEnv) {
    if (!exists("..", envir = .env)) {
        stop("Piper not initialized. Run piper.new() and piper.load(...) first.")
    }
    pipe <- get("..", envir = .env)
    if (!inherits(pipe, "piper")) {
        stop("'..' is not a piper instance.")
    }
    pipe$set_capture(enable)
    invisible(NULL)
}

#' @title piper.watch
#' @description Register variable names to watch during pipeline execution. When
#'   you run the pipeline (via \code{...pipe()} or \code{..$compute()}), the
#'   piper instance reports when each watched variable first becomes available
#'   in the pipeline environment: either already present when a block started, or
#'   first became available after a given block. Useful for fixing tests or
#'   imports when a variable exists in a full run but is missing when a module
#'   is tested in isolation.
#' @param vars character vector or list of variable names to watch (e.g.
#'   \code{c("var1", "var2")} or \code{list("var1", "var2")}).
#' @param .env environment where \code{..} (piper instance) is bound, DEFAULT: .GlobalEnv
#' @export piper.watch
piper.watch <- function(vars, .env = .GlobalEnv) {
    vars <- unique(as.character(unlist(vars)))
    options(piper.watch = if (length(vars) > 0) vars else NULL)
    if (length(vars) > 0) {
        message("[piper watch] monitoring for variables: ", paste(vars, collapse = ", "))
    } else {
        message("[piper watch] cleared (no variables monitored)")
    }
    if (exists("..", envir = .env)) {
        pipe <- get("..", envir = .env)
        if (inherits(pipe, "piper")) {
            pipe$reset_watch()
        }
    }
    invisible(NULL)
}

#' @title piper.purge
#' @description Purge module pipe.
#' @param .env a target environmehnt hook, DEFAULT: parent.frame()
#' @export piper.purge
piper.purge <- function(.env = parent.frame()) {
    #nolintr
    .purge <- c(
        unlist(..$namespace),
        ..$get_stack(),
        ".."
    )
    rm(list = .purge, envir = .env)
    gc(full = TRUE, reset = TRUE)
}

#' @title ...push
#' @description Populate piper module stack.
#' @param .this state initializers
#' @param .with expression constructors
#' @param ... a list of functional arguments
#' @export ...push
...push <- function(.this = {}, .with = {}, ...) {
    #nolintr
    ..$push(substitute(.this %>>% .with), ...)
}

#' @title ...pipe
#' @description Execute a piper module.
#' @param ... a list of language expressions
#' @export ...pipe
...pipe <- function(...) {
    #nolintr
    ..$compute(...)
}

#' @title ...env
#' @description Fetch global module environment.
#' @export ...env
...env <- function() {
    #nolintr
    ..$get_env()
}

# Suppress warnings for dynamically assigned global variable
utils::globalVariables("..")
