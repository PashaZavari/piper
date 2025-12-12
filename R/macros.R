#' @name %>>%
#' @title Custom pipe operator
#' @description Custom pipe operator
#' @param lhs a valid R expression
#' @param rhs a valid R expression
#' @export `%>>%`
`%>>%` <- function(lhs, rhs) {
    # nolint
    parent_env <- parent.frame(n = 1)
    if (exists("module_", envir = .GlobalEnv)) {
        pipe <- get("module_", envir = .GlobalEnv)
        stack <- pipe$get_stack()
        parent_env <- pipe$get_env()
    } else {
        stack <- {}
        warning("Running %>>% without any pipe definitions.")
    }

    local_env <- new.env()
    eval(substitute(lhs), envir = local_env)
    local_namespace <- ls(envir = local_env)
    args <- mget(local_namespace, envir = local_env)
    # Intentional assignment to .GlobalEnv: module environments are stored globally
    # for inter-module communication and namespace management
    assign(args$id, local_env, envir = .GlobalEnv)

    error_guard(
        expr = substitute(rhs),
        args = args,
        env = parent_env,
        stack = stack
    )
}

#' @name %-%
#' @title Fail-safe object initializer & validator
#' @description Fail-safe object initializer & validator
#' @param lhs a valid R object reference
#' @param rhs a valid R object reference
#' @export `%-%`
`%-%` <- function(lhs, rhs) {
    if (inherits(try(lhs, silent = TRUE), "try-error")) {
        return(rhs)
    } else {
        value <- eval(lhs)
        expecting <- typeof(rhs)
        actual <- typeof(value)
        if (actual != expecting && !is.null(rhs)) {
            msg <- paste0(
                "Validation failed for: ",
                deparse(substitute(lhs)),
                " \n Expecting -> {",
                expecting,
                "} \n Actual -> {",
                actual,
                "}"
            )
            stop(msg)
        } else {
            if (length(value) == 0) {
                value <- NA
            }
            return(value)
        }
    }
}

#' @title error_guard
#' @description Custom internal error handler.
#' @param expr a valid R expression to error check
#' @param args optional meta information
#' @param env parent environment pointer
#' @param stack pipe dependency stack
#' @param pipe piper pipe pointer
#' @export error_guard
error_guard <- function(expr, args, env, stack, pipe) {
    # Extract block ID:
    block_id <- args$id
    on_error <- args$onError
    depends <- args$depends
    export <- args$export
    deport <- args$deport
    imports <- args$imports
    .id <- paste0("Block ID: {", block_id, "}")
    message(paste(Sys.time(), block_id, sep = " -> "))

    if (length(depends) > 0) {
        pipe <- get("module_", envir = .GlobalEnv)
        if (length(stack) > 0) {
            is_missing <- setdiff(depends, stack)
            if (length(is_missing) > 0) {
                for (import in depends) {
                    auto_load <- try(pipe$compute(import), silent = FALSE)
                    message(
                        "*** Missing import for requested block:",
                        import,
                        ". Attempting auto-load..."
                    )
                    if (inherits(auto_load, "try-error")) {
                        pipe$compute_stack(import)
                        msg <- paste(
                            "Auto-load failed for requested block import:",
                            import
                        )
                        stop(msg)
                    } else {
                        message("Auto-load successful!")
                    }
                }
            }
        } else {
            for (import in depends) {
                auto_load <- try(pipe$compute(import), silent = TRUE)
                message(
                    "*** Missing import for requested block:",
                    import,
                    ". Attempting auto-load..."
                )
                if (inherits(auto_load, "try-error")) {
                    pipe$compute_stack(import)
                    msg <- paste(
                        "Auto-load failed for requested block import:",
                        import
                    )
                    stop(msg)
                } else {
                    message("Auto-load successful!")
                }
            }
        }
    }

    # Validate imports availability after dependencies are resolved
    # Refresh stack in case dependencies were auto-loaded
    if (!is.null(imports) && length(imports) > 0) {
        pipe <- get("module_", envir = .GlobalEnv)
        stack <- pipe$get_stack() # Refresh stack after auto-loading
        pipe$validate_imports_availability(imports, env, stack, block_id)
    }

    tryCatch(
        withCallingHandlers(
            {
                .pre_global_namespace <- take_snapshot()

                # eval(expr, envir = env)
                populate_env(
                    expr = expr,
                    env = env,
                    export = export,
                    deport = deport
                )

                .post_global_namespace <- take_snapshot()

                pipe_assets <- compare_snapshots(
                    .before = .pre_global_namespace,
                    .after = .post_global_namespace,
                    block_id = block_id,
                    expr = expr
                )

                pretty_print_table(pipe_assets)
            },
            warning = function(w) {
                msg <- gsub(pattern = '"', replacement = "'", w$message)
                call_trace <- gsub(
                    pattern = '"',
                    replacement = "'",
                    deparse1(w$call, collapse = "")
                )
                message(paste0(
                    .id,
                    ", Warning: {",
                    msg,
                    "}, Trace: {",
                    call_trace,
                    "}"
                ))
                invokeRestart("muffleWarning")
            }
        ),
        error = function(e) {
            msg <- gsub(pattern = '"', replacement = "'", e$message)
            if (grepl(pattern = "locked binding", x = msg)) {
                var <- gsub(
                    pattern = ".*?'(.*?)'.*",
                    replacement = "\\1",
                    x = msg
                )
                message(paste0(
                    "Variable '",
                    var,
                    "' is already locked to an overwrite binding."
                ))
            } else {
                call_trace <- gsub(
                    pattern = '"',
                    replacement = "'",
                    deparse1(e$call, collapse = "")
                )
                parse_error(.id, msg, call_trace, on_error)
            }
        }
    )
}

# Function to create a checksum using serialize and sum (rudimentary checksum)
create_checksum <- function(obj) {
    raw_obj <- serialize(obj, NULL)
    return(sum(as.integer(raw_obj))) # Sum of serialized byte stream as a simple checksum
}

# Function to take a snapshot of the global environment using checksums
take_snapshot <- function() {
    snapshot <- ls(envir = .GlobalEnv)
    obj_checksums <- sapply(
        snapshot,
        function(obj) {
            list(
                obj = obj,
                checksum = create_checksum(get(obj, envir = .GlobalEnv))
            )
        },
        simplify = FALSE
    )
    return(obj_checksums)
}

# Function to compare two snapshots based on checksums
compare_snapshots <- function(.before, .after, block_id, expr) {
    # Fetch pipe
    pipe <- get("module_", envir = .GlobalEnv)

    # Inittialize namespace
    namespace <- pipe$get_namespace(block_id)

    # Check namespace emptiness, reset to default expr if empty -
    # usually indicating improper initialization
    if (check_empty(namespace)) {
        expr_trace <- trace_expr(expr)
        namespace <- expr_trace$variables
    }

    # Find objects that were added or removed
    added <- setdiff(names(.after), names(.before))

    # Find objects that have been changed (by comparing checksums)
    changed <- sapply(intersect(names(.before), names(.after)), function(obj) {
        .before[[obj]]$checksum != .after[[obj]]$checksum
    })
    changed <- names(changed[changed == TRUE])
    changed <- setdiff(namespace, changed)

    # Find objects that were added or removed
    removed <- setdiff(namespace, names(.after))

    # Rebuild namespace
    if (length(added) > 0) {
        namespace <- c(added, namespace)
    }
    if (length(removed) > 0) {
        namespace <- namespace[-match(removed, namespace)]
    }

    pipe$update_namespace(block_id, unique(namespace))

    vec_list <- list(
        added = added,
        removed = removed,
        changed = changed,
        active = setdiff(namespace, c(added, removed))
    )

    # Find the maximum length
    max_len <- max(lengths(vec_list))

    # Pad each vector with NA to match the maximum length
    vec_list <- lapply(vec_list, function(x) c(x, rep(NA, max_len - length(x))))

    as.data.frame(vec_list)
}

# Helper function to format each cell with padding and apply colors
format_cell <- function(cell, width, style = NULL) {
    formatted_cell <- sprintf(paste0("%-", width, "s"), cell)
    if (!is.null(style)) {
        formatted_cell <- style(formatted_cell)
    }

    return(formatted_cell)
}

#' @title pretty_print_table
#' @description Pretty print a data frame in table format with aligned columns and color palette
#' @param data a valid R object, e.g. data.frame
#' @importFrom cli cli_alert_info cli_text cli_verbatim make_ansi_style col_yellow
#' @export pretty_print_table
pretty_print_table <- function(data) {
    if (is.data.frame(data) && nrow(data) > 0) {
        cat("\n\t")
        cli_alert_info("[Module Summary]")

        # Get the column names and values as strings
        col_names <- names(data)
        col_values <- apply(data, 2, as.character)

        if (is.vector(col_values)) {
            col_values <- t(as.matrix(col_values)) # Convert vector to matrix with 1 row
        }

        # Calculate the maximum width of each column (either from the column name or the data)
        col_widths <- sapply(seq_along(col_names), function(i) {
            max(nchar(c(col_names[i], col_values[, i])), na.rm = TRUE)
        })

        # Define background and text color styles
        header_text_col <- make_ansi_style("#689d6a", bg = FALSE)
        body_text_col <- make_ansi_style("#b16286", bg = FALSE)

        # Apply colors to the header
        header <- col_yellow(paste0(
            "| ",
            paste(
                mapply(
                    format_cell,
                    col_names,
                    col_widths,
                    MoreArgs = list(style = header_text_col)
                ),
                collapse = " | "
            ),
            " |"
        ))
        separator <- col_yellow(paste0(
            "|-",
            paste(
                mapply(
                    function(width) {
                        col_yellow(paste(rep("-", width), collapse = ""))
                    },
                    col_widths
                ),
                collapse = "-+-"
            ),
            "-|"
        ))

        # Print table top border and header
        cat("\t")
        cli_verbatim(col_yellow(paste0(
            "+-",
            paste(
                mapply(
                    function(width) {
                        col_yellow(paste(rep("-", width), collapse = ""))
                    },
                    col_widths
                ),
                collapse = "-+-"
            ),
            "-+"
        )))
        cat("\t")
        cli_verbatim(header)
        cat("\t")
        cli_verbatim(separator)

        # Print each row with alternating background colors
        row_styles <- list(body_text_col)

        for (i in seq_len(nrow(data))) {
            style <- row_styles[[i %% length(row_styles) + 1]] # Alternating row colors
            row <- col_yellow(paste0(
                "| ",
                paste(
                    mapply(
                        format_cell,
                        col_values[i, ],
                        col_widths,
                        MoreArgs = list(style = style)
                    ),
                    collapse = " | "
                ),
                " |"
            ))
            cat("\t")
            cli_verbatim(row)
        }

        # Print bottom border
        cat("\t")
        cli_text(col_yellow(paste0(
            "+-",
            paste(
                mapply(
                    function(width) {
                        col_yellow(paste(rep("-", width), collapse = ""))
                    },
                    col_widths
                ),
                collapse = "-+-"
            ),
            "-+"
        )))
    }
}

# Function to extract variable and function declarations
trace_expr <- function(expr) {
    results <- list(variables = character(), functions = character())

    walk_expr <- function(e, results) {
        if (is.call(e)) {
            op <- as.character(e[[1]])

            if (op %in% c("<-", "=", "->", "assign")) {
                if (op == "assign") {
                    var_name <- as.character(e[[2]])
                    value <- e[[3]]
                } else {
                    var_name <- if (op == "->") {
                        as.character(e[[3]])
                    } else {
                        as.character(e[[2]])
                    }
                    value <- if (op == "->") e[[2]] else e[[3]]
                }

                if (is.call(value) && value[[1]] == as.name("function")) {
                    results$functions <- c(results$functions, var_name)
                } else {
                    results$variables <- c(results$variables, var_name)
                }
            }

            # Only recurse if the call has arguments
            if (length(e) > 1) {
                for (i in seq_along(e)) {
                    if (i > 1) {
                        # Skip operator itself
                        results <- walk_expr(e[[i]], results)
                    }
                }
            }
        } else if (is.expression(e)) {
            for (sub_expr in e) {
                results <- walk_expr(sub_expr, results)
            }
        }

        return(results)
    }

    walk_expr(expr, results)
}

parse_error <- function(.id, msg, call_trace, on_error) {
    offender <- sub(".*'(.*)'.*", "\\1", msg)
    # Check for specific common errors and provide custom messages
    if (grepl("unexpected", msg)) {
        message <- paste(
            "It looks like there's an issue with your syntax ->",
            offender
        )
    } else if (grepl("non-numeric argument", msg)) {
        message <- paste(
            "A numeric value was expected, but a non-numeric argument was provided ->",
            offender
        )
    } else if (grepl("non-conformable arguments", msg)) {
        message <- paste(
            "The dimensions of the objects don't match for this operation ->",
            offender
        )
    } else if (grepl("object .* not found", msg)) {
        message <- paste(
            "One or more variables are missing or undefined ->",
            offender
        )
    } else if (grepl("subscript out of bounds", msg)) {
        message <- paste("An index is out of range ->", offender)
    } else if (grepl("argument .* is missing", msg)) {
        message <- paste(
            "A required argument is missing or not provided ->",
            offender
        )
    } else if (grepl("cannot open the connection", msg)) {
        message <- paste(
            "There was an issue opening a file or connection ->",
            offender
        )
    } else if (grepl("no package|package .* not found", msg)) {
        message <- paste("A required package is missing ->", offender)
    } else if (grepl("object.*not found|argument.*is missing", msg, ignore.case = TRUE)) {
        # Extract the missing object or argument name
        message <- paste("Missing Variable/Input/Name ->", offender)
    } else if (
        grepl(
            "non-numeric argument to binary operator|undefined columns",
            msg,
            ignore.case = TRUE
        )
    ) {
        # Identify mathematical or logical error specifics
        message <- paste("Mathematical/Logical Operator Error ->", offender)
    } else if (grepl("cannot coerce|not compatible", msg, ignore.case = TRUE)) {
        # Identify type incompatibility
        message <- paste("Incompatible type for item ->", offender)
    } else if (
        grepl(
            "subscript out of bounds|non-conformable arguments",
            msg,
            ignore.case = TRUE
        )
    ) {
        # Identify non-conformable items in matrix operations
        message <- paste("Dimensional Error ->", offender)
    } else {
        # Default to the original error message if no match is found
        message <- paste(msg)
    }

    on_error <- paste(
        sapply(names(on_error), function(name) {
            value <- on_error[[name]]
            # Add quotes for character values
            if (is.character(value)) {
                paste0(name, ": ", value)
            } else {
                paste0(name, ": ", value)
            }
        }),
        collapse = ", "
    )

    stop(paste0(
        .id,
        ", Message: {",
        on_error,
        "}, Error: {",
        message,
        "}, Trace: {",
        call_trace,
        "}"
    ))
}

populate_env <- function(expr, env, export = NULL, deport = NULL) {
    temp_env <- new.env(parent = env) # Create a temporary environment

    eval(expr, envir = temp_env) # Evaluate the expression in temp_env

    all_vars <- ls(temp_env) # Get all created variables

    # Determine final variables to copy
    if (!is.null(export)) {
        final_vars <- intersect(all_vars, export)
    } else {
        final_vars <- all_vars # If no export filter, consider all
    }

    if (!is.null(deport)) {
        final_vars <- setdiff(final_vars, deport) # Remove deport variables
    }

    # Copy selected variables to the target environment
    for (name in final_vars) {
        assign(name, get(name, envir = temp_env), envir = env)
    }
}
