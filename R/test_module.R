#' @title test_module
#' @description Test a module independently by providing required imports and validating exports
#' @param module_id the ID of the module to test
#' @param imports a list of imports to provide, with structure matching module imports declaration
#' @param expected_exports a named list of expected export values to validate
#' @param test_env optional test environment, DEFAULT: new.env()
#' @param .debug should debug mode be triggered, DEFAULT: FALSE
#' @importFrom testthat expect_identical
#' @export test_module
test_module <- function(
    module_id,
    imports = list(),
    expected_exports = list(),
    test_env = new.env(),
    .debug = FALSE
) {
    # Get the piper instance
    if (!exists("module_", envir = .GlobalEnv)) {
        stop(
            "module_ instance not found. Please initialize with piper.new() first."
        )
    }

    pipe <- get("module_", envir = .GlobalEnv)

    # Check that module exists
    if (!(module_id %in% names(pipe$imports))) {
        stop(paste0("Module '", module_id, "' not found in pipeline"))
    }

    # Get module metadata to understand its imports and exports
    local_env <- new.env()
    module_expr <- pipe$imports[[module_id]]
    args <- pipe$memory_map(module_expr, local_env)

    module_imports <- args$imports
    module_export <- args$export
    module_depends <- args$depends

    # Track which global variables we set up for cleanup
    global_vars_setup <- character(0)

    # Set up imports in test environment
    # First, handle global imports
    # imports structure: list(source_module_id = list(var_name = value, ...), global = list(var_name = value, ...))
    if (!is.null(module_imports) && "global" %in% names(module_imports)) {
        global_vars <- module_imports[["global"]]
        for (var_name in global_vars) {
            if (
                "global" %in%
                    names(imports) &&
                    is.list(imports[["global"]]) &&
                    var_name %in% names(imports[["global"]])
            ) {
                assign(
                    var_name,
                    imports[["global"]][[var_name]],
                    envir = .GlobalEnv
                )
                global_vars_setup <- c(global_vars_setup, var_name)
            } else {
                # Clean up any globals we already set up before erroring
                for (gvar in global_vars_setup) {
                    if (exists(gvar, envir = .GlobalEnv)) {
                        rm(list = gvar, envir = .GlobalEnv)
                    }
                }
                stop(paste0(
                    "Missing required global import: '",
                    var_name,
                    "'. Provide it in imports as: imports = list(global = list(",
                    var_name,
                    " = <value>))"
                ))
            }
        }
    }

    # Handle module dependency imports
    # imports structure: list(source_module_id = list(var_name = value, ...), global = list(var_name = value, ...))
    if (!is.null(module_imports)) {
        for (source in names(module_imports)) {
            if (source != "global") {
                var_names <- module_imports[[source]]
                for (var_name in var_names) {
                    # Check if provided in imports parameter
                    if (
                        source %in%
                            names(imports) &&
                            is.list(imports[[source]]) &&
                            var_name %in% names(imports[[source]])
                    ) {
                        assign(
                            var_name,
                            imports[[source]][[var_name]],
                            envir = test_env
                        )
                    } else {
                        stop(paste0(
                            "Missing required import '",
                            var_name,
                            "' from module '",
                            source,
                            "'. Provide it in imports as: imports = list(",
                            source,
                            " = list(",
                            var_name,
                            " = <value>))"
                        ))
                    }
                }
            }
        }
    }

    # Set the test environment as the module execution environment
    pipe$set_env(test_env)

    # Save original stack to restore later
    original_stack <- pipe$get_stack()

    # Pre-populate stack with dependency modules to prevent auto-execution
    # This tells error_guard that dependencies have already been executed
    # so it won't try to auto-load them
    if (!is.null(module_depends) && length(module_depends) > 0) {
        for (dep in module_depends) {
            if (!(dep %in% pipe$get_stack())) {
                pipe$stack <- append(pipe$stack, dep)
            }
        }
    }

    # Execute the module (suppress messages for cleaner test output)
    tryCatch(
        {
            suppressMessages({
                pipe$compute(module_id, .debug = .debug)
            })

            # Validate exports
            test_results <- list()
            if (length(expected_exports) > 0) {
                for (export_name in names(expected_exports)) {
                    if (exists(export_name, envir = test_env)) {
                        actual_value <- get(export_name, envir = test_env)
                        expected_value <- expected_exports[[export_name]]
                        test_results[[export_name]] <- tryCatch(
                            {
                                testthat::expect_identical(
                                    actual_value,
                                    expected_value
                                )
                                list(
                                    status = "PASS",
                                    message = paste0(
                                        "Export '",
                                        export_name,
                                        "' matches expected value"
                                    )
                                )
                            },
                            error = function(e) {
                                list(status = "FAIL", message = e$message)
                            }
                        )
                    } else {
                        test_results[[export_name]] <- list(
                            status = "FAIL",
                            message = paste0(
                                "Export '",
                                export_name,
                                "' not found in module output"
                            )
                        )
                    }
                }
            } else if (!is.null(module_export) && length(module_export) > 0) {
                # If no expected_exports provided but module has exports, just check they exist
                for (export_name in module_export) {
                    if (exists(export_name, envir = test_env)) {
                        test_results[[export_name]] <- list(
                            status = "PASS",
                            message = paste0(
                                "Export '",
                                export_name,
                                "' exists"
                            )
                        )
                    } else {
                        test_results[[export_name]] <- list(
                            status = "FAIL",
                            message = paste0(
                                "Export '",
                                export_name,
                                "' not found in module output"
                            )
                        )
                    }
                }
            }

            # Clean up: restore original stack and remove test module from stack
            pipe$stack <- original_stack
            pipe$compute_stack(module_id)

            # Clean up global imports
            for (var_name in global_vars_setup) {
                if (exists(var_name, envir = .GlobalEnv)) {
                    rm(list = var_name, envir = .GlobalEnv)
                }
            }

            # Return test results
            return(list(
                module_id = module_id,
                status = if (all(sapply(test_results, function(x) x$status == "PASS"))) {
                    "PASS"
                } else {
                    "FAIL"
                },
                results = test_results
            ))
        },
        error = function(e) {
            # Clean up: restore original stack
            pipe$stack <- original_stack

            # Clean up global imports on error
            for (var_name in global_vars_setup) {
                if (exists(var_name, envir = .GlobalEnv)) {
                    rm(list = var_name, envir = .GlobalEnv)
                }
            }
            stop(paste0("Error testing module '", module_id, "': ", e$message))
        }
    )
}
