test_that("%>>% warns when module_ instance doesn't exist", {
    # Ensure no module_ exists
    if (exists("module_", envir = .GlobalEnv)) {
        piper.purge(.env = .GlobalEnv)
    }

    # Using %>>% without module_ should warn and then error
    # because error_guard tries to access module_ from .GlobalEnv
    # Suppress all output including messages from error_guard
    expect_warning(
        {
            captured <- capture.output(
                expect_error(
                    suppressMessages({
                        {
                            id = "test_module"
                            description = "Test"
                            depends = {}
                            onError = {}
                        } %>>%
                            {
                                x <- 1
                            }
                    }),
                    regexp = "module_|One or more variables"
                ),
                type = "message"
            )
        },
        "Running %>>% without any pipe definitions"
    )

    # Clean up any created variables
    if (exists("test_module", envir = .GlobalEnv)) {
        rm("test_module", envir = .GlobalEnv)
    }
})

test_that("%>>% works correctly with module_ instance", {
    piper.new(.env = .GlobalEnv)

    # Use module_.push which internally uses %>>%
    # This properly registers the module
    suppressMessages({
        module_.push(
            .this = {
                id = "pipe_test"
                description = "Pipe test"
                depends = {}
                onError = {}
            },
            {
                result <- 42
            }
        )
    })

    # Module should be registered
    pipe <- get("module_", envir = .GlobalEnv)
    expect_true("pipe_test" %in% names(pipe$imports))

    piper.purge(.env = .GlobalEnv)
})

test_that("%>>% uses global environment when module_ doesn't exist", {
    # Ensure no module_ exists
    if (exists("module_", envir = .GlobalEnv)) {
        piper.purge(.env = .GlobalEnv)
    }

    # %>>% should warn and then error when module_ doesn't exist
    # because error_guard tries to access module_ from .GlobalEnv
    # Suppress all output including messages from error_guard
    expect_warning(
        {
            captured <- capture.output(
                expect_error(
                    suppressMessages({
                        {
                            id = "env_test"
                            description = "Environment test"
                            depends = {}
                            onError = {}
                        } %>>%
                            {
                                x <- 1
                            }
                    }),
                    regexp = "module_|One or more variables"
                ),
                type = "message"
            )
        },
        "Running %>>% without any pipe definitions"
    )

    # Clean up
    if (exists("env_test", envir = .GlobalEnv)) {
        rm("env_test", envir = .GlobalEnv)
    }
})

test_that("%>>% uses module environment when module_ exists", {
    piper.new(.env = .GlobalEnv)

    # Set a custom environment with a variable
    custom_env <- new.env()
    assign("custom_var", 200, envir = custom_env)

    pipe <- get("module_", envir = .GlobalEnv)
    pipe$set_env(custom_env)

    # Use module_.push which internally uses %>>%
    # This properly registers the module
    suppressMessages({
        module_.push(
            .this = {
                id = "custom_env_test"
                description = "Custom env test"
                depends = {}
                onError = {}
            },
            {
                result <- custom_var + 1
            }
        )
    })

    # Verify the module was registered (pushed)
    expect_true("custom_env_test" %in% names(pipe$imports))

    # Now compute it - should use the custom environment
    suppressMessages({
        module_.compute("custom_env_test")
    })

    # Verify result is in the custom environment
    expect_true(exists("result", envir = custom_env))
    expect_equal(get("result", envir = custom_env), 201)

    piper.purge(.env = .GlobalEnv)
})

test_that("%>>% handles complex expressions correctly", {
    piper.new(.env = .GlobalEnv)

    # First create the dependency module
    suppressMessages({
        module_.push(
            .this = {
                id = "other_module"
                description = "Other module"
                depends = {}
                export = list("other_var")
                onError = {}
            },
            {
                other_var <- 10
            }
        )
    })

    # Test with complex metadata using module_.push (which uses %>>% internally)
    suppressMessages({
        module_.push(
            .this = {
                id = "complex_test"
                description = "Complex expression test"
                depends = list("other_module")
                imports = list(
                    other_module = list("other_var")
                )
                export = list("result")
                deport = list("temp")
                onError = list(message = "Error")
            },
            {
                temp <- 1
                result <- other_var + temp
            }
        )
    })

    # Module should be registered with all metadata
    pipe <- get("module_", envir = .GlobalEnv)
    expect_true("complex_test" %in% names(pipe$imports))

    piper.purge(.env = .GlobalEnv)
})

test_that("%>>% creates module environment in global namespace", {
    piper.new(.env = .GlobalEnv)

    suppressMessages({
        result <- {
            id = "namespace_test"
            description = "Namespace test"
            depends = {}
            onError = {}
        } %>>%
            {
                x <- 1
            }
    })

    # The module ID should be assigned to .GlobalEnv as an environment
    expect_true(exists("namespace_test", envir = .GlobalEnv))
    expect_true(is.environment(get("namespace_test", envir = .GlobalEnv)))

    # Clean up
    if (exists("namespace_test", envir = .GlobalEnv)) {
        rm("namespace_test", envir = .GlobalEnv)
    }
    piper.purge(.env = .GlobalEnv)
})
