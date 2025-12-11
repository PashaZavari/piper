test_that("module_.compute executes a simple module", {
    piper.new(.env = .GlobalEnv)

    module_.push(
        .this = {
            id = "simple_module"
            description = "Simple test module"
            depends = {}
            onError = {}
        },
        {
            result <- 42
        }
    )

    suppressMessages({
        module_.compute("simple_module")
    })
    expect_true(exists("result", envir = module_.env()))
    expect_equal(get("result", envir = module_.env()), 42)

    piper.purge(.env = .GlobalEnv)
})

test_that("module_.compute handles dependencies", {
    piper.new(.env = .GlobalEnv)

    # First module
    module_.push(
        .this = {
            id = "module_a"
            description = "Module A"
            depends = {}
            onError = {}
        },
        {
            value_a <- 10
        }
    )

    # Second module depends on first
    module_.push(
        .this = {
            id = "module_b"
            description = "Module B"
            depends = list("module_a")
            imports = list(
                module_a = list("value_a")
            )
            onError = {}
        },
        {
            value_b <- value_a * 2
        }
    )

    # Execute in order
    suppressMessages({
        module_.compute("module_a")
        module_.compute("module_b")
    })

    expect_equal(get("value_b", envir = module_.env()), 20)

    piper.purge(.env = .GlobalEnv)
})

test_that("module_.compute auto-loads missing dependencies", {
    piper.new(.env = .GlobalEnv)

    module_.push(
        .this = {
            id = "module_a"
            description = "Module A"
            depends = {}
            onError = {}
        },
        {
            value_a <- 5
        }
    )

    module_.push(
        .this = {
            id = "module_b"
            description = "Module B"
            depends = list("module_a")
            imports = list(
                module_a = list("value_a")
            )
            onError = {}
        },
        {
            value_b <- value_a + 1
        }
    )

    # Execute only module_b - should auto-load module_a
    # Use capture.output to suppress all output while still testing for the message
    captured_output <- capture.output(
        {
            captured_messages <- capture.output(
                expect_message(module_.compute("module_b"), "Attempting auto-load"),
                type = "message"
            )
        },
        type = "output"
    )
    # Re-execute silently to get the result
    suppressMessages({
        module_.compute("module_b")
    })
    expect_equal(get("value_b", envir = module_.env()), 6)

    piper.purge(.env = .GlobalEnv)
})

test_that("module_.compute validates imports availability", {
    piper.new(.env = .GlobalEnv)

    module_.push(
        .this = {
            id = "module_a"
            description = "Module A"
            depends = {}
            onError = {}
        },
        {
            value_a <- 10
        }
    )

    module_.push(
        .this = {
            id = "module_b"
            description = "Module B"
            depends = list("module_a")
            imports = list(
                module_a = list("value_a", "missing_var")
            )
            onError = {}
        },
        {
            value_b <- value_a
        }
    )

    suppressMessages({
        module_.compute("module_a")
    })
    expect_error(
        suppressMessages({
            module_.compute("module_b")
        }),
        "Missing required imports"
    )

    piper.purge(.env = .GlobalEnv)
})

test_that("module_.compute handles global imports", {
    piper.new(.env = .GlobalEnv)

    # Set up global variable
    assign("global_x", 100, envir = .GlobalEnv)

    module_.push(
        .this = {
            id = "test_module"
            description = "Test module"
            depends = {}
            imports = list(
                global = list("global_x")
            )
            onError = {}
        },
        {
            result <- global_x + 1
        }
    )

    suppressMessages({
        module_.compute("test_module")
    })
    expect_equal(get("result", envir = module_.env()), 101)

    # Clean up
    rm("global_x", envir = .GlobalEnv)
    piper.purge(.env = .GlobalEnv)
})
