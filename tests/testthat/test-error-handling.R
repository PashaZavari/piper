test_that("onError custom messages are included in error output", {
    piper.new(.env = .GlobalEnv)

    ...push(
        .this = {
            id = "error_test_module"
            description = "Test error handling"
            depends = {}
            onError = list(
                message = "Custom error message",
                action = "stop"
            )
        },
        {
            # This will cause an error - using undefined variable
            result <- undefined_variable + 1
        }
    )

    # Execute and check error contains onError message
    expect_error(
        suppressMessages({
            ...pipe("error_test_module")
        }),
        "Custom error message"
    )

    piper.purge(.env = .GlobalEnv)
})

test_that("onError with multiple fields formats correctly", {
    piper.new(.env = .GlobalEnv)

    ...push(
        .this = {
            id = "multi_error_test"
            description = "Test multiple onError fields"
            depends = {}
            onError = list(
                message = "Error occurred",
                action = "stop",
                context = "test context"
            )
        },
        {
            # Cause an error
            x <- non_existent_var
        }
    )

    # Error should include all onError fields
    error_msg <- tryCatch(
        {
            suppressMessages({
                ...pipe("multi_error_test")
            })
            NULL
        },
        error = function(e) e$message
    )

    expect_true(grepl("Error occurred", error_msg))
    expect_true(grepl("action: stop", error_msg))
    expect_true(grepl("context: test context", error_msg))

    piper.purge(.env = .GlobalEnv)
})

test_that("onError with empty list still works", {
    piper.new(.env = .GlobalEnv)

    ...push(
        .this = {
            id = "empty_error_test"
            description = "Test empty onError"
            depends = {}
            onError = list()
        },
        {
            # Cause an error
            y <- missing_var
        }
    )

    # Should still error, just without custom onError content
    expect_error(
        suppressMessages({
            ...pipe("empty_error_test")
        })
    )

    piper.purge(.env = .GlobalEnv)
})

test_that("duplicate module IDs trigger warning", {
    piper.new(.env = .GlobalEnv)

    # Push first module
    ...push(
        .this = {
            id = "duplicate_test"
            description = "First module"
            depends = {}
            onError = {}
        },
        {
            x <- 1
        }
    )

    # Push module with same ID - should warn
    expect_warning(
        ...push(
            .this = {
                id = "duplicate_test"
                description = "Second module (duplicate)"
                depends = {}
                onError = {}
            },
            {
                y <- 2
            }
        ),
        "Multiple imports found"
    )

    # Verify the module was replaced (should only have one)
    pipe <- get("..", envir = .GlobalEnv)
    expect_true("duplicate_test" %in% names(pipe$imports))
    # Should only appear once
    expect_equal(sum(names(pipe$imports) == "duplicate_test"), 1)

    piper.purge(.env = .GlobalEnv)
})

test_that("duplicate module ID replaces previous module", {
    piper.new(.env = .GlobalEnv)

    # Push first module
    ...push(
        .this = {
            id = "replace_test"
            description = "Original"
            depends = {}
            onError = {}
        },
        {
            original_value <- 100
        }
    )

    # Replace with new module
    suppressWarnings({
        ...push(
            .this = {
                id = "replace_test"
                description = "Replacement"
                depends = {}
                onError = {}
            },
            {
                replacement_value <- 200
            }
        )
    })

    # Execute and verify replacement module runs
    suppressMessages({
        ...pipe("replace_test")
    })

    env <- ...env()
    # Should have replacement value, not original
    expect_true(exists("replacement_value", envir = env))
    expect_false(exists("original_value", envir = env))
    expect_equal(get("replacement_value", envir = env), 200)

    piper.purge(.env = .GlobalEnv)
})
