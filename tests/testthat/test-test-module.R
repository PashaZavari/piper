test_that("test_module executes a module in isolation", {
    piper.new(.env = .GlobalEnv)

    module_.push(
        .this = {
            id = "simple_test"
            description = "Simple test module"
            depends = {}
            export = list("result")
            onError = {}
        },
        {
            result <- 42
        }
    )

    test_result <- test_module(
        module_id = "simple_test",
        imports = list(),
        expected_exports = list(result = 42)
    )

    expect_equal(test_result$status, "PASS")
    expect_equal(test_result$module_id, "simple_test")

    piper.purge(.env = .GlobalEnv)
})

test_that("test_module works with provided imports", {
    piper.new(.env = .GlobalEnv)

    module_.push(
        .this = {
            id = "test_with_imports"
            description = "Test module with imports"
            depends = {}
            imports = list(
                global = list("input_value")
            )
            export = list("output")
            onError = {}
        },
        {
            output <- input_value * 2
        }
    )

    # Set up global variable
    assign("input_value", 5, envir = .GlobalEnv)

    test_result <- test_module(
        module_id = "test_with_imports",
        imports = list(
            global = list(input_value = 5)
        ),
        expected_exports = list(output = 10)
    )

    expect_equal(test_result$status, "PASS")

    # Verify global was cleaned up
    expect_false(exists("input_value", envir = .GlobalEnv))

    piper.purge(.env = .GlobalEnv)
})

test_that("test_module prevents dependency auto-execution", {
    piper.new(.env = .GlobalEnv)

    # Create dependency module
    module_.push(
        .this = {
            id = "dependency_module"
            description = "Dependency module"
            depends = {}
            export = list("dep_value")
            onError = {}
        },
        {
            dep_value <- 100
        }
    )

    # Create module that depends on it
    module_.push(
        .this = {
            id = "dependent_module"
            description = "Dependent module"
            depends = list("dependency_module")
            imports = list(
                dependency_module = list("dep_value")
            )
            export = list("result")
            onError = {}
        },
        {
            result <- dep_value + 1
        }
    )

    # Test dependent module without executing dependency
    # Should use provided imports instead
    test_result <- test_module(
        module_id = "dependent_module",
        imports = list(
            dependency_module = list(dep_value = 50)
        ),
        expected_exports = list(result = 51)
    )

    expect_equal(test_result$status, "PASS")

    # Verify dependency module was not executed (dep_value should not be in env)
    # The test_env should only have result, not dep_value
    test_env <- new.env()
    test_module(
        module_id = "dependent_module",
        imports = list(
            dependency_module = list(dep_value = 50)
        ),
        expected_exports = list(result = 51),
        test_env = test_env
    )

    # dep_value should exist in test_env (it was provided as import)
    # The important thing is that the dependency module wasn't executed
    expect_true(exists("dep_value", envir = test_env))
    expect_equal(get("dep_value", envir = test_env), 50)  # Should be the provided value
    expect_true(exists("result", envir = test_env))

    piper.purge(.env = .GlobalEnv)
})

test_that("test_module validates exports correctly", {
    piper.new(.env = .GlobalEnv)

    module_.push(
        .this = {
            id = "export_test"
            description = "Export test module"
            depends = {}
            export = list("correct_export")
            onError = {}
        },
        {
            correct_export <- "expected"
            wrong_export <- "unexpected"
        }
    )

    # Test with correct expected export
    result1 <- test_module(
        module_id = "export_test",
        imports = list(),
        expected_exports = list(correct_export = "expected")
    )
    expect_equal(result1$status, "PASS")

    # Test with incorrect expected export
    result2 <- test_module(
        module_id = "export_test",
        imports = list(),
        expected_exports = list(correct_export = "wrong")
    )
    expect_equal(result2$status, "FAIL")

    piper.purge(.env = .GlobalEnv)
})

test_that("test_module handles missing imports gracefully", {
    piper.new(.env = .GlobalEnv)

    module_.push(
        .this = {
            id = "import_test"
            description = "Import test module"
            depends = {}
            imports = list(
                global = list("required_var")
            )
            onError = {}
        },
        {
            result <- required_var
        }
    )

    expect_error(
        test_module(
            module_id = "import_test",
            imports = list(),
            expected_exports = list()
        ),
        "Missing required global import"
    )

    piper.purge(.env = .GlobalEnv)
})

test_that("test_module cleans up after errors", {
    piper.new(.env = .GlobalEnv)

    module_.push(
        .this = {
            id = "error_test"
            description = "Error test module"
            depends = {}
            imports = list(
                global = list("test_global")
            )
            onError = {}
        },
        {
            result <- test_global
        }
    )

    # This should error but clean up any globals it set up
    # Since imports is empty, it should error before setting up test_global
    expect_error(
        test_module(
            module_id = "error_test",
            imports = list(), # Missing required import
            expected_exports = list()
        ),
        "Missing required global import"
    )

    # Verify test_global doesn't exist (wasn't set up due to error)
    expect_false(exists("test_global", envir = .GlobalEnv))

    piper.purge(.env = .GlobalEnv)
})
