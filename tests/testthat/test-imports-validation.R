test_that("validate_imports_structure accepts valid structure", {
    # Clean up any existing instance
    if (exists("..", envir = .GlobalEnv)) {
        piper.purge(.env = .GlobalEnv)
    }

    piper.new(.env = .GlobalEnv)
    expect_true(exists("..", envir = .GlobalEnv))
    pipe <- get("..", envir = .GlobalEnv)

    # Valid: imports from dependencies
    expect_no_error(
        pipe$validate_imports_structure(
            imports = list(
                module_a = list("var1", "var2"),
                global = list("var3")
            ),
            depends = list("module_a"),
            block_id = "test"
        )
    )

    # Valid: only global imports
    expect_no_error(
        pipe$validate_imports_structure(
            imports = list(
                global = list("var1", "var2")
            ),
            depends = list(),
            block_id = "test"
        )
    )

    # Valid: empty imports
    expect_no_error(
        pipe$validate_imports_structure(
            imports = NULL,
            depends = list(),
            block_id = "test"
        )
    )

    piper.purge(.env = .GlobalEnv)
})

test_that("validate_imports_structure rejects invalid sources", {
    # Clean up any existing instance
    if (exists("..", envir = .GlobalEnv)) {
        piper.purge(.env = .GlobalEnv)
    }

    piper.new(.env = .GlobalEnv)
    expect_true(exists("..", envir = .GlobalEnv))
    pipe <- get("..", envir = .GlobalEnv)

    # Invalid: source not in depends and not 'global'
    expect_error(
        pipe$validate_imports_structure(
            imports = list(
                invalid_module = list("var1")
            ),
            depends = list("module_a"),
            block_id = "test"
        ),
        "Invalid import sources"
    )

    piper.purge(.env = .GlobalEnv)
})

test_that("validate_imports_structure requires list values", {
    # Clean up any existing instance
    if (exists("..", envir = .GlobalEnv)) {
        piper.purge(.env = .GlobalEnv)
    }

    piper.new(.env = .GlobalEnv)
    expect_true(exists("..", envir = .GlobalEnv))
    pipe <- get("..", envir = .GlobalEnv)

    # Invalid: non-list value
    expect_error(
        pipe$validate_imports_structure(
            imports = list(
                module_a = "not_a_list"
            ),
            depends = list("module_a"),
            block_id = "test"
        ),
        "must be a list"
    )

    piper.purge(.env = .GlobalEnv)
})

test_that("validate_imports_availability checks variable existence", {
    # Clean up any existing instance
    if (exists("..", envir = .GlobalEnv)) {
        piper.purge(.env = .GlobalEnv)
    }

    piper.new(.env = .GlobalEnv)
    expect_true(exists("..", envir = .GlobalEnv))
    pipe <- get("..", envir = .GlobalEnv)
    test_env <- new.env()

    # Set up test environment with variables
    assign("var1", 10, envir = test_env)
    assign("var2", 20, envir = .GlobalEnv)

    # Valid: all variables exist
    expect_no_error(
        pipe$validate_imports_availability(
            imports = list(
                module_a = list("var1"),
                global = list("var2")
            ),
            env = test_env,
            stack = list("module_a"),
            block_id = "test"
        )
    )

    # Invalid: missing variable from module
    expect_error(
        pipe$validate_imports_availability(
            imports = list(
                module_a = list("missing_var")
            ),
            env = test_env,
            stack = list("module_a"),
            block_id = "test"
        ),
        "Missing required imports"
    )

    # Invalid: missing global variable
    expect_error(
        pipe$validate_imports_availability(
            imports = list(
                global = list("missing_global")
            ),
            env = test_env,
            stack = list(),
            block_id = "test"
        ),
        "Missing required imports"
    )

    # Clean up
    rm("var2", envir = .GlobalEnv)
    piper.purge(.env = .GlobalEnv)
})

test_that("validate_imports_availability checks module execution", {
    # Clean up any existing instance
    if (exists("..", envir = .GlobalEnv)) {
        piper.purge(.env = .GlobalEnv)
    }

    piper.new(.env = .GlobalEnv)
    expect_true(exists("..", envir = .GlobalEnv))
    pipe <- get("..", envir = .GlobalEnv)
    test_env <- new.env()

    # Error: module not in stack
    expect_error(
        pipe$validate_imports_availability(
            imports = list(
                module_a = list("var1")
            ),
            env = test_env,
            stack = list(), # module_a not executed
            block_id = "test"
        ),
        "has not been executed yet"
    )

    piper.purge(.env = .GlobalEnv)
})
