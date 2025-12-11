test_that("piper.load loads module from file", {
    # Set up test directory structure
    test_root <- tempfile("piper_test_")
    on.exit(unlink(test_root, recursive = TRUE))

    # Create directory structure: test_root/R++/modules/test_module/v1.0/
    module_dir <- file.path(test_root, "R++", "modules", "test_module", "v1.0")
    dir.create(module_dir, recursive = TRUE)

    # Create a test module file
    module_file <- file.path(module_dir, "main.r")
    writeLines(
        c(
            'module_.push(',
            '    .this = {',
            '        id = "loaded_module"',
            '        description = "Module loaded from file"',
            '        depends = {}',
            '        onError = {}',
            '    }, {',
            '        result <- 42',
            '    }',
            ')'
        ),
        module_file
    )

    # Initialize piper
    piper.new(.env = .GlobalEnv)

    # Load the module
    suppressMessages({
        piper.load(
            module = "test_module",
            from = test_root,
            header = "main.r",
            src = "R++",
            pipe = "modules",
            version = "v1.0"
        )
    })

    # Verify module was loaded
    pipe <- get("module_", envir = .GlobalEnv)
    expect_true("loaded_module" %in% names(pipe$imports))

    piper.purge(.env = .GlobalEnv)
})

test_that("piper.load uses default parameters", {
    # Set up test directory structure with defaults
    test_root <- tempfile("piper_test_")
    on.exit(unlink(test_root, recursive = TRUE))

    # Default structure: test_root/R++/modules/test_module/main.r
    module_dir <- file.path(test_root, "R++", "modules", "test_module")
    dir.create(module_dir, recursive = TRUE)

    # Create a test module file
    module_file <- file.path(module_dir, "main.r")
    writeLines(
        c(
            'module_.push(',
            '    .this = {',
            '        id = "default_module"',
            '        description = "Module with defaults"',
            '        depends = {}',
            '        onError = {}',
            '    }, {',
            '        value <- 100',
            '    }',
            ')'
        ),
        module_file
    )

    # Initialize piper
    piper.new(.env = .GlobalEnv)

    # Load with minimal parameters (uses defaults)
    suppressMessages({
        piper.load(
            module = "test_module",
            from = test_root
        )
    })

    # Verify module was loaded
    pipe <- get("module_", envir = .GlobalEnv)
    expect_true("default_module" %in% names(pipe$imports))

    piper.purge(.env = .GlobalEnv)
})

test_that("piper.load handles custom header file name", {
    # Set up test directory structure
    test_root <- tempfile("piper_test_")
    on.exit(unlink(test_root, recursive = TRUE))

    module_dir <- file.path(test_root, "R++", "modules", "custom_module", "v1.0")
    dir.create(module_dir, recursive = TRUE)

    # Create custom header file
    module_file <- file.path(module_dir, "custom.r")
    writeLines(
        c(
            'module_.push(',
            '    .this = {',
            '        id = "custom_header_module"',
            '        description = "Module with custom header"',
            '        depends = {}',
            '        onError = {}',
            '    }, {',
            '        custom_value <- 200',
            '    }',
            ')'
        ),
        module_file
    )

    # Initialize piper
    piper.new(.env = .GlobalEnv)

    # Load with custom header
    suppressMessages({
        piper.load(
            module = "custom_module",
            from = test_root,
            header = "custom.r",
            version = "v1.0"
        )
    })

    # Verify module was loaded
    pipe <- get("module_", envir = .GlobalEnv)
    expect_true("custom_header_module" %in% names(pipe$imports))

    piper.purge(.env = .GlobalEnv)
})

test_that("piper.load sets environment correctly", {
    # Set up test directory structure
    test_root <- tempfile("piper_test_")
    on.exit(unlink(test_root, recursive = TRUE))

    module_dir <- file.path(test_root, "R++", "modules", "env_test", "v1.0")
    dir.create(module_dir, recursive = TRUE)

    module_file <- file.path(module_dir, "main.r")
    writeLines(
        c(
            'module_.push(',
            '    .this = {',
            '        id = "env_module"',
            '        description = "Environment test"',
            '        depends = {}',
            '        onError = {}',
            '    }, {',
            '        env_value <- 300',
            '    }',
            ')'
        ),
        module_file
    )

    # Initialize piper
    piper.new(.env = .GlobalEnv)

    # Create custom environment
    custom_env <- new.env()

    # Load with custom environment
    suppressMessages({
        piper.load(
            module = "env_test",
            from = test_root,
            .env = custom_env,
            version = "v1.0"
        )
    })

    # Verify environment was set
    pipe <- get("module_", envir = .GlobalEnv)
    expect_identical(pipe$get_env(), custom_env)

    piper.purge(.env = .GlobalEnv)
})

test_that("piper.load errors on missing file", {
    # Initialize piper
    piper.new(.env = .GlobalEnv)

    # Try to load non-existent module
    # Suppress both messages and warnings
    expect_error(
        suppressMessages(
            suppressWarnings({
                piper.load(
                    module = "nonexistent_module",
                    from = tempdir()
                )
            })
        ),
        "cannot open"
    )

    piper.purge(.env = .GlobalEnv)
})

test_that("piper.load loads multiple modules from file", {
    # Set up test directory structure
    test_root <- tempfile("piper_test_")
    on.exit(unlink(test_root, recursive = TRUE))

    module_dir <- file.path(test_root, "R++", "modules", "multi_module", "v1.0")
    dir.create(module_dir, recursive = TRUE)

    module_file <- file.path(module_dir, "main.r")
    writeLines(
        c(
            'module_.push(',
            '    .this = {',
            '        id = "module_one"',
            '        description = "First module"',
            '        depends = {}',
            '        onError = {}',
            '    }, {',
            '        one <- 1',
            '    }',
            ')',
            '',
            'module_.push(',
            '    .this = {',
            '        id = "module_two"',
            '        description = "Second module"',
            '        depends = {}',
            '        onError = {}',
            '    }, {',
            '        two <- 2',
            '    }',
            ')'
        ),
        module_file
    )

    # Initialize piper
    piper.new(.env = .GlobalEnv)

    # Load the module file (should load both modules)
    suppressMessages({
        piper.load(
            module = "multi_module",
            from = test_root,
            version = "v1.0"
        )
    })

    # Verify both modules were loaded
    pipe <- get("module_", envir = .GlobalEnv)
    expect_true("module_one" %in% names(pipe$imports))
    expect_true("module_two" %in% names(pipe$imports))

    piper.purge(.env = .GlobalEnv)
})
