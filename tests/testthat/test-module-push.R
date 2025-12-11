test_that("module_.push adds a module to the pipeline", {
    piper.new(.env = .GlobalEnv)

    module_.push(
        .this = {
            id = "test_module"
            description = "Test module"
            depends = {}
            onError = {}
        },
        {
            x <- 1
        }
    )

    pipe <- get("module_", envir = .GlobalEnv)
    expect_true("test_module" %in% names(pipe$imports))

    piper.purge(.env = .GlobalEnv)
})

test_that("module_.push validates imports structure", {
    piper.new(.env = .GlobalEnv)

    # Invalid import source (not in depends and not 'global')
    expect_error(
        module_.push(
            .this = {
                id = "test_module"
                description = "Test module"
                depends = list("other_module")
                imports = list(
                    invalid_module = list("x")
                )
                onError = {}
            },
            {
                y <- x
            }
        ),
        "Invalid import sources"
    )

    piper.purge(.env = .GlobalEnv)
})

test_that("module_.push accepts valid imports structure", {
    piper.new(.env = .GlobalEnv)

    # Valid import from dependency
    module_.push(
        .this = {
            id = "test_module"
            description = "Test module"
            depends = list("other_module")
            imports = list(
                other_module = list("x")
            )
            onError = {}
        },
        {
            y <- x + 1
        }
    )

    pipe <- get("module_", envir = .GlobalEnv)
    expect_true("test_module" %in% names(pipe$imports))

    piper.purge(.env = .GlobalEnv)
})

test_that("module_.push accepts global imports", {
    piper.new(.env = .GlobalEnv)

    module_.push(
        .this = {
            id = "test_module"
            description = "Test module"
            depends = {}
            imports = list(
                global = list("x")
            )
            onError = {}
        },
        {
            y <- x + 1
        }
    )

    pipe <- get("module_", envir = .GlobalEnv)
    expect_true("test_module" %in% names(pipe$imports))

    piper.purge(.env = .GlobalEnv)
})

test_that("module_.push handles export and deport", {
    piper.new(.env = .GlobalEnv)

    module_.push(
        .this = {
            id = "test_module"
            description = "Test module"
            depends = {}
            export = list("y")
            deport = list("temp")
            onError = {}
        },
        {
            temp <- 1
            y <- 2
            z <- 3
        }
    )

    pipe <- get("module_", envir = .GlobalEnv)
    expect_true("test_module" %in% names(pipe$imports))

    piper.purge(.env = .GlobalEnv)
})

test_that("export filters variables during execution", {
    piper.new(.env = .GlobalEnv)

    module_.push(
        .this = {
            id = "export_test"
            description = "Test export filtering"
            depends = {}
            export = list("result", "value")
            onError = {}
        },
        {
            result <- 42
            value <- 100
            temp <- 999
            intermediate <- "should not export"
        }
    )

    suppressMessages({
        module_.compute("export_test")
    })

    env <- module_.env()
    # Exported variables should exist
    expect_true(exists("result", envir = env))
    expect_true(exists("value", envir = env))
    expect_equal(get("result", envir = env), 42)
    expect_equal(get("value", envir = env), 100)

    # Non-exported variables should not exist
    expect_false(exists("temp", envir = env))
    expect_false(exists("intermediate", envir = env))

    piper.purge(.env = .GlobalEnv)
})

test_that("deport excludes variables from exports", {
    piper.new(.env = .GlobalEnv)

    module_.push(
        .this = {
            id = "deport_test"
            description = "Test deport filtering"
            depends = {}
            deport = list("temp", "intermediate")
            onError = {}
        },
        {
            result <- 42
            value <- 100
            temp <- 999
            intermediate <- "should be deported"
        }
    )

    suppressMessages({
        module_.compute("deport_test")
    })

    env <- module_.env()
    # Variables not in deport should exist
    expect_true(exists("result", envir = env))
    expect_true(exists("value", envir = env))
    expect_equal(get("result", envir = env), 42)
    expect_equal(get("value", envir = env), 100)

    # Deported variables should not exist
    expect_false(exists("temp", envir = env))
    expect_false(exists("intermediate", envir = env))

    piper.purge(.env = .GlobalEnv)
})

test_that("export and deport work together", {
    piper.new(.env = .GlobalEnv)

    module_.push(
        .this = {
            id = "export_deport_test"
            description = "Test export and deport together"
            depends = {}
            export = list("result", "value", "temp")
            deport = list("temp", "secret")
            onError = {}
        },
        {
            result <- 42
            value <- 100
            temp <- 999
            secret <- "should be deported"
            other <- "should not be exported"
        }
    )

    suppressMessages({
        module_.compute("export_deport_test")
    })

    env <- module_.env()
    # Variables in export but not deport should exist
    expect_true(exists("result", envir = env))
    expect_true(exists("value", envir = env))
    expect_equal(get("result", envir = env), 42)
    expect_equal(get("value", envir = env), 100)

    # Variables in deport should not exist (even if in export)
    expect_false(exists("temp", envir = env))
    expect_false(exists("secret", envir = env))

    # Variables not in export should not exist
    expect_false(exists("other", envir = env))

    piper.purge(.env = .GlobalEnv)
})
