test_that("piper.make creates directory structure", {
    test_root <- tempfile("piper_test_")
    on.exit(unlink(test_root, recursive = TRUE))

    capture.output(
        {
            result <- piper.make(pipe = "test_pipe", root = test_root)
        },
        type = "output"
    )
    expect_true(dir.exists(file.path(test_root, "test_pipe")))
})

test_that("piper.module creates module directory", {
    test_root <- tempfile("piper_test_")
    on.exit(unlink(test_root, recursive = TRUE))

    capture.output(
        {
            piper.make(pipe = "test_pipe", root = test_root)
            result <- piper.module(pipe = "test_pipe", module = "test_module", parent = test_root)
        },
        type = "output"
    )
    expect_true(dir.exists(file.path(test_root, "test_pipe", "test_module")))
})

test_that("piper.brew creates version directory and header file", {
    test_root <- tempfile("piper_test_")
    on.exit(unlink(test_root, recursive = TRUE))

    capture.output(
        {
            piper.make(pipe = "test_pipe", root = test_root)
            piper.module(pipe = "test_pipe", module = "test_module", parent = test_root)
            result <- piper.brew(
                pipe = "test_pipe",
                module = "test_module",
                version = "v1.0",
                header = "main.r",
                parent = test_root
            )
        },
        type = "output"
    )

    expect_true(dir.exists(file.path(test_root, "test_pipe", "test_module", "v1.0")))
    expect_true(file.exists(file.path(test_root, "test_pipe", "test_module", "v1.0", "main.r")))
})

test_that("module_.env returns the module environment", {
    piper.new(.env = .GlobalEnv)

    env <- ...env()
    expect_true(is.environment(env))

    piper.purge(.env = .GlobalEnv)
})

test_that("map_pipeline shows module information", {
    piper.new(.env = .GlobalEnv)

    ...push(
        .this = {
            id = "test_module"
            description = "Test description"
            depends = {}
            onError = {}
        },
        {
            x <- 1
        }
    )

    pipe <- get("..", envir = .GlobalEnv)
    # Should not error
    suppressMessages({
        expect_no_error(pipe$map_pipeline())
    })

    piper.purge(.env = .GlobalEnv)
})

test_that("check_empty works correctly", {
    expect_true(check_empty(list()))
    expect_true(check_empty(NULL))
    expect_false(check_empty(list(a = 1)))
    expect_false(check_empty(c(1, 2, 3)))
})

test_that("pretty_print_table handles data frames", {
    test_df <- data.frame(
        module = c("test1", "test2"),
        workflow = c("desc1", "desc2"),
        dependencies = c("", "test1")
    )

    # Should not error
    suppressMessages({
        expect_no_error(pretty_print_table(test_df))
    })
})
