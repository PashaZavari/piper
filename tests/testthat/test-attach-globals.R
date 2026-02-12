test_that("attach_globals exports arguments to environment", {
    test_env <- new.env()

    attach_globals(a = 1, b = 2, c = "test", .env = test_env)

    expect_true(exists("a", envir = test_env))
    expect_true(exists("b", envir = test_env))
    expect_true(exists("c", envir = test_env))
    expect_equal(get("a", envir = test_env), 1)
    expect_equal(get("b", envir = test_env), 2)
    expect_equal(get("c", envir = test_env), "test")
})

test_that("attach_globals uses parent.frame() by default", {
    # Create a function that calls attach_globals
    test_function <- function() {
        attach_globals(x = 10, y = 20)
        return(list(x = x, y = y))
    }

    result <- test_function()
    expect_equal(result$x, 10)
    expect_equal(result$y, 20)
})

test_that("attach_globals extracts from .data object", {
    test_env <- new.env()
    test_data <- list(a = 100, b = 200, c = 300)

    attach_globals(a, b, .data = test_data, .env = test_env)

    expect_true(exists("a", envir = test_env))
    expect_true(exists("b", envir = test_env))
    expect_equal(get("a", envir = test_env), 100)
    expect_equal(get("b", envir = test_env), 200)
    # Note: The function extracts requested variables from .data
    # The key test is that 'a' and 'b' have correct values
})

test_that("attach_globals applies transform function with .f", {
    test_env <- new.env()
    test_data <- list(a = 10, b = 20)

    attach_globals(a, b, .data = test_data, .f = function(x) x * 2, .env = test_env)

    expect_true(exists("a", envir = test_env))
    expect_true(exists("b", envir = test_env))
    expect_equal(get("a", envir = test_env), 20)
    expect_equal(get("b", envir = test_env), 40)
})

test_that("attach_globals handles empty .data", {
    test_env <- new.env()
    empty_data <- list()

    # Should use ... arguments when .data is empty
    attach_globals(x = 5, .data = empty_data, .env = test_env)

    expect_true(exists("x", envir = test_env))
    expect_equal(get("x", envir = test_env), 5)
})

test_that("attach_globals handles NULL .data", {
    test_env <- new.env()

    # Should use ... arguments when .data is NULL
    attach_globals(x = 5, .data = NULL, .env = test_env)

    expect_true(exists("x", envir = test_env))
    expect_equal(get("x", envir = test_env), 5)
})

test_that("attach_globals can extract nested data", {
    test_env <- new.env()
    nested_data <- list(
        level1 = list(
            level2 = list(
                value = 42
            )
        )
    )

    # This tests the pluck functionality
    attach_globals(value, .data = nested_data$level1$level2, .env = test_env)

    expect_true(exists("value", envir = test_env))
    expect_equal(get("value", envir = test_env), 42)
})

test_that("attach_globals overwrites existing variables", {
    test_env <- new.env()
    assign("x", 1, envir = test_env)

    attach_globals(x = 2, .env = test_env)

    expect_equal(get("x", envir = test_env), 2)
})
