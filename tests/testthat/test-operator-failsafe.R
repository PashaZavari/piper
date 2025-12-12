test_that("%-% returns default value when object doesn't exist", {
    # Clean up if variable exists
    if (exists("nonexistent_var", envir = .GlobalEnv)) {
        rm("nonexistent_var", envir = .GlobalEnv)
    }

    result <- nonexistent_var %-% 42
    expect_equal(result, 42)
})

test_that("%-% returns existing value when object exists", {
    existing_var <- 100
    result <- existing_var %-% 42
    expect_equal(result, 100)

    # Clean up
    if (exists("existing_var", envir = .GlobalEnv)) {
        rm("existing_var", envir = .GlobalEnv)
    }
})

test_that("%-% validates type correctly", {
    # Valid type match
    x <- 10
    result <- x %-% 5
    expect_equal(result, 10)

    # Type mismatch should error
    x <- "string"
    expect_error(
        x %-% 5,
        "Validation failed"
    )

    # Clean up
    if (exists("x", envir = .GlobalEnv)) {
        rm("x", envir = .GlobalEnv)
    }
})

test_that("%-% handles NULL default value", {
    if (exists("test_var", envir = .GlobalEnv)) {
        rm("test_var", envir = .GlobalEnv)
    }

    # NULL should not trigger type validation
    result <- test_var %-% NULL
    expect_null(result)
})

test_that("%-% converts empty values to NA", {
    empty_vec <- numeric(0)
    result <- empty_vec %-% c(1, 2, 3)
    expect_true(is.na(result))

    # Clean up
    if (exists("empty_vec", envir = .GlobalEnv)) {
        rm("empty_vec", envir = .GlobalEnv)
    }
})

test_that("%-% handles different types", {
    # Integer
    x <- 10L
    result <- x %-% 5L
    expect_equal(result, 10L)
    expect_type(result, "integer")

    # Double
    y <- 10.5
    result <- y %-% 5.0
    expect_equal(result, 10.5)
    expect_type(result, "double")

    # Character
    z <- "test"
    result <- z %-% "default"
    expect_equal(result, "test")
    expect_type(result, "character")

    # Logical
    w <- TRUE
    result <- w %-% FALSE
    expect_equal(result, TRUE)
    expect_type(result, "logical")

    # Clean up - only remove variables that exist
    vars_to_remove <- c("x", "y", "z", "w")
    existing_vars <- vars_to_remove[vars_to_remove %in% ls(envir = .GlobalEnv)]
    if (length(existing_vars) > 0) {
        rm(list = existing_vars, envir = .GlobalEnv)
    }
})

test_that("%-% handles try-error from complex expressions", {
    # Expression that would error
    result <- (nonexistent_object$property) %-% "default"
    expect_equal(result, "default")
})

test_that("%-% works with list access", {
    my_list <- list(a = 1, b = 2)

    # Existing element
    result <- my_list$a %-% 0
    expect_equal(result, 1)

    # Non-existing element returns NULL, which has length 0
    # So it gets converted to NA per the operator's logic
    result <- my_list$nonexistent %-% NULL
    expect_true(is.na(result))

    # Type mismatch should error when accessing non-existent element
    expect_error(
        my_list$nonexistent %-% 999,
        "Validation failed"
    )

    # Clean up
    if (exists("my_list", envir = .GlobalEnv)) {
        rm("my_list", envir = .GlobalEnv)
    }
})
