test_that("%-% returns default value when object doesn't exist", {
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
    if (exists("existing_var", envir = .GlobalEnv)) {
        rm("existing_var", envir = .GlobalEnv)
    }
})

test_that("%-% validates type correctly", {
    x <- 10
    result <- x %-% 5
    expect_equal(result, 10)

    x <- "string"
    expect_error(
        suppressMessages(x %-% 5),
        "Validation failed"
    )
    if (exists("x", envir = .GlobalEnv)) {
        rm("x", envir = .GlobalEnv)
    }
})

test_that("%-% handles NULL default value", {
    if (exists("test_var", envir = .GlobalEnv)) {
        rm("test_var", envir = .GlobalEnv)
    }
    result <- test_var %-% NULL
    expect_null(result)
})

test_that("%-% converts empty values by expected type (numeric -> NA_real_)", {
    empty_vec <- numeric(0)
    result <- empty_vec %-% c(1, 2, 3)
    expect_identical(result, NA_real_)
    expect_type(result, "double")
    if (exists("empty_vec", envir = .GlobalEnv)) {
        rm("empty_vec", envir = .GlobalEnv)
    }
})

test_that("%-% converts empty values by expected type (logical -> NA)", {
    empty_logical <- logical(0)
    result <- empty_logical %-% TRUE
    expect_identical(result, NA)
    expect_type(result, "logical")
    if (exists("empty_logical", envir = .GlobalEnv)) {
        rm("empty_logical", envir = .GlobalEnv)
    }
})

test_that("%-% converts empty values by expected type (character -> NA_character_)", {
    empty_chr <- character(0)
    result <- empty_chr %-% "default"
    expect_identical(result, NA_character_)
    expect_type(result, "character")
    if (exists("empty_chr", envir = .GlobalEnv)) {
        rm("empty_chr", envir = .GlobalEnv)
    }
})

test_that("%-% converts empty values by expected type (list -> NULL)", {
    empty_list <- list()
    result <- empty_list %-% list(a = 1)
    expect_null(result)
    if (exists("empty_list", envir = .GlobalEnv)) {
        rm("empty_list", envir = .GlobalEnv)
    }
})

test_that("%-% uses rhs when length 0 and expected type is not double/logical/character/list", {
    empty_int <- integer(0)
    result <- empty_int %-% 42L
    expect_identical(result, 42L)
    if (exists("empty_int", envir = .GlobalEnv)) {
        rm("empty_int", envir = .GlobalEnv)
    }
})

test_that("%-% handles different types", {
    x <- 10L
    result <- x %-% 5L
    expect_equal(result, 10L)
    expect_type(result, "integer")

    y <- 10.5
    result <- y %-% 5.0
    expect_equal(result, 10.5)
    expect_type(result, "double")

    z <- "test"
    result <- z %-% "default"
    expect_equal(result, "test")
    expect_type(result, "character")

    w <- TRUE
    result <- w %-% FALSE
    expect_equal(result, TRUE)
    expect_type(result, "logical")

    vars_to_remove <- c("x", "y", "z", "w")
    existing_vars <- vars_to_remove[vars_to_remove %in% ls(envir = .GlobalEnv)]
    if (length(existing_vars) > 0) {
        rm(list = existing_vars, envir = .GlobalEnv)
    }
})

test_that("%-% handles try-error from complex expressions", {
    result <- (nonexistent_object$property) %-% "default"
    expect_equal(result, "default")
})

test_that("%-% works with list access", {
    my_list <- list(a = 1, b = 2)

    result <- my_list$a %-% 0
    expect_equal(result, 1)

    result <- my_list$nonexistent %-% NULL
    expect_null(result)

    expect_error(
        suppressMessages(my_list$nonexistent %-% 999),
        "Validation failed"
    )
    if (exists("my_list", envir = .GlobalEnv)) {
        rm("my_list", envir = .GlobalEnv)
    }
})
