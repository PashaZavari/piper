test_that("piper.new initializes a new piper instance", {
    # Clean up any existing instance
    if (exists("..", envir = .GlobalEnv)) {
        piper.purge(.env = .GlobalEnv)
    }

    # Initialize new instance
    piper.new(.env = .GlobalEnv)
    expect_true(exists("..", envir = .GlobalEnv))
    expect_true(inherits(get("..", envir = .GlobalEnv), "piper"))

    # Clean up
    piper.purge(.env = .GlobalEnv)
})

test_that("piper.new handles auto_purge correctly", {
    # Initialize first instance
    piper.new(.env = .GlobalEnv)
    expect_true(exists("..", envir = .GlobalEnv))

    # Initialize again with auto_purge (default)
    expect_warning(piper.new(.env = .GlobalEnv), "is already defined")
    expect_true(exists("..", envir = .GlobalEnv))

    # Clean up
    piper.purge(.env = .GlobalEnv)
})

test_that("piper.new with auto_purge = FALSE warns but doesn't replace", {
    # Initialize first instance
    piper.new(.env = .GlobalEnv)
    old_instance <- get("..", envir = .GlobalEnv)

    # Try to initialize again with auto_purge = FALSE
    expect_warning(piper.new(.env = .GlobalEnv, auto_purge = FALSE), "is already defined")
    new_instance <- get("..", envir = .GlobalEnv)

    # Should be the same instance
    expect_identical(old_instance, new_instance)

    # Clean up
    piper.purge(.env = .GlobalEnv)
})

test_that("piper.purge removes .. from global environment", {
    piper.new(.env = .GlobalEnv)
    expect_true(exists("..", envir = .GlobalEnv))

    piper.purge(.env = .GlobalEnv)
    expect_false(exists("..", envir = .GlobalEnv))
})
