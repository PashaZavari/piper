test_that("empty module with no code block works", {
    piper.new(.env = .GlobalEnv)

    # Module with empty code block
    module_.push(
        .this = {
            id = "empty_module"
            description = "Empty module"
            depends = {}
            onError = {}
        },
        {
            # Empty block
        }
    )

    # Should be able to compute it (just does nothing)
    suppressMessages({
        module_.compute("empty_module")
    })

    # Module should be in stack
    pipe <- get("module_", envir = .GlobalEnv)
    expect_true("empty_module" %in% pipe$get_stack())

    piper.purge(.env = .GlobalEnv)
})

test_that("module with minimal metadata works", {
    piper.new(.env = .GlobalEnv)

    # Module with only required fields
    module_.push(
        .this = {
            id = "minimal_module"
        },
        {
            x <- 1
        }
    )

    # Should work
    suppressMessages({
        module_.compute("minimal_module")
    })

    env <- module_.env()
    expect_true(exists("x", envir = env))
    expect_equal(get("x", envir = env), 1)

    piper.purge(.env = .GlobalEnv)
})

test_that("very long dependency chain works", {
    piper.new(.env = .GlobalEnv)

    # Create a chain of 10 modules
    # First module
    module_.push(
        .this = {
            id = "chain_1"
            description = "Chain module 1"
            depends = {}
            export = list("value")
            onError = {}
        },
        {
            value <- 1
        }
    )

    # Subsequent modules depend on previous
    # Need to create each module individually to avoid variable scoping issues
    module_.push(
        .this = {
            id = "chain_2"
            description = "Chain module 2"
            depends = list("chain_1")
            imports = list(chain_1 = list("value"))
            export = list("value")
            onError = {}
        },
        {
            value <- value + 1
        }
    )
    module_.push(
        .this = {
            id = "chain_3"
            description = "Chain module 3"
            depends = list("chain_2")
            imports = list(chain_2 = list("value"))
            export = list("value")
            onError = {}
        },
        {
            value <- value + 1
        }
    )
    module_.push(
        .this = {
            id = "chain_4"
            description = "Chain module 4"
            depends = list("chain_3")
            imports = list(chain_3 = list("value"))
            export = list("value")
            onError = {}
        },
        {
            value <- value + 1
        }
    )
    module_.push(
        .this = {
            id = "chain_5"
            description = "Chain module 5"
            depends = list("chain_4")
            imports = list(chain_4 = list("value"))
            export = list("value")
            onError = {}
        },
        {
            value <- value + 1
        }
    )
    # Test with 5 modules instead of 10 to keep it manageable

    # Compute the last module - should auto-load all dependencies
    suppressMessages({
        module_.compute("chain_5")
    })

    # Verify all modules are in stack
    pipe <- get("module_", envir = .GlobalEnv)
    stack <- pipe$get_stack()
    expect_true(all(paste0("chain_", 1:5) %in% stack))

    # Verify final value
    env <- module_.env()
    expect_true(exists("value", envir = env))
    expect_equal(get("value", envir = env), 5)

    piper.purge(.env = .GlobalEnv)
})

test_that("module with no exports works correctly", {
    piper.new(.env = .GlobalEnv)

    # Module that creates variables but doesn't export any
    module_.push(
        .this = {
            id = "no_export_module"
            description = "Module with no exports"
            depends = {}
            export = list() # Explicitly empty
            onError = {}
        },
        {
            temp1 <- 1
            temp2 <- 2
            temp3 <- 3
        }
    )

    suppressMessages({
        module_.compute("no_export_module")
    })

    env <- module_.env()
    # No variables should be exported
    expect_false(exists("temp1", envir = env))
    expect_false(exists("temp2", envir = env))
    expect_false(exists("temp3", envir = env))

    piper.purge(.env = .GlobalEnv)
})

test_that("module that only deports variables works", {
    piper.new(.env = .GlobalEnv)

    # Module that deports everything (nothing exported)
    module_.push(
        .this = {
            id = "deport_all_module"
            description = "Module that deports all"
            depends = {}
            deport = list("a", "b", "c")
            onError = {}
        },
        {
            a <- 1
            b <- 2
            c <- 3
            d <- 4
        }
    )

    suppressMessages({
        module_.compute("deport_all_module")
    })

    env <- module_.env()
    # Deported variables should not exist
    # Note: Check that they don't exist OR that they're not the values we set
    # (base R has a 'c' function, so exists("c") might return TRUE for the function)
    if (exists("a", envir = env)) {
        expect_false(get("a", envir = env) == 1) # Should not be our value
    }
    if (exists("b", envir = env)) {
        expect_false(get("b", envir = env) == 2) # Should not be our value
    }
    if (exists("c", envir = env)) {
        # If 'c' exists, it might be the base function, check it's not our value
        c_val <- tryCatch(get("c", envir = env), error = function(e) NULL)
        if (!is.null(c_val) && !is.function(c_val)) {
            expect_false(c_val == 3) # Should not be our value
        }
    }
    # d should exist (not in deport list)
    expect_true(exists("d", envir = env))
    expect_equal(get("d", envir = env), 4)

    piper.purge(.env = .GlobalEnv)
})

test_that("module with very long variable names works", {
    piper.new(.env = .GlobalEnv)

    # Use a reasonably long variable name (20 chars) to test the functionality
    # Using a simpler approach with a fixed name
    module_.push(
        .this = {
            id = "long_names_module"
            description = "Module with long variable names"
            depends = {}
            export = list("very_long_var_name_xx")
            onError = {}
        },
        {
            very_long_var_name_xx <- 42
        }
    )

    suppressMessages({
        module_.compute("long_names_module")
    })

    env <- module_.env()
    expect_true(exists("very_long_var_name_xx", envir = env))
    expect_equal(get("very_long_var_name_xx", envir = env), 42)

    piper.purge(.env = .GlobalEnv)
})

test_that("module with many dependencies works", {
    piper.new(.env = .GlobalEnv)

    # Create 5 independent modules (create individually to avoid scoping issues)
    module_.push(
        .this = {
            id = "dep_1"
            description = "Dependency 1"
            depends = {}
            export = list("val_1")
            onError = {}
        },
        {
            val_1 <- 10
        }
    )
    module_.push(
        .this = {
            id = "dep_2"
            description = "Dependency 2"
            depends = {}
            export = list("val_2")
            onError = {}
        },
        {
            val_2 <- 20
        }
    )
    module_.push(
        .this = {
            id = "dep_3"
            description = "Dependency 3"
            depends = {}
            export = list("val_3")
            onError = {}
        },
        {
            val_3 <- 30
        }
    )
    module_.push(
        .this = {
            id = "dep_4"
            description = "Dependency 4"
            depends = {}
            export = list("val_4")
            onError = {}
        },
        {
            val_4 <- 40
        }
    )
    module_.push(
        .this = {
            id = "dep_5"
            description = "Dependency 5"
            depends = {}
            export = list("val_5")
            onError = {}
        },
        {
            val_5 <- 50
        }
    )

    # Create a module that depends on all 5
    module_.push(
        .this = {
            id = "many_deps_module"
            description = "Module with many dependencies"
            depends = list("dep_1", "dep_2", "dep_3", "dep_4", "dep_5")
            imports = list(
                dep_1 = list("val_1"),
                dep_2 = list("val_2"),
                dep_3 = list("val_3"),
                dep_4 = list("val_4"),
                dep_5 = list("val_5")
            )
            export = list("sum")
            onError = {}
        },
        {
            sum <- val_1 + val_2 + val_3 + val_4 + val_5
        }
    )

    # Compute - should auto-load all dependencies
    suppressMessages({
        module_.compute("many_deps_module")
    })

    env <- module_.env()
    expect_true(exists("sum", envir = env))
    expect_equal(get("sum", envir = env), 150) # 10 + 20 + 30 + 40 + 50

    piper.purge(.env = .GlobalEnv)
})

test_that("module with empty depends list works", {
    piper.new(.env = .GlobalEnv)

    # Module with explicit empty depends
    module_.push(
        .this = {
            id = "empty_depends"
            description = "Module with empty depends"
            depends = list() # Explicitly empty list
            onError = {}
        },
        {
            x <- 42
        }
    )

    suppressMessages({
        module_.compute("empty_depends")
    })

    env <- module_.env()
    expect_true(exists("x", envir = env))
    expect_equal(get("x", envir = env), 42)

    piper.purge(.env = .GlobalEnv)
})

test_that("module with empty imports works", {
    piper.new(.env = .GlobalEnv)

    # Module with explicit empty imports
    module_.push(
        .this = {
            id = "empty_imports"
            description = "Module with empty imports"
            depends = {}
            imports = list() # Explicitly empty
            onError = {}
        },
        {
            x <- 100
        }
    )

    suppressMessages({
        module_.compute("empty_imports")
    })

    env <- module_.env()
    expect_true(exists("x", envir = env))
    expect_equal(get("x", envir = env), 100)

    piper.purge(.env = .GlobalEnv)
})
