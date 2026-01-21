test_that("circular dependency between two modules causes error", {
    piper.new(.env = .GlobalEnv)

    # Module A depends on B
    ...push(
        .this = {
            id = "module_a"
            description = "Module A"
            depends = list("module_b")
            imports = list(
                module_b = list("value_b")
            )
            onError = {}
        },
        {
            value_a <- value_b + 1
        }
    )

    # Module B depends on A (circular!)
    ...push(
        .this = {
            id = "module_b"
            description = "Module B"
            depends = list("module_a")
            imports = list(
                module_a = list("value_a")
            )
            onError = {}
        },
        {
            value_b <- value_a + 1
        }
    )

    # Attempting to compute either module should fail due to circular dependency
    # This will cause infinite recursion in auto-loading
    expect_error(
        suppressMessages({
            ...pipe("module_a")
        }),
        regexp = "Auto-load failed|Missing import|has not been executed"
    )

    piper.purge(.env = .GlobalEnv)
})

test_that("circular dependency in three-module chain causes error", {
    piper.new(.env = .GlobalEnv)

    # A -> B -> C -> A (circular)
    ...push(
        .this = {
            id = "mod_a"
            description = "Module A"
            depends = list("mod_c")
            imports = list(
                mod_c = list("value_c")
            )
            onError = {}
        },
        {
            value_a <- value_c + 1
        }
    )

    ...push(
        .this = {
            id = "mod_b"
            description = "Module B"
            depends = list("mod_a")
            imports = list(
                mod_a = list("value_a")
            )
            onError = {}
        },
        {
            value_b <- value_a + 1
        }
    )

    ...push(
        .this = {
            id = "mod_c"
            description = "Module C"
            depends = list("mod_b")
            imports = list(
                mod_b = list("value_b")
            )
            onError = {}
        },
        {
            value_c <- value_b + 1
        }
    )

    # Should fail due to circular dependency
    expect_error(
        suppressMessages({
            ...pipe("mod_a")
        }),
        regexp = "Auto-load failed|Missing import|has not been executed"
    )

    piper.purge(.env = .GlobalEnv)
})

test_that("self-referential module causes error", {
    piper.new(.env = .GlobalEnv)

    # Module depends on itself
    ...push(
        .this = {
            id = "self_ref"
            description = "Self-referential module"
            depends = list("self_ref")
            onError = {}
        },
        {
            value <- 42
        }
    )

    # Should fail when trying to compute
    expect_error(
        suppressMessages({
            ...pipe("self_ref")
        }),
        regexp = "Auto-load failed|Missing import|has not been executed"
    )

    piper.purge(.env = .GlobalEnv)
})

test_that("circular dependency detected during auto-load", {
    piper.new(.env = .GlobalEnv)

    # Set up circular dependency
    ...push(
        .this = {
            id = "circ_a"
            description = "Circular A"
            depends = list("circ_b")
            imports = list(
                circ_b = list("b_val")
            )
            onError = {}
        },
        {
            a_val <- b_val * 2
        }
    )

    ...push(
        .this = {
            id = "circ_b"
            description = "Circular B"
            depends = list("circ_a")
            imports = list(
                circ_a = list("a_val")
            )
            onError = {}
        },
        {
            b_val <- a_val / 2
        }
    )

    # When computing circ_a, it will try to auto-load circ_b
    # which will try to auto-load circ_a, creating infinite recursion
    expect_error(
        suppressMessages({
            ...pipe("circ_a")
        }),
        regexp = "Auto-load failed|Missing import|has not been executed"
    )

    piper.purge(.env = .GlobalEnv)
})
