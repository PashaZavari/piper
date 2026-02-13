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

test_that("set_capture and get_captured record block inputs and exports", {
    piper.new(.env = .GlobalEnv)
    on.exit(piper.purge(.env = .GlobalEnv))

    ...push(
        .this = {
            id = "cap_a"
            description = "Capture test A"
            depends = {}
            export = list("x")
            onError = {}
        },
        { x <- 1 }
    )
    ...push(
        .this = {
            id = "cap_b"
            description = "Capture test B"
            depends = list("cap_a")
            imports = list(cap_a = list("x"))
            export = list("y")
            onError = {}
        },
        { y <- x + 1 }
    )

    pipe <- get("..", envir = .GlobalEnv)
    pipe$set_capture(TRUE)
    capture.output(suppressMessages({
        ...pipe("cap_a", .debug = FALSE)
        ...pipe("cap_b", .debug = FALSE)
    }), type = "output")
    out <- pipe$get_captured()
    expect_named(out, c("imports", "exports"))
    expect_named(out$imports, c("cap_a", "cap_b"))
    expect_named(out$exports, c("cap_a", "cap_b"))
    expect_equal(out$exports$cap_a$x, 1)
    expect_equal(out$imports$cap_b$cap_a$x, 1)
    expect_equal(out$exports$cap_b$y, 2)
})

test_that("piper.capture enables capture on .. in given env", {
    piper.new(.env = .GlobalEnv)
    on.exit(piper.purge(.env = .GlobalEnv))

    ...push(
        .this = {
            id = "cap_single"
            description = "Single"
            depends = {}
            export = list("z")
            onError = {}
        },
        { z <- 2 }
    )

    piper.capture(TRUE)
    capture.output(suppressMessages(...pipe("cap_single", .debug = FALSE)), type = "output")
    out <- get("..", envir = .GlobalEnv)$get_captured()
    expect_equal(out$exports$cap_single$z, 2)
})

test_that("capture disabled: get_captured returns empty when capture never enabled", {
    piper.new(.env = .GlobalEnv)
    on.exit(piper.purge(.env = .GlobalEnv))

    ...push(
        .this = {
            id = "no_cap"
            description = "No capture"
            depends = {}
            export = list("v")
            onError = {}
        },
        { v <- 10 }
    )

    capture.output(suppressMessages(...pipe("no_cap", .debug = FALSE)), type = "output")
    out <- get("..", envir = .GlobalEnv)$get_captured()
    expect_length(out$imports, 0)
    expect_length(out$exports, 0)
})

test_that("capture disabled after enable: later pipes not recorded", {
    piper.new(.env = .GlobalEnv)
    on.exit(piper.purge(.env = .GlobalEnv))

    ...push(
        .this = {
            id = "first"
            description = "First"
            depends = {}
            export = list("a")
            onError = {}
        },
        { a <- 1 }
    )
    ...push(
        .this = {
            id = "second"
            description = "Second"
            depends = list("first")
            imports = list(first = list("a"))
            export = list("b")
            onError = {}
        },
        { b <- a + 1 }
    )

    pipe <- get("..", envir = .GlobalEnv)
    pipe$set_capture(TRUE)
    capture.output(suppressMessages({
        ...pipe("first", .debug = FALSE)
        pipe$set_capture(FALSE)
        ...pipe("second", .debug = FALSE)
    }), type = "output")
    out <- pipe$get_captured()
    expect_equal(names(out$imports), "first")
    expect_equal(names(out$exports), "first")
    expect_equal(out$exports$first$a, 1)
})

test_that("set_capture(TRUE) clears previous captured data", {
    piper.new(.env = .GlobalEnv)
    on.exit(piper.purge(.env = .GlobalEnv))

    ...push(
        .this = {
            id = "clear_me"
            description = "Clear"
            depends = {}
            export = list("n")
            onError = {}
        },
        { n <- 42 }
    )

    pipe <- get("..", envir = .GlobalEnv)
    pipe$set_capture(TRUE)
    capture.output(suppressMessages(...pipe("clear_me", .debug = FALSE)), type = "output")
    out1 <- pipe$get_captured()
    expect_equal(out1$exports$clear_me$n, 42)

    pipe$set_capture(TRUE)
    out2 <- pipe$get_captured()
    expect_length(out2$imports, 0)
    expect_length(out2$exports, 0)
})

test_that("capture handles block with no imports and block with no exports", {
    piper.new(.env = .GlobalEnv)
    on.exit(piper.purge(.env = .GlobalEnv))

    ...push(
        .this = {
            id = "no_imports"
            description = "No imports"
            depends = {}
            export = list("foo")
            onError = {}
        },
        { foo <- "bar" }
    )
    ...push(
        .this = {
            id = "no_exports"
            description = "No exports"
            depends = list("no_imports")
            imports = list(no_imports = list("foo"))
            export = list()
            onError = {}
        },
        { baz <- foo }
    )

    pipe <- get("..", envir = .GlobalEnv)
    pipe$set_capture(TRUE)
    capture.output(suppressMessages({
        ...pipe("no_imports", .debug = FALSE)
        ...pipe("no_exports", .debug = FALSE)
    }), type = "output")
    out <- pipe$get_captured()
    expect_equal(out$imports$no_imports, list())
    expect_equal(out$exports$no_imports$foo, "bar")
    expect_equal(out$imports$no_exports$no_imports$foo, "bar")
    expect_equal(out$exports$no_exports, list())
})

# ---- Capture feature: pipe env is source of truth for imports/exports ----

test_that("capture reads imports and exports from pipe env (get_env)", {
    piper.new(.env = .GlobalEnv)
    on.exit(piper.purge(.env = .GlobalEnv))

    pipe <- get("..", envir = .GlobalEnv)
    # Blocks run in and write to pipe$get_env(); capture snapshots from that env
    ...push(
        .this = {
            id = "env_cap_a"
            description = "Exports to pipe env"
            depends = {}
            export = list("alpha")
            onError = {}
        },
        { alpha <- 100 }
    )
    ...push(
        .this = {
            id = "env_cap_b"
            description = "Reads from pipe env"
            depends = list("env_cap_a")
            imports = list(env_cap_a = list("alpha"))
            export = list("beta")
            onError = {}
        },
        { beta <- alpha + 1 }
    )

    pipe$set_capture(TRUE)
    capture.output(suppressMessages({
        ...pipe("env_cap_a", .debug = FALSE)
        ...pipe("env_cap_b", .debug = FALSE)
    }), type = "output")
    out <- pipe$get_captured()
    # Imports for B were captured from pipe env after A ran (so alpha was present)
    expect_equal(out$imports$env_cap_b$env_cap_a$alpha, 100)
    expect_equal(out$exports$env_cap_a$alpha, 100)
    expect_equal(out$exports$env_cap_b$beta, 101)
})

test_that("capture includes global imports when block declares global", {
    piper.new(.env = .GlobalEnv)
    on.exit({
        piper.purge(.env = .GlobalEnv)
        if (exists("global_payload", envir = .GlobalEnv)) rm(list = "global_payload", envir = .GlobalEnv)
    })

    pipe <- get("..", envir = .GlobalEnv)
    # Global imports are validated against and read from .GlobalEnv; use pipe env = .GlobalEnv
    pipe$set_env(.GlobalEnv)
    assign("global_payload", list(id = 1437L), envir = .GlobalEnv)

    ...push(
        .this = {
            id = "global_consumer"
            description = "Uses global import"
            depends = {}
            imports = list(global = list("global_payload"))
            export = list("seen_id")
            onError = {}
        },
        { seen_id <- global_payload$id }
    )

    pipe$set_capture(TRUE)
    capture.output(suppressMessages(...pipe("global_consumer", .debug = FALSE)), type = "output")
    out <- pipe$get_captured()
    expect_equal(out$imports$global_consumer$global$global_payload, list(id = 1437L))
    expect_equal(out$exports$global_consumer$seen_id, 1437L)
})

test_that("captured imports/exports round-trip as test_module inputs", {
    piper.new(.env = .GlobalEnv)
    on.exit(piper.purge(.env = .GlobalEnv))

    ...push(
        .this = {
            id = "round_a"
            description = "Round A"
            depends = {}
            export = list("r_a")
            onError = {}
        },
        { r_a <- 5 }
    )
    ...push(
        .this = {
            id = "round_b"
            description = "Round B"
            depends = list("round_a")
            imports = list(round_a = list("r_a"))
            export = list("r_b")
            onError = {}
        },
        { r_b <- r_a * 2 }
    )

    pipe <- get("..", envir = .GlobalEnv)
    pipe$set_capture(TRUE)
    capture.output(suppressMessages({
        ...pipe("round_a", .debug = FALSE)
        ...pipe("round_b", .debug = FALSE)
    }), type = "output")
    captured <- pipe$get_captured()

    # Using captured data as test_module inputs should reproduce the same result
    result <- suppressMessages(test_module(
        module_id = "round_b",
        imports = captured$imports$round_b,
        expected_exports = captured$exports$round_b
    ))
    expect_equal(result$status, "PASS")
})

test_that("capture with multiple dependencies records all sources", {
    piper.new(.env = .GlobalEnv)
    on.exit(piper.purge(.env = .GlobalEnv))

    ...push(
        .this = {
            id = "multi_a"
            description = "A"
            depends = {}
            export = list("a")
            onError = {}
        },
        { a <- 1 }
    )
    ...push(
        .this = {
            id = "multi_b"
            description = "B"
            depends = {}
            export = list("b")
            onError = {}
        },
        { b <- 2 }
    )
    ...push(
        .this = {
            id = "multi_c"
            description = "C depends on A and B"
            depends = list("multi_a", "multi_b")
            imports = list(
                multi_a = list("a"),
                multi_b = list("b")
            )
            export = list("c")
            onError = {}
        },
        { c <- a + b }
    )

    pipe <- get("..", envir = .GlobalEnv)
    pipe$set_capture(TRUE)
    capture.output(suppressMessages({
        ...pipe("multi_a", .debug = FALSE)
        ...pipe("multi_b", .debug = FALSE)
        ...pipe("multi_c", .debug = FALSE)
    }), type = "output")
    out <- pipe$get_captured()
    expect_equal(out$imports$multi_c$multi_a$a, 1)
    expect_equal(out$imports$multi_c$multi_b$b, 2)
    expect_equal(out$exports$multi_c$c, 3)
})

test_that("capture reads from pipe env set by set_env", {
    piper.new(.env = .GlobalEnv)
    on.exit(piper.purge(.env = .GlobalEnv))

    pipe <- get("..", envir = .GlobalEnv)
    # set_env(custom_env): blocks run in and write to custom_env; capture snapshots from it
    custom_env <- new.env(parent = .GlobalEnv)
    assign("seed", 42L, envir = custom_env)
    pipe$set_env(custom_env)

    ...push(
        .this = {
            id = "custom_env_block"
            description = "Runs in custom env"
            depends = {}
            # seed is in custom_env (the pipe env), so we declare it as from a dependency, not global
            # Global is validated against .GlobalEnv. So use a block with no global import:
            # block just uses seed from the pipe env (we put it there). So no imports declaration.
            export = list("doubled")
            onError = {}
        },
        { doubled <- seed * 2L }
    )

    pipe$set_capture(TRUE)
    capture.output(suppressMessages(...pipe("custom_env_block", .debug = FALSE)), type = "output")
    out <- pipe$get_captured()
    expect_equal(out$exports$custom_env_block$doubled, 84L)
})

test_that("piper.watch reports when watched variables first become available", {
    piper.new(.env = .GlobalEnv)
    on.exit({
        piper.purge(.env = .GlobalEnv)
        options(piper.watch = NULL)
    })

    ...push(
        .this = {
            id = "watch_a"
            description = "Watch A"
            depends = {}
            export = list("x")
            onError = {}
        },
        { x <- 1 }
    )
    ...push(
        .this = {
            id = "watch_b"
            description = "Watch B"
            depends = list("watch_a")
            imports = list(watch_a = list("x"))
            export = list("y")
            onError = {}
        },
        { y <- x + 10 }
    )

    suppressMessages(piper.watch(c("x", "y")))
    msgs <- capture.output(
        {
            ...pipe("watch_a", .debug = FALSE)
            ...pipe("watch_b", .debug = FALSE)
        },
        type = "message"
    )
    expect_true(
        any(grepl("'x' first became available after block 'watch_a'", msgs, fixed = TRUE)),
        info = paste(msgs, collapse = "\n")
    )
    expect_true(
        any(grepl("'y' first became available after block 'watch_b'", msgs, fixed = TRUE)),
        info = paste(msgs, collapse = "\n")
    )
})

test_that("piper.watch reports already-present variables when block starts", {
    piper.new(.env = .GlobalEnv)
    on.exit({
        piper.purge(.env = .GlobalEnv)
        options(piper.watch = NULL)
    })

    # Pre-create a variable in the pipeline env so it exists before any block
    suppressWarnings(piper.new(.env = .GlobalEnv))
    pipe <- get("..", envir = .GlobalEnv)
    assign("pre_existing", 100, envir = pipe$get_env())

    ...push(
        .this = {
            id = "watch_only"
            description = "Only block"
            depends = {}
            export = list("z")
            onError = {}
        },
        { z <- pre_existing + 1 }
    )

    suppressMessages(piper.watch(c("pre_existing", "z")))
    msgs <- capture.output(
        ...pipe("watch_only", .debug = FALSE),
        type = "message"
    )
    expect_true(
        any(grepl("'pre_existing' was already present when block 'watch_only' started", msgs, fixed = TRUE)),
        info = paste(msgs, collapse = "\n")
    )
    expect_true(
        any(grepl("'z' first became available after block 'watch_only'", msgs, fixed = TRUE)),
        info = paste(msgs, collapse = "\n")
    )
})
