# Justfile for piper package development

# Default recipe - show available commands
default:
    just --list


# Install package dependencies
install-deps:
    @echo "Installing package dependencies..."
    Rscript -e "devtools::install_deps()"

# Rebuild documentation (roxygen)
docs:
    @echo "Rebuilding documentation..."
    Rscript -e "roxygen2::roxygenize()"

# Build the package (creates tarball)
build: docs
    @echo "Building piper package..."
    Rscript -e "devtools::build()"

# Install the package locally
install:
    @echo "Installing piper package..."
    Rscript -e "devtools::install()"

# Run tests
test:
    @echo "Running tests..."
    Rscript -e "devtools::test()"

# Run tests with output
test-verbose:
    @echo "Running tests with verbose output..."
    Rscript -e "devtools::test(reporter = 'verbose')"

# Run package check (comprehensive package check)
check:
    rm -rf ..Rcheck
    @echo "Running package check..."
    Rscript -e "devtools::check()"

