# piper <img src="https://github.com/PashaZavari/piper/blob/master/assets/logo.png?raw=true" align="right" height="138" />

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)  
[![Build Status](https://github.com/PashaZavari/piper/workflows/R-CMD-check/badge.svg)](https://github.com/PashaZavari/piper/actions)


# Piper: Modular Pipelines with Dependency and Namespace Management in R

**Piper** is an R package designed to simplify the creation and management of large, complex ETL (Extract, Transform, Load) or computational pipelines. It enables pipelines to be broken into modular blocks that can run sequentially with a dependency structure, while each module’s environment namespace is made available globally for seamless integration with other modules.

---

## Key Features

- **Modular Design**: Decompose complex workflows into smaller, reusable pipeline modules.
- **Dependency Management**: Automatically resolve and manage dependencies between pipeline modules.
- **Shared Namespace**: Each module’s environment is made accessible globally, enabling inter-module communication.
- **Scalable Execution**: Build scalable pipelines for handling ETL processes or computational tasks.
- **Customizable Logic**: Easily customize individual pipeline blocks with your own R functions.

---

## Installation

Install Piper directly from GitHub:

```R
# Install devtools if you haven't already
install.packages("devtools")

# Install Piper
devtools::install_github("PashaZavari/piper")

```

## Setup

```R
# Loaded piper
library(piper)

# Initialize Instance [Execute on each call]
piper.new()

# Create fresh pipe project [Execute only during project setup]
piper.make(pipe = "package_pipe")

# Attach new module library [Execute only during project setup]
piper.module(pipe = "package_pipe", module = "xyz_module")

# Build version specific assets [Execute only during release version upgrade]
piper.brew(pipe = "package_pipe", module = "xyz_module", version = "v1.0", header = "main.r")

```

## Example

```bash
$ cd ~/[package-root-directory]
```

Open `~/[package-root-directory]/R++/package_pipe/xyz_module/v1.0/main.r` in your favorite IDE (RStudio, vsCode, Positron etc...) and add your modules using the `piper` verbiage:

```R
#./[package-root-directory]/R++/package_pipe/xyz_module/v1.0/main.r

...push(
    .this = {
        id = "main_rng"
        description = "Random number generator"
        depends = {}
        onError = {}
    }, {
        rng <- runif(n = 1000, min = -1, max = 1)
    }
)

...push(
    .this = {
        id = "main_gm"
        description = "Calculate Gaussian moments"
        depends = list(
            "main_rng"
        )
        imports = list(
            main_rng = list("rng")
        )
        onError = {}
    }, {
        # Calculate the mean and standard deviation of the random numbers
        mean_value <- mean(rng)
        sd_value <- sd(rng)

        # Print the mean and standard deviation
        cat("Mean:", mean_value, "\n")
        cat("Standard Deviation:", sd_value, "\n")
    }
)

...push(
    .this = {
        id = "main_cp"
        description = "Calculate probability of a random number."
        depends = list(
            "main_gm"
        )
        imports = list(
            main_gm = list("mean_value", "sd_value"),
            global = list("random_number")
        )
        onError = {}
    }, {
        z_score <- (random_number - mean_value) / sd_value
        upper_prob <- pnorm(z_score, lower.tail = FALSE)
    }
)
```

Now we create an `accessor` to execute the pipeline. 

```bash
$ touch R/gaussian.R
```

Open `gaussian.R` and follow the logic below:

```R
# ./R/gaussian.R

# Initialize pipeline
piper.new()

# Load module assets [Execute on every function call]
piper.load(pipe = "package_pipe", module = "xyz_module", version = "v1.0", header = "main.r")

# Execute random number pipeline
random_number <- 10
...pipe("main_rng", .debug = FALSE)
...pipe("main_gm", .debug = FALSE)
...pipe("main_cp", .debug = FALSE)
```
You can also export specific variables from each module or deport ones that you wish to drop: 

```R
...push(
    .this = {
        id = "main_gm"
        description = "Calculate Gaussian moments"
        depends = list(
            "main_rng"
        )
        imports = list(
            main_rng = list("rng")
        )
        export = list("mean_value", "sd_value")
        deport = list("median_value")
        onError = {}
    }, {
        # Calculate the mean and standard deviation of the random numbers
        median_value <- median(rng)
        mean_value <- mean(rng)
        sd_value <- sd(rng)

        # Print the mean and standard deviation
        cat("Mean:", mean_value, "\n")
        cat("Standard Deviation:", sd_value, "\n")
    }
)
```

## Module Arguments

### `depends`

A list of module IDs that must be executed before this module. The pipeline will automatically resolve and execute dependencies in the correct order.

```R
depends = list("main_rng", "another_module")
```

### `imports`

The `imports` argument explicitly declares where each variable used in a module originates from. This enables better refactoring, parallelization, and testing. The format is a nested list where:

- **Keys** are module IDs (from `depends`) or `"global"` for global environment variables
- **Values** are lists of variable names that come from that source

**Example with multiple import sources:**

```R
imports = list(
    main_gm = list("mean_value", "sd_value"),
    main_rng = list("rng"),
    global = list("random_number", "threshold")
)
```

### `export`

A list of variable names that should be exported from the module to the global environment. If not specified, all variables created in the module are exported.

```R
export = list("mean_value", "sd_value")
```

### `deport`

A list of variable names that should be excluded from exports, even if they are created in the module. Useful for intermediate variables that shouldn't pollute the global namespace.

```R
deport = list("median_value", "temp_calculation")
```

### `onError`

A named list of error handling instructions. Currently used for custom error messages and error handling behavior.

```R
onError = list(
    message = "Custom error message",
    action = "stop"
)
```

**Complete example with all arguments:**

```R
...push(
    .this = {
        id = "main_cp"
        description = "Calculate probability of a random number."
        depends = list("main_gm", "main_rng")
        imports = list(
            main_gm = list("mean_value", "sd_value"),
            main_rng = list("rng"),
            global = list("random_number", "threshold")
        )
        export = list("z_score", "upper_prob")
        deport = list("intermediate_value")
        onError = {}
    }, {
        z_score <- (random_number - mean_value) / sd_value
        upper_prob <- pnorm(z_score, lower.tail = FALSE)
    }
)
```

## Testing Modules

Each module can be independently tested using the `test_module` function, which sets up required imports, executes the module in isolation, and validates exports. The function prevents dependency modules from auto-executing by pre-populating the execution stack, ensuring true isolation for testing.

**Example test file:**

```R
# test/test_modules.R
library(testthat)
library(piper)

test_that("main_gm module calculates moments correctly", {
    # Initialize pipeline
    piper.new()
    
    # Load module assets
    piper.load(pipe = "package_pipe", module = "xyz_module", version = "v1.0", header = "main.r")
    
    # Test the module with provided imports
    result <- test_module(
        ..id = "main_gm",
        imports = list(
            main_rng = list(rng = runif(1000, min = -1, max = 1))
        ),
        expected_exports = list(
            mean_value = 0.0,  # Approximate expected value
            sd_value = 0.577   # Approximate expected value for uniform(-1, 1)
        )
    )
    
    expect_equal(result$status, "PASS")
})

test_that("main_cp module calculates probability correctly", {
    # Initialize pipeline
    piper.new()
    
    # Load module assets
    piper.load(pipe = "package_pipe", module = "xyz_module", version = "v1.0", header = "main.r")
    
    # Test the module with provided imports
    result <- test_module(
        ..id = "main_cp",
        imports = list(
            main_gm = list(
                mean_value = 0.0,
                sd_value = 1.0
            ),
            global = list(
                random_number = 1.96
            )
        ),
        expected_exports = list(
            z_score = 1.96,
            upper_prob = pnorm(1.96, lower.tail = FALSE)
        )
    )
    
    expect_equal(result$status, "PASS")
})
```
