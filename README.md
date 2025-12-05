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

module_.push(
    .this = {
        id = "main_rng"
        description = "Random number generator"
        depends = {}
        onError = {}
    }, {
        rng <- runif(n = 1000, min = -1, max = 1)
    }
)

module_.push(
    .this = {
        id = "main_gm"
        description = "Calculate Gaussian moments"
        depends = list(
            "main_rng"
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

module_.push(
    .this = {
        id = "main_cp"
        description = "Calculate probability of a random number."
        depends = list(
            "main_gm"
        )
        onError = {}
    }, {
        z_score <- (random_number - mean_value) / sd_value
        upper_prob <- pnorm(s_score, lower.tail = FALSE)
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
module_.compute("main_rng", .debug = FALSE)
module_.compute("main_gm", .debug = FALSE)
module_.compute("main_cp", .debug = FALSE)
```
You can also export specific variables from each module or deport ones that you wish to drop: 

```R
module_.push(
    .this = {
        id = "main_gm"
        description = "Calculate Gaussian moments"
        depends = list(
            "main_rng"
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
