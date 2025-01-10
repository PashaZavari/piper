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
