# piper <img src="assets/logo.png" align="right" height="138" />

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
