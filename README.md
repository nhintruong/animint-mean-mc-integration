# Animint2_Tests

# Programming Language Popularity Analysis

This repository contains a collection of R projects focusing on various aspects of data analysis, visualization, and statistical modeling. The projects within this repository cover different domains and showcase the capabilities of R for developing data-driven solutions.

## Overview

The main objective of this repository is to provide a resource for learning and practicing R programming through hands-on projects. Each project is designed to explore different R functionalities, libraries, and best practices. Whether you are a beginner looking to build your R skills or an experienced user seeking interesting projects, you'll find a diverse range of topics to explore.

## Directory Structure

- **`data/`**: Contains the raw data files in CSV format.
- **`scripts/`**: Contains R scripts for data analysis and visualization.
- **`language_statistics/`**: Stores the generated plots and visualizations.

## Data Source

The programming language popularity data is collected from [insert data source link] and is stored in CSV format. The data includes information on the usage percentages of various programming languages for each month.


## Viewing Plots

Explore the visualizations generated by the projects. Click on the project name to view the plots:

- [Programming Language popularity over the years](https://tushar98644.github.io/Animint2_Tests/language_statistics/index.html)
- [Sample Mean Monte Carlo integration](https://tushar98644.github.io/Animint2_Tests/monte-carlo-integration/index.html)

## How to Use

1. Clone the repository:

    ```bash
    git clone https://github.com/Tushar98644/Animint2_Tests.git
    cd scripts
    ```

2. Run the R scripts:

    ```R
    # Install required packages
    install.packages(c("ggplot2", "tidyr", "dplyr", "zoo","animint2"))

    # Run the main analysis script
    source("scripts/language_stats.R")
    ```

3. View the results:

    - Plots and visualizations will be saved in the `output/` directory.

## Contributing

If you have suggestions, enhancements, or want to report issues, feel free to open an [issue](https://github.com/Tushar98644/Animint2_Tests/issues) or submit a pull request.

## License

This project is licensed under the [GPL-3.0 license](LICENSE).
