# Clinical Analysis Pipeline

> **Author:** Andrea Peccati (`Peccadotto`)\
> **Repository:** <https://github.com/peccadotto/R_clinical_analysis>

## Overview

The **Clinical Analysis Pipeline** is an automated workflow built in **R** and integrated with **Quarto** (`.qmd`) to process, aggregate, and generate comprehensive PDF and HTML reports of outpatient visits, diagnostic examinations, and medical consultations. It is designed to handle multi-center clinical logs, normalize service types across different healthcare facilities, calculate key performance indicators (such as occupancy rates and annual patient volumes), and export clean analytical outputs in multiple formats (`.rds` and `.xlsx`).

------------------------------------------------------------------------

## Key Features

- **Automated Package Management:** Automatically checks for required dependencies, installs missing packages from CRAN, and handles environment setups.
- **Data Integration & Cleansing:** Merges historical CSV datasets from multiple clinical facilities, parses dates and appointment times, and handles specialized service multipliers (e.g., doubling ultrasound records to count examinations rather than patients).
- **Advanced Aggregations & Pivots:**
  - Facility-level patient volume tracking with date spans, operational durations, and annualized patient rates.
  - Yearly and monthly clinical productivity breakdowns with running totals and percentage distributions.
  - Daily time slot occupancy analysis across operational hours (8:00 AM - 8:00 PM).
  - Service type and practice system breakdowns mapped against customized professional categories.
  - Cross-tabulated pivot matrices for facility-by-year and service-by-year distributions.
- **Multi-Format Exporting:** Saves processed datasets and pivot tables into an internal R structure (`export.rds`) and an Excel workbook (`export.xlsx`) containing structured sheets for downstream reporting.
- **Dynamic Quarto Reporting:** Compiles custom-styled PDF and interactive HTML reports featuring:
  - Professional custom ggplot themes and tailored color palettes.
  - Treemap visualizations for service and system proportions via `treemap`.
  - Striped data tables via `kableExtra` highlighting peak activity metrics and totals.

------------------------------------------------------------------------

## Project Structure

``` text
R_clinical_analysis/
│
├── data/
│   ├── camozzi.csv             # Raw clinical logs (CAM)
│   ├── humanitas.csv           # Raw clinical logs (ICH)
│   ├── medcare.csv             # Raw clinical logs (HMC)
│   ├── modoetia.csv            # Raw clinical logs (MOD)
│   ├── policlinico.csv         # Raw clinical logs (POL)
│   ├── salerno.csv             # Raw clinical logs (SAL)
│   ├── zucchi.csv              # Raw clinical logs (ICZ)
│   ├── export.rds              # Processed R data list (generated)
│   └── export.xlsx             # Processed Excel workbook (generated)
│
├── version_2.R                 # Main pipeline script
├── report_version_2.qmd        # Quarto document template (pdf)
├── report_version_2_plot.qmd   # Quarto document template (html)
└── README.md                   # Project documentation
```

------------------------------------------------------------------------

## Prerequisites & Dependencies

The R script automatically verifies and installs the following required CRAN packages if they are missing:

- `tidyverse` (Data manipulation and visualization: `dplyr`, `stringr`, `readr`, etc.)
- `formattable` (Formatting numbers and percentages)
- `janitor` (Cross-tabulation and total row adornment)
- `lubridate` (Date and time parsing)
- `quarto` (Dynamic report rendering interface)
- `scales` (Scale functions for graphics and formatting)
- `treemap` (Hierarchical visualization of proportions)
- `writexl` (Exporting data frames to Excel spreadsheets)

> **Note on Quarto:** Ensure that Quarto is installed on your system. The script includes a Windows path configuration for RStudio's bundled Quarto binary (`C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe`), which can be modified depending on your operating system.

------------------------------------------------------------------------

## Usage Guide

1.  **Clone the Repository:**

    ``` bash
    git clone https://github.com/peccadotto/R_clinical_analysis.git
    cd R_clinical_analysis
    ```

2.  **Prepare Your Data:** Place your source CSV files inside the `data/` directory matching the facility names (`camozzi.csv`, `humanitas.csv`, `medcare.csv`, etc.), ensuring they include semicolon separators (`sep = ";"`) and appropriate column headers (`Date`, `Time`, `Service`, `System`, etc.).

3.  **Run the Analysis Script:** Open and execute `version_2.R` in RStudio or via R command line:

    ``` r
    source("version_2.R")
    ```

    This script will process the raw facility logs, print summary tables to the console, save the processed outputs to `data/export.rds` and `data/export.xlsx`, and automatically trigger the Quarto renders to output the final PDF and HTML reports.

------------------------------------------------------------------------

## License

This project is open-source and available under the [MIT License](LICENSE).
