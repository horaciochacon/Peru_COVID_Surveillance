# COVID-19 Surveillance Secondary Data Source Analysis

This repository contains the code and data used for analyzing secondary data sources related to COVID-19 surveillance in Peru. The analysis covers various aspects such as data completeness, validity, concordance, and timeliness.

## Repository Structure


Key Files and Directories
Analysis.Rmd: An R Markdown file containing the main analysis code.

R/: Directory containing R functions used in the analysis.
Report.pdf: The final analysis report in PDF format.
Report.qmd: A Quarto file for generating the final report.
data/: Directory containing the input data files.
output/: Directory containing the generated output files, such as plots and maps.
renv/: Directory containing the R environment configuration files.
renv.lock: File locking the R package versions used in the analysis.
src/: Directory containing various R scripts used in the analysis pipeline.

## Getting Started
To run the analysis locally, follow these steps:

- Clone the repository: git clone https://github.com/your-repo/covid-19-surveillance-analysis.git
- Open the RStudio project file Peru_COVID_Surveillance.Rproj.
- Install the required R packages by running renv::restore() in the R console.
- Open the Analysis.Rmd file and follow the instructions to run the analysis.
