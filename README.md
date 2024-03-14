
# COVID-19 Surveillance System Performance: A Secondary Data Source Analysis

[![DOI](https://zenodo.org/badge/772271662.svg)](https://zenodo.org/doi/10.5281/zenodo.10819647)

This repository contains the code used for analyzing secondary data sources related to COVID-19 surveillance in Peru. Using CDC's [Guidelines for Evaluating Surveillance Systems](https://www.cdc.gov/mmwr/preview/mmwrhtml/00001769.htm) framework, the analysis covers various indicators such as data completeness, validity, concordance, and timeliness.

## Data Sources

| Dataset                                                                                                                                      | Institution | Source of the information            | Granularity level            |
|-------------|-------------|---------------------|-------------|
| [COVID-19 deaths](https://www.datosabiertos.gob.pe/dataset/fallecidos-por-covid-19-ministerio-de-salud-minsa)                                | CDC - MINSA | SINANDEF (Vital Registration System) | Patient level (death)        |
| [Molecular tests](https://www.datosabiertos.gob.pe/dataset/dataset-de-pruebas-moleculares-del-instituto-nacional-de-salud-para-covid-19-ins) | INS         | NETLAB (National laboratory system)  | PCR Tests                    |
| [Positive tests](https://www.datosabiertos.gob.pe/dataset/casos-positivos-por-covid-19-ministerio-de-salud-minsa)                            | CDC - MINSA | SISCOVID                             | Positive Tests               |
| [Triage](https://www.datosabiertos.gob.pe/dataset/sospechoso-de-covid-19)                                                                    | CDC - MINSA | SISCOVID (F00 Form)                  | Telephonic triage evaluation |
| [Hospitalized](https://datosabiertos.gob.pe/dataset/hospitalizados-vacunados-y-fallecidos-por-covid-19)                                      | CDC - MINSA | SISCOVID (F500 Form)                 | Hospitalized patient         |

## Getting Started

To run the analysis locally, follow these steps:

-   Clone the repository: git clone <https://github.com/your-repo/covid-19-surveillance-analysis.git>
-   Open the RStudio project file `Peru_COVID_Surveillance.Rproj`.
-   Install the required R packages by running `renv::restore()` in the R console.
-   Open the `Report.qmd` file and follow the instructions to run the analysis.
