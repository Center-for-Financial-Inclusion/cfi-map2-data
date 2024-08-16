# cfi-map2-data
Analysis of MAP2 demand-side surveys from five cities in Indonesia, India, Ethiopia, Nigeria and Brazil.

**Description**

This repository contains all of the data and code for the set of analyses and figures for MAP2.

**Data and Code Availability Statement**

The primary data sets used in this analysis were produced by Multicultural Insights who coordinated the fieldwork and data collection in each of the five countries. 

**Computational requirements**

This analysis was conducted using R/version 4.3.2 (2023-03-31) and RStudio/version 2023.03.0+386 (2023.03.0+386). 

R version 4.3.2 (2023-10-31)
Platform: x86_64-apple-darwin20 (64-bit)
Running under: macOS Sonoma 14.2.1

**Instructions for Data Preparation and Analysis**

The full workflow for data downloading, cleaning, analysis and visuazliation of results is set out in the file: *cfi_map2.qmd*. This is a quarto file which when run, generates as output the files in the docs folder. The individual functions which perform the data downloading, data cleaning, data analysis and data visualization tasks are in the R subfolder. 

**Outputs**

All outputs used for the paper can be found in the files generated automatically when running the main .qmd file. 
