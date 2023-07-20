
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flowjotter

<!-- badges: start -->
<!-- badges: end -->

The objective of flowjotter is to be a time-saving tool for researchers
working with High Dimensional Flow Cytometry datasets.

## Installation

You can install the development version of flowjotter from
[GitHub](https://github.com/) with:

    if(!require(devtools)) {
      install.packages("devtools")
    } 
    devtools::install_github("SameOldSamOld/flowjotter")

## Usage

This is a basic example which shows you how to use the shiny app:

    ## Load Package
    library(flowjotter)

    ## Run shiny app
    flowjotter()
