
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flowjotter

<!-- badges: start -->
<!-- badges: end -->

The objective of flowjotter is to be a time-saving tool for researchers
working with High Dimensional Flow Cytometry data.

Flowjotter is useful for quick, automated plotting of Cytometry data
copied from the FlowJo data export table into excel files.

Flowjotter also features extremely fast conversion to .pzfx Prism file
format for users who would prefer to make their graphs or stats in
Prism.

## Installation

You can install the development version of flowjotter from
[GitHub](https://github.com/) with:

    if(!require(devtools)) {
      install.packages("devtools")
    } 
    devtools::install_github("SameOldSamOld/flowjotter")

## Running flowjotter

This is a basic example which shows you how to use the shiny app:

    ## Load Package
    library(flowjotter)

    ## Run shiny app
    flowjotter()

## Usage

### Choosing Graph types

Graph type is decided by the first character of the column title

% = Percentage Graph

N = Number Graph

M = MFI Plot

Red columns highlight columns not starting with `%/N/M`

Column names must be unique

### Choosing Groups

The left-most column (‘Samples’) dictates groups for samples

Everything before the first underscore becomes grouped

There is no limit on the number of groups

## FAQ

### Is there an example of how to setup an Excel file?

A typical Excel counts layout can be read in /data/ with base::load()

### Can my excel file contain multiple sheets?

Yes, but each sheet must have a consistent and viable column name, group
name and data.

No:

- If you are changing the groups that are to be plotted in each sheet,
  it will not work.

- Groups must stay the same throughout the excel sheet e.g. HDM vs Nb vs
  Ca

- To perform multiple comparisons e.g. (HDM vs Nb) & (HDM vs Ca) & (Nb
  vs Ca), you will need to create multiple excel files

  - If multiple sheets are encountered, you may reach a warning if not
    correctly formatted.

### Why are my images so squished?

You need to increase the ‘Height px’ of your image

### My groups are getting oversplit into multiple groups

Carefully read the figure legend, there may be a tiny typo
