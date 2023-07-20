#' flowjotter example data
#'
#' A test dataset created by Sam Old to test various plotting functions and features
#' of the flowjottter app.
#'
#' @docType data
#'
#' @usage data(flowjotter_example)
#'
#' @keywords datasets
#'
#' @format An excel spreadsheet with the first column specifying groups with an
#'  identical factor before an underscore which then can contain unique identifiers
#'  useful for the user. There can be multiple sheets, and as many columns as you'd
#'  like to include. Empty columns will be removed. Optionally you can add a final
#'  row with a label in the `Samples` column called `plot`. The `plot` row uses y/n
#'  values as a binary option to decide if columns should be skipped for plotting.
#'  This may be useful for researchers who use intermediary columns in excel to
#'  calculate percentages, etc.
#'
#' @examples
#' data(flowjotter_example)
"flowjotter_example"
