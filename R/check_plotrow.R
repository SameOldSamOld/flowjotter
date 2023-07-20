#' Keep or Remove Columns
#'
#' Check if the final row of `xs` contains optional "plot" filter, and apply specified filters
#'   if so. The final row (as labelled "plot" in the "Samples" column) should contains "y" or "n"
#'   values to dictate if the associated column should be plotted or not.
#'
#' @param xs "tbl_df" "tbl" "data.frame" format from readxl::read_excel() reading an excel
#'   spreadsheet
#' @param sheet a character string provided by `readxl::excel_sheets()` for the excel file xs
#' @returns
#' * `check_plotrow` returns a tabular format similar to xs input `"tbl_df" "tbl" "data.frame"`
#' * `check_plotrow` removes the final row `xs[tolower("plot"),]` if it existed, and specified
#'   columns to not be plotted.
#'
#' @examples
#' # Load flowjotter's example dataset that is loaded in app by default
#' data(flowjotter_example)
#'
#' # Ensure the first columns is titled "Samples"
#' flowjotter_example <- dplyr::rename(flowjotter_example, `Samples` = 1)
#'
#' # Remove empty columns
#' flowjotter_example <- dplyr::select(flowjotter_example, dplyr::where(~ !all(is.na(.x))))
#'
#' # Use check_plotrow to filter on last row.
#' flowjotter_example <- check_plotrow(xs = flowjotter_example, sheet = "CD4")
#' @export
check_plotrow <- function(xs = NULL, sheet = NULL) {

  # Hidden feature: If a final row with first value "plot" is added,
  #   you can filter which columns are plotted.
  if (tolower(xs[nrow(xs), 1]) == "plot") {
    xs[nrow(xs), 1] <- tolower(xs[nrow(xs), 1])
    xs <- xs[, tolower(as.character(xs[nrow(xs), ])) %in% c("plot", "y")]
    xs <- dplyr::slice_head(xs, n = (nrow(xs) - 1))
  } # else {
    # cat("\nLast row does not contain optional plot status. Sheet: ", sheet, "\n")
  # }
  return(xs)
}
