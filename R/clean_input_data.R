#' Ensure correct dataframe class conversions before plotting from input excel files.
#'
#' Ensures data columns are stored as numeric values, and first column `Samples`
#'  is correctly split by an underscore and factorised.
#' @param dirty_data input excel file from readxl::read_excel after column filtering has been
#'  applied. First column contains groups that will be split and factored by underscore:
#'  `factor(gsub("\\_.*", "", dirty_data[,1]))`. Other columns will contain values converted to numeric.
#'  Other columns cannot contain ascii characters such as letters or '%' characters.
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
#' # Use check_plotrow to apply filters from the last row if they exist
#' flowjotter_example <- check_plotrow(xs = flowjotter_example, sheet = "CD4")
#'
#' # Use clean_input_data to prepare for plotting, and grouping samples.
#' flowjotter_example <- clean_input_data(flowjotter_example)
#' @export
clean_input_data <- function(dirty_data = NULL) {

  samples_var <- colnames(dirty_data)[1]

  # Change the data format from character columns to integer ----------------
  dirty_data[, -1] <- lapply(dirty_data[, -1], function(x) {
    if (is.character(x)) as.numeric(as.character(x)) else x
  })

  # Factorise `Samples` for plot grouping -----------------------------------
  # dirty_data <- plyr::mutate(dirty_data, `Samples` = factor(gsub("\\_.*", "", `samples_var`)))
  dirty_data[[samples_var]] <- factor(gsub("\\_.*", "", dirty_data[[samples_var]]))

  return(dirty_data)
}
