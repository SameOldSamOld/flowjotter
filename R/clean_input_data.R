clean_input_data <- function(dirty_data = NULL, samples_var = "Samples") {

  # Change the data format from character columns to integer ----------------
  dirty_data[, -1] <- lapply(dirty_data[, -1], function(x) {
    if (is.character(x)) as.numeric(as.character(x)) else x
  })

  # Factorise `Samples` for plot grouping -----------------------------------
  # dirty_data <- plyr::mutate(dirty_data, `Samples` = factor(gsub("\\_.*", "", `samples_var`)))
  dirty_data[[samples_var]] <- factor(gsub("\\_.*", "", dirty_data[[samples_var]]))

  return(dirty_data)
}
