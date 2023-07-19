clean_input_data <- function(dirty_data = NULL) {

  # Change the data format from character columns to integer ----------------
  dirty_data[,-1] <- lapply(dirty_data[,-1], function(x) {
    if(is.character(x)) as.numeric(as.character(x)) else x
  })

  # Factorise `Samples` for plot grouping -----------------------------------
  dirty_data <- dirty_data |>
    plyr::mutate(Samples = factor(gsub("\\_.*", "", Samples)))

  return(dirty_data)
}
