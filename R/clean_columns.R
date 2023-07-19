
# Remove and report columns with duplications/error prone symbols ---------
clean_columns <- function(pd = NULL) {

  cn <- colnames(pd)

  # cn <- sub(".*/", "", colnames(pd))
  if (length(cn[duplicated(cn)]) > 0) {
    cat("\n\tDuplicated colnames:   ", cn[duplicated(cn)], "\n")
  }

  # Why did I even have this as an option to start with?
  # if (length(cn[grep("\\/", cn)]) > 0) {
  #   cat('\n\tColnames with a "/":   ', cn[grep("\\/", cn)], "\n")
  # }

  removed <- c(cn[duplicated(cn)])#, cn[grep("\\/", cn)])
  if(length(removed) > 0 ) {
    cat("\nRemoved columns:   ", removed)
  }

  pd <- pd[,cn[!cn %in% removed]]
  return(pd)
}
