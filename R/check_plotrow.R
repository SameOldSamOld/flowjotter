# Check the last row is formatted & apply filter --------------------------
check_plotrow <- function(xs = NULL, sheet = NULL) {

  if (tolower(xs[nrow(xs),1]) == "plot") {

    xs[nrow(xs),1] <- tolower(xs[nrow(xs),1])
    xs <- xs[,tolower(as.character(xs[nrow(xs),])) %in% c("plot", "y")]
    xs <- dplyr::slice_head(xs, n = (nrow(xs) - 1))

  } else {
    cat("\nLast row does not contain optional plot status. Sheet: ", sheet, "\n")
  }
  return(xs)
}
