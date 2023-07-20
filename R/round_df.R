# Round off a table before it is plotted in data table --------------------
# and forced to deal with excels bullshit of floating #s ------------------

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[, nums] <- round(df[, nums], digits = digits)

  (df)
}
