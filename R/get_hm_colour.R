
# Select Colour Palette ---------------------------------------------------

get_hm_colour <- function(hmcol = NULL) {

  # Use obtained value and return correct colour palette

  if (hmcol == "Warm") {
    col.res <- grDevices::colorRampPalette(
      c("grey92", "#fbedeb", "#FFD6BA", "#F8EA8C", "#E1C340", "#eb5a46", "#ae0000"))(256)
    col.res[1] = "white"
  } else if (hmcol == "Default") {
    col.res = grDevices::colorRampPalette
    (c("#4575B4","#91BFDB","#E0F3F8","#FFFFBF","#FEE090","#FC8D59","#D73027"))(256)
  } else {
    col.res <- get(tolower(hmcol))(256)
  }
  return(col.res)
}
