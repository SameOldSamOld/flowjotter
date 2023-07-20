#' ggplot to pptx conversion function
#'
#' Exports final ggplot images to single page pptx files. Could be sped up,
#'   takes approximately 1 second per image generated. Required for pptx.
#'
#' @param plot ggplot2 output
#' @param path tempfile location to save pptx file
#' @param left left coordinate of the bounding box
#' @param top top coordinate of the bounding box
#' @param width width of the bounding box
#' @param height height of the bounding box
#'
#' @examples
#' # example code
#' create_dml <- function(plot) {
#'   rvg::dml(ggobj = plot)
#' }
#' g <- list()
#' g[[1]]    <- ggplot2::ggplot(data.frame(1),
#'                ggplot2::aes(x = 1, y = 1)) +
#'                ggplot2::geom_point()
#' g_dml     <- purrr::map(g, create_dml)
#' temp_file <- paste0(tempfile(), ".pptx")
#'
#' purrr::map(
#'   g_dml,
#'   flowjotter::create_pptx,
#'   path = temp_file
#' )
#' @export
create_pptx <- function(plot, path, left = 0.5, top = 1, width = 9, height = 4.95) {
  # Create new Power Point file -------------------------------------------
  if (!file.exists(path)) {
    out <- officer::read_pptx()
  }

  # Append slides to existing file ------------------------------------------
  else {
    out <- officer::read_pptx(path)
  }

  out |>
    officer::add_slide() |>
    officer::ph_with(
      plot,
      location = officer::ph_location(
        width  = width,
        height = height,
        left   = left,
        top    = top
      )
    ) |>
    base::print(target = path)
}
