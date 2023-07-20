# -------------------------------------------------------------------------
# pptx required function --------------------------------------------------
# -------------------------------------------------------------------------

# function to export plot to PowerPoint -----------------------------------
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
        width = width,
        height = height,
        left = left,
        top = top
      )
    ) |>
    base::print(target = path)
}
