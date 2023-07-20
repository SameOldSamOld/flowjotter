create_single_plot <- function(
    tempData = NULL,
    label = "",
    jitter_width = 0.1,
    jotter = TRUE,
    MFIlogScale = FALSE,
    colours = "Set1",
    font.size = 18,
    pt.size = 3,
    plot.bar = FALSE,
    plot.boxplot = FALSE,
    plot.mean = FALSE,
    plot.se.of.mean = FALSE,
    axis.text.x = 45,
    chosen.theme = "bw",
    samples_var = "Samples",
    se_var = "se") {
  # shiny::req(!is.null(tempData))
  error_columns <- NULL


  # Stop ggplot re-ordering Samples -----------------------------------------
  tempData$Samples <- factor(tempData$Samples, levels = unique(tempData$Samples))

  # samples_var <- "Samples"

  # Get the party started! --------------------------------------------------

  g_output <- ggplot2::ggplot(
    tempData,
    ggplot2::aes(
      x = !!ggplot2::sym(samples_var),
      # x = !!samples_var,
      y = as.numeric(!!ggplot2::sym(label)),
      fill = !!ggplot2::sym(samples_var)
    )
  )

  # Optional blocks to go behind points -------------------------------------

  if (plot.boxplot) {
    g_output <- g_output +
      ggplot2::geom_boxplot()
  }

  if (plot.bar) {
    g_output <- g_output +
      ggplot2::geom_bar(stat = "summary", fun = mean)
  }


  # ==IF== plot.mean -------------------------------------------------------

  if (plot.mean) {
    data_stats <- summary_se(
      as.data.frame(tempData),
      measurevar = label,
      groupvars = "Samples"
    )

    g_output <- g_output +
      ggplot2::geom_errorbar(
        data = data_stats,
        ggplot2::aes(
          x = !!ggplot2::sym(samples_var),
          ymin = as.numeric(!!ggplot2::sym(label)),
          ymax = as.numeric(!!ggplot2::sym(label))
        ),
        width = 0.35
      )
  }


  # ==IF== plot.mean.and.error ----------------------------------------------

  if (plot.se.of.mean) {
    data_stats <- summary_se(
      as.data.frame(tempData),
      measurevar = label,
      groupvars = "Samples"
    )

    g_output <- g_output +
      ggplot2::geom_errorbar(
        data = data_stats,
        ggplot2::aes(
          x = !!ggplot2::sym(samples_var),
          ymin = as.numeric(!!ggplot2::sym(label)) - !!ggplot2::sym(se_var),
          ymax = as.numeric(!!ggplot2::sym(label)) + !!ggplot2::sym(se_var)
        ),
        width = 0.25
      )
  }

  # ==IF== jotter to jot points ---------------------------------------------


  if (jotter) {
    g_output <- g_output +
      ggplot2::geom_jitter(
        colour = "black",
        pch = 21,
        height = 0,
        width = jitter_width,
        size = pt.size,
        alpha = 0.8
      )
  }


  # Add custom plot labels --------------------------------------------------

  if (substr(label, 1, 1) == "%") {
    # Create Percentage plot

    g_output <- g_output +
      ggplot2::ylab("Percentage") +
      ggplot2::scale_y_continuous(
        limits = c(0, NA),
        expand = ggplot2::expansion(mult = c(0, .1))
      )

    label <- paste0("Percentage", substr(label, 2, 1000000L))
  } else if (substr(label, 1, 1) == "N") {
    # Create Number plot

    g_output <- g_output +
      ggplot2::ylab("Number") +
      ggplot2::scale_y_continuous(
        limits = c(0, NA),
        expand = ggplot2::expansion(mult = c(0, .1))
      )
  } else if (substr(label, 1, 1) == "M") {
    # Create an MFI plot
    ## NB::Stopped doing this because scale_y_flowjo_biexp did not work with
    ##   histograms, crashed app and caused massive slowdowns

    # if (MFIlogScale) {
    #
    #   g_output <- g_output +
    #     ylab("MFI (biexponential)") +
    #     ggcyto::scale_y_flowjo_biexp(
    #       limits = c(
    #         min(0, min(as.numeric(tempData[[label]])) * 1.1),
    #         max(as.numeric(tempData[[label]]))*1.1))
    #
    # } else {

    g_output <- g_output +
      ggplot2::ylab("MFI (continuous)") +
      ggplot2::scale_y_continuous(
        limits = c(
          min(0, (min(as.numeric(tempData[[label]])) * 1.1)),
          NA
        ),
        expand = ggplot2::expansion(mult = c(0, .1))
      )
    # }
  } else {
    # No correct plot found for this column label
    label <- paste("Title Error:", label)
    error_columns <- c(error_columns, label)
    g_output <- ggplot2::ggplot(
      data.frame(1),
      ggplot2::aes(x=1,y=1)) +
      ggplot2::geom_blank() +
      ggplot2::theme_void()
  }

  # Choose fill Palette

  if (colours == "Black & White") {
    g_output <- g_output +
      ggplot2::scale_fill_grey()
  } else {
    # Allow scaling past 8 colours
    cols <- grDevices::colorRampPalette(
      RColorBrewer::brewer.pal(8, colours)
    )(length(unique(tempData$Samples)))

    if (length(cols) > 8) {
      g_output <- g_output +
        ggplot2::scale_fill_manual(
          values = cols
        )
    } else {
      g_output <- g_output +
        ggplot2::scale_fill_brewer(palette = colours)
    }
  }


  # Choose theme ------------------------------------------------------------
  # NB::Currently most not reachable in UI

  if (chosen.theme == "bw") {
    g_output <- g_output +
      ggplot2::theme_bw()
  } else if (chosen.theme == "void") {
    g_output <- g_output +
      ggplot2::theme_void()
  } else if (chosen.theme == "light") {
    g_output <- g_output +
      ggplot2::theme_light()
  } else if (chosen.theme == "dark") {
    g_output <- g_output +
      ggplot2::theme_dark()
  } else if (chosen.theme == "minimal") {
    g_output <- g_output +
      ggplot2::theme_minimal()
  } else if (chosen.theme == "classic") {
    g_output <- g_output +
      ggplot2::theme_classic()
  }

  # Consistent layers added -------------------------------------------------

  g_output <- g_output +
    ggplot2::labs(title = label) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(size = floor(font.size * 3 / 4)),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = axis.text.x,
        size = floor(font.size / 2),
        vjust = 1, hjust = 1
      ),
      plot.title = ggplot2::element_text(size = font.size)
    )

  return(g_output)
}
