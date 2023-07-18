# Functions required for this Shiny App -----------------------------------
# Sam Old, 3rd February 2023 ----------------------------------------------


# Check the last row is formatted & apply filter --------------------------
check_plotrow <- function(xs = NULL, sheet = NULL) {

  if (tolower(xs[nrow(xs),1]) == "plot") {

    xs[nrow(xs),1] <- tolower(xs[nrow(xs),1])
    xs <- xs[,tolower(as.character(xs[nrow(xs),])) %in% c("plot", "y")]
    xs <- slice_head(xs, n = (nrow(xs) - 1))

  } else {
    cat("\nLast row does not contain optional plot status. Sheet: ", sheet, "\n")
  }
  return(xs)
}

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


# Round off a table before it is plotted in data table --------------------
# and forced to deal with excels bullshit of floating #s ------------------

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}


create_single_plot <- function(
    tempData        = NULL,
    label           = "",
    jitter_width    = 0.1,
    jotter          = TRUE,
    MFIlogScale     = FALSE,
    colours         = "Set1",
    font.size       = 18,
    pt.size         = 3,
    plot.bar        = FALSE,
    plot.boxplot    = FALSE,
    plot.mean       = FALSE,
    plot.se.of.mean = FALSE,
    axis.text.x     = 45,
    chosen.theme    = "bw") {

  # req(!is.null(tempData))
  error_columns <- NULL;


  # Stop ggplot re-ordering Samples -----------------------------------------
  tempData$Samples <- factor(tempData$Samples, levels = unique(tempData$Samples))


  # Get the party started! --------------------------------------------------

  g_output <- ggplot(
    tempData,
    aes(
      x = Samples,
      y = as.numeric(!!sym(label)),
      fill = Samples)
    )


  # Optional blocks to go behind points -------------------------------------

  if (plot.boxplot) {
    g_output <- g_output +
      geom_boxplot()
  }

  if (plot.bar) {
    g_output <- g_output +
      geom_bar(stat = "summary", fun = mean)
  }


  # ==IF== plot.mean -------------------------------------------------------

  if (plot.mean) {

    data_stats <- summarySE(
      as.data.frame(tempData),
      measurevar = label,
      groupvars = "Samples"
    )

    g_output <- g_output +
      geom_errorbar(
        data = data_stats,
        aes(
          x = Samples,
          ymin = as.numeric(!!sym(label)),
          ymax = as.numeric(!!sym(label))),
        width = 0.35
      )
  }


  # ==IF== plot.mean.and.error ----------------------------------------------

  if (plot.se.of.mean) {

    data_stats <- summarySE(
      as.data.frame(tempData),
      measurevar = label,
      groupvars = "Samples"
    )

    g_output <- g_output +
      geom_errorbar(
        data = data_stats,
        aes(
          x = Samples,
          ymin = as.numeric(!!sym(label)) - se,
          ymax = as.numeric(!!sym(label)) + se),
        width = 0.25
      )
  }

  # ==IF== jotter to jot points ---------------------------------------------


  if (jotter) {
    g_output <- g_output +
      geom_jitter(
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
      ylab("Percentage") +
      scale_y_continuous(
        limits = c(0, NA),
        expand = expansion(
          mult = c(0, .1)))

    label <- paste0("Percentage", substr(label, 2, 1000000L))

  } else if (substr(label, 1, 1) == "N") {

    # Create Number plot

    g_output <- g_output +
      ylab("Number") +
      scale_y_continuous(
        limits = c(0, NA),
        expand = expansion(
          mult = c(0, .1)))

  } else if (substr(label, 1, 1) == "M") {


    # Create an MFI plot
    ## NB::Stopped doing this because scale_y_flowjo_biexp did not work with
    ##   histograms, crashed app and caused massive slowdowns

    # if (MFIlogScale) {
    #
    #   g_output <- g_output +
    #     ylab("MFI (biexponential)") +
    #     scale_y_flowjo_biexp(
    #       limits = c(
    #         min(0, min(as.numeric(tempData[[label]])) * 1.1),
    #         max(as.numeric(tempData[[label]]))*1.1))
    #
    # } else {

    g_output <- g_output +
      ylab("MFI (continuous)") +
      scale_y_continuous(
        limits = c(
          min(0, (min(as.numeric(tempData[[label]])) * 1.1)),
          NA),
        expand = expansion(mult = c(0, .1)))
    # }
  } else {

    # No correct plot found for this column label
    label         <- paste("Title Error:", label)
    error_columns <- c(error_columns, label)
    df            <- data.frame(`x` = rnorm(20), `y` = rnorm(20))
    g_output      <- ggplot(df,aes(x,y)) +
                       geom_blank()
  }

  # Choose fill Palette

  if (colours == "Black & White") {
    g_output <- g_output +
      scale_fill_grey()

  } else {

    # Allow scaling past 8 colours
    cols = colorRampPalette(brewer.pal(8, colours))(length(unique(tempData$Samples)))

    if(length(cols) > 8) {
      g_output <- g_output +
        scale_fill_manual(
          values = cols
        )
    } else {
      g_output <- g_output +
        scale_fill_brewer(palette = colours)
    }
  }


  # Choose theme ------------------------------------------------------------
  # NB::Currently most not reachable in UI

  if(chosen.theme == "bw") {
    g_output <- g_output +
      theme_bw()
  } else if(chosen.theme == "void") {
    g_output <- g_output +
      theme_void()
  } else if(chosen.theme == "light") {
    g_output <- g_output +
      theme_light()
  } else if(chosen.theme == "dark") {
    g_output <- g_output +
      theme_dark()
  } else if(chosen.theme == "minimal") {
    g_output <- g_output +
      theme_minimal()
  } else if(chosen.theme == "classic") {
    g_output <- g_output +
      theme_classic()
  }

  # Consistent layers added -------------------------------------------------

    g_output <- g_output +
    labs(title = label) +
    theme(
      axis.title.y = element_text(size = floor(font.size*3/4)),
      axis.title.x = element_blank(),
      axis.text.x = element_text(
        angle = axis.text.x,
        size = floor(font.size/2),
        vjust = 1, hjust=1),
      plot.title = element_text(size = font.size)
    )

  return(g_output)
}


# Select Colour Palette ---------------------------------------------------

getHMColour <- function(hmcol = NULL) {

  # Use obtained value and return correct colour palette

  if (hmcol == "Warm") {
    col.res <- colorRampPalette(c("grey92", "#fbedeb", "#FFD6BA", "#F8EA8C", "#E1C340", "#eb5a46", "#ae0000"))(256)
    col.res[1] = "white"
  } else if (hmcol == "Default") {
    col.res = colorRampPalette(c("#4575B4","#91BFDB","#E0F3F8","#FFFFBF","#FEE090","#FC8D59","#D73027"))(256)
  } else {
    col.res <- get(tolower(hmcol))(256)
  }
  return(col.res)
}

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
# Citation: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  # library(plyr)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )

  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}


# -------------------------------------------------------------------------
# pptx required functions -------------------------------------------------
# -------------------------------------------------------------------------


create_dml <- function(plot){
  rvg::dml(ggobj = plot)
}

# function to export plot to PowerPoint -----------------------------------
create_pptx <- function(plot, path, left = 0.5, top = 1, width = 9, height = 4.95){

  # Create new Power Point file -------------------------------------------
  if (!file.exists(path)) {
    out <- officer::read_pptx()
  }

  # Append slides to existing file ------------------------------------------
  else {
    out <- officer::read_pptx(path)
  }

  out %>%
    officer::add_slide() %>%
    officer::ph_with(
      plot,
      location = officer::ph_location(
        width = width,
        height = height,
        left = left,
        top = top)
      ) %>%
    base::print(target = path)
}



clean_input_data <- function(dirty_data = NULL) {

  # Change the data format from character columns to integer ----------------
  dirty_data[,-1] <- lapply(dirty_data[,-1], function(x) {
    if(is.character(x)) as.numeric(as.character(x)) else x
  })

  # Factorise `Samples` for plot grouping -----------------------------------
  dirty_data <- dirty_data %>%
    mutate(Samples = factor(gsub("\\_.*", "", Samples)))

  return(dirty_data)
}
