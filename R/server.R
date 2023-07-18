
flowjotterServer <- shinyServer(function(input, output, session) {

  # Reactively replace data -------------------------------------------------

  inputData <- reactive({

    # input$file1 will need to be filled with example data initially
    inFile <- input$file1

    if (is.null(inFile)) {
      data <- tibble(
        `Samples`= c("PBS_1.fcs", "PBS_2.fcs", "PBS_3.fcs", "HDM_1.fcs", "HDM_2.fcs", "HDM_3.fcs"),
        `% Example Data` = c(21.8, 17.45, 15, 17.01, 22.99, 21),
        `% TF123+ of total` = c(33.1, 38.2, 41.2, 35.2, 28, 31.9),
        `% TF123- of live` = c(13.9, 10.8, 9.59, 11.2, 16.5, 14.4),

        `No. total TF123+ cells` = c(282, 69, 91, 1700, 5778, 10066),
        `No. CDABC+ TF123+ CDXYZ` = c(74, 16, 26, 565, 1789, 3210),
        `No. total cell/ LN` = c(1.60E+05, 1.36E+05, 1.28E+05, 6.40E+05, 7.20E+05, 1120000),

        `FFI xyz500` = c(12000, 13000, 15000, 1000, 500, -100),
        `MFI XQC360` = c(234234, 456456, 456456, -123, -345, -345),
        `MFI EPC456` = c(561236, 591632, 521456, 750123, 760456, 770789)
      )

    } else {

      # Read in all excel data, and apply plotting filters
      sheets <- readxl::excel_sheets(inFile$datapath)
      data <- read_excel(inFile$datapath, sheet = sheets[1]) %>%
        dplyr::rename(`Samples` = 1) %>%
        select(where(~!all(is.na(.x)))) %>%
        # functionality to remove plots as determined by "plot" row:
        check_plotrow(xs = ., sheet = sheets[1])

      ## Abbie Suggestion: Add 'sheet' ID into column name!
      if (input$append_sheet) {
        colnames(data)[2:ncol(data)] <- paste(colnames(data)[2:ncol(data)], sheets[1])
      }

      # Merge Additional excel sheets for plotting
      if (length(sheets) > 1) {
        for (i in 2:length(sheets)) {

          additional_sheet <- read_excel(path = inFile$datapath, sheet = sheets[i]) %>%
            dplyr::rename(`Samples` = 1) %>%
            select(where(~!all(is.na(.x)))) %>%
            check_plotrow(xs = ., sheet = sheets[i])

          ## Abbie suggestion: Add `sheet` ID into column name!
          if (input$append_sheet) {
            colnames(additional_sheet)[2:ncol(additional_sheet)] <- paste(
              colnames(additional_sheet)[2:ncol(additional_sheet)], sheets[i])
          }

          data <- left_join(data, additional_sheet, by = "Samples")
        }
      }
    }

    # Filter columns with error prone names
    data <- clean_columns(data)

    # Convert all columns bar 'Samples' to numeric ----------------------------

    # Clean out all % columns with '%' and whitespace data in it; thanks Hannah!
    data[,stringr::str_detect(data[1,1:ncol(data)], "%")] <-  as_tibble(
      sapply(
        data[,stringr::str_detect(data[1,1:ncol(data)], "%")],
        function(x) as.numeric(gsub(" ", "", gsub("%", "", x)))))

    numeric_cols       <- colnames(data)[!colnames(data) %in% "Samples"]
    data[numeric_cols] <- as_tibble(sapply(data[numeric_cols], as.numeric))

    return(data)
  })


  # Render Images -----------------------------------------------------------

  output$data_to_header_table <- renderDT({

    table_data <- inputData() %>%
      round_df(digits = 2)

    # Add new hidden columns that decides if a column is plotted as white or red
    hidden_column_n     <- ncol(table_data) - 1
    actual_column_data  <- colnames(table_data)[2:ncol(table_data)]
    hidden_column_index <- (hidden_column_n + 2):(hidden_column_n * 2 + 1)

    for (i in 1:hidden_column_n) {
      actual_index <- actual_column_data[i]
      hidden_index <- hidden_column_index[i]

      # This colours the columns based on correct formatting or not
      table_data[hidden_index] <- substr(actual_index, 0, 1) %in% c("M", "N", "%")
    }

    datatable(
      table_data,
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,

        # Keep the 'l'ength changing input control and 't'able
        dom = 'lt',

        # Change font & background of header
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#303030', 'color': 'white'});",
          "}"),

        # Hide indicator (logical) columns
        columnDefs = list(
          list(
            visible = FALSE,
            targets = hidden_column_index)))) %>%

      # Correct formatting of 'Samples' Column
      formatStyle(
        columns = 'Samples',
        backgroundColor = '#303030',
        color = 'white') %>%

      # Conditional colouring of Columns
      formatStyle(
        columns = actual_column_data,
        valueColumns = hidden_column_index,
        backgroundColor = styleEqual(c(0, 1), c('red', '#222222')),
        color = 'white') %>%
      return
  })
  # , options = list(scrollX = TRUE, sScrollY = '75vh', scrollCollapse = TRUE), extensions = list("Scroller"))

  output$ggAllPlots <- renderPlot({

    # Write all valid columns of PLOT_DATA to ggplot grid ---------------------

    chosen_legend <- input$legend
    ggplotAllData <- inputData()


    # Change the data format from character columns to integer ----------------

    ggplotAllData[,-1] <- lapply(ggplotAllData[,-1], function(x) {
      if(is.character(x)) as.numeric(as.character(x)) else x
    })

    # Factorise `Samples` for plot grouping -----------------------------------
    ggplotAllData <- ggplotAllData %>%
      mutate(Samples = factor(gsub("\\_.*", "", Samples)))


    # Plan grid size ----------------------------------------------------------

    n_plots  <- ncol(ggplotAllData) - 1
    n_width  <- ceiling(sqrt(n_plots))
    n_height <- ceiling(n_plots/n_width)


    # Generating each plot ----------------------------------------------------

    g <- list();

    withProgress(message = 'Building plot', value = 0, {

      for (i in 2:ncol(ggplotAllData)) {

        plot_data_temp <- ggplotAllData[,c(1, i)]
        label          <- colnames(plot_data_temp)[2]
        plot_n         <- i-1

        incProgress( 1 / (ncol(ggplotAllData) + 1), detail = paste("Plotting", label))

        g[[plot_n]] <- create_single_plot(
          tempData        = plot_data_temp,
          label           = label,
          jitter_width    = ifelse(input$jitter, 0.1, 0),
          jotter          = input$jotter,
          MFIlogScale     = input$MFI_logscale,
          colours         = input$colour_palette_picker,
          font.size       = input$font_size,
          pt.size         = input$pt_size,
          plot.boxplot    = input$plot_boxplot,
          plot.bar        = input$plot_bar,
          plot.mean       = input$plot_mean,
          plot.se.of.mean = input$plot_se_of_the_mean,
          axis.text.x     = as.numeric(input$xlab_angle),
          chosen.theme    = input$ggtheme)
      }

      incProgress( 1 / (ncol(ggplotAllData) + 1), detail = "Compiling Full Image")

      gg_fin <- ggarrange(
        plotlist      = g,
        ncol          = n_height,
        nrow          = n_width,
        common.legend = TRUE,
        legend        = chosen_legend) +
        bgcolor("#262626")

      incProgress( 1 / (ncol(ggplotAllData) + 1), detail = "Done")
    })

    return(gg_fin)

  }, height = 800)#, height = 800*n_width/n_height)

  output$ggSinglePlot <- renderPlot({

    # Write all valid columns of PLOT_DATA to ggplot grid ---------------------

    req(input$choose_graph)

    ggplotAllData <- clean_input_data(inputData())
    chosen_legend <- input$legend
    selected_plot <- input$choose_graph


   # Generate chosen image ---------------------------------------------------

    plot_data_temp <- dplyr::select(ggplotAllData, c(`Samples`, all_of(selected_plot)))

    return(
      create_single_plot(
        tempData        = plot_data_temp,
        label           = selected_plot,
        jitter_width    = ifelse(input$jitter, 0.1, 0),
        jotter          = input$jotter,
        # MFIlogScale     = input$MFI_logscale,
        colours         = input$colour_palette_picker,
        font.size       = input$font_size,
        pt.size         = input$pt_size,
        plot.boxplot    = input$plot_boxplot,
        plot.bar        = input$plot_bar,
        plot.mean       = input$plot_mean,
        plot.se.of.mean = input$plot_se_of_the_mean,
        axis.text.x     = as.numeric(input$xlab_angle),
        chosen.theme    = input$ggtheme) +
        theme(legend.position = input$legend))
  },
  width  = reactive(input$fig_width),
  height = reactive(input$fig_height))


  # UI Elements -------------------------------------------------------------

  output$graphSelectControls <- renderUI ({

    choice_names <- colnames(inputData())
    choice_names <- choice_names[2:length(choice_names)]

    tagList(
      fluidRow(
        column(
          width = 6,
          selectInput(
            inputId  = "choose_graph",
            label    = "Selected Plot",
            choices  = choice_names,
            selected = choice_names[1],
            multiple = FALSE)),
        column(
          width = 6,
          downloadButton(
            outputId = "download_single_image",
            label    = "Download Selected Image"))
      ),

      plotOutput(
        "ggSinglePlot",
        # height = 950
        width  = (input$fig_width),
        height = (input$fig_height)),
    )
  })


  # Download sections -------------------------------------------------------

  output$download_master_image <- downloadHandler(
    filename = function() {

      inFile <- input$file1

      if(is.null(inFile)) {
        myfile <- "example_data.pdf"
      } else {
        myfile <- paste0(tools::file_path_sans_ext(inFile$name), '.pdf')
      }
      myfile

    },
    content = function(file) {

      ggplotAllData <- clean_input_data(inputData())

      # Plan grid size ----------------------------------------------------------

      # n_plots  <- ncol(ggplotAllData) - 1
      n_width  <- ceiling(sqrt((ncol(ggplotAllData) - 1)))
      n_height <- ceiling((ncol(ggplotAllData) - 1)/n_width)


      # Generating each plot ----------------------------------------------------

      g <- list();

        for (i in 2:ncol(ggplotAllData)) {

          plot_data_temp <- ggplotAllData[,c(1, i)]
          label          <- colnames(plot_data_temp)[2]
          plot_n         <- i-1

          g[[plot_n]] <- create_single_plot(
            tempData        = plot_data_temp,
            label           = label,
            jitter_width    = ifelse(input$jitter, 0.1, 0),
            jotter          = input$jotter,
            MFIlogScale     = input$MFI_logscale,
            colours         = input$colour_palette_picker,
            font.size       = input$font_size,
            pt.size         = input$pt_size,
            plot.boxplot    = input$plot_boxplot,
            plot.bar        = input$plot_bar,
            plot.mean       = input$plot_mean,
            plot.se.of.mean = input$plot_se_of_the_mean,
            axis.text.x     = as.numeric(input$xlab_angle),
            chosen.theme    = input$ggtheme)
        }

        gg_fin <- ggarrange(
          plotlist      = g,
          ncol          = n_height,
          nrow          = n_width,
          common.legend = TRUE,
          legend        = input$legend) +
          bgcolor("#262626")

        ggsave(
          file,
          plot      = gg_fin,
          device    = "pdf",
          width     = input$fig_width*n_width,
          height    = input$fig_height*n_height,
          limitsize = FALSE,
          units     = "px",
          dpi       = 72)
    }
  )

  output$download_single_image <- downloadHandler(
    filename = function() {

      inFile <- input$file1

      if(is.null(inFile)) {
        myfile <- paste0("example_data_",
                         input$choose_graph, ".pdf")
      } else {
        myfile <- paste0(tools::file_path_sans_ext(inFile$name), '_',
                         input$choose_graph, '.pdf')
      }
      myfile
    },
    content = function(file) {

      ggplotAllData <- clean_input_data(inputData())
      tempData      <- ggplotAllData[,c("Samples", input$choose_graph)]

      g_single <- create_single_plot(
        tempData        = tempData,
        label           = input$choose_graph,
        jitter_width    = ifelse(input$jitter, 0.1, 0),
        jotter          = input$jotter,
        MFIlogScale     = input$MFI_logscale,
        colours         = input$colour_palette_picker,
        font.size       = input$font_size,
        pt.size         = input$pt_size,
        plot.boxplot    = input$plot_boxplot,
        plot.bar        = input$plot_bar,
        plot.mean       = input$plot_mean,
        plot.se.of.mean = input$plot_se_of_the_mean,
        axis.text.x     = as.numeric(input$xlab_angle),
        chosen.theme    = input$ggtheme)

      ggsave(
        file,
        plot      = g_single,
        device    = "pdf",
        width     = input$fig_width,
        height    = input$fig_height,
        limitsize = FALSE,
        units     = "px",
        dpi       = 72)

    }
  )

  output$download_prism <- downloadHandler(
    filename = function() {

      inFile <- input$file1

      if(is.null(inFile)) {
        myfile <- "example_data.pzfx"
      } else {
        myfile <- paste0(tools::file_path_sans_ext(inFile$name), '.pzfx')
      }
      myfile
    },
    content = function(file) {

      data   <- inputData()
      groups <- gsub("\\_.*", "", data$Samples)

      # Correctly format data for prism download ----------------------------

      prism_list <- list();
      for (uniq_coln in colnames(data)[2:ncol(data)]) {

        temp_dat <- matrix(
          data = NA,
          ncol = length(unique(groups)),
          nrow = max(table(groups)),
          dimnames = list(
            c(paste0("Rep", 1:max(table(groups)))),
            unique(groups)
            )
          )

        for (i in 1:nrow(data)){

          value <- as.numeric(data[i, uniq_coln])
          temp_col <- groups[i]

          for (j in 1:nrow(temp_dat)){

            if(is.na(temp_dat[j,temp_col])) {
              temp_dat[j, temp_col] <- value
              value <- NA
            }
          }
        }
        # ??dont?? Transpose for replicate rows
        # potentially an option... people can probably deal tho
        if (input$transpose_prism) {
          temp_dat <- t(temp_dat)
        }
        prism_list[[uniq_coln]] <- temp_dat
      }

      temp_file <- tempfile()
      write_pzfx(
        x = prism_list,
        path = temp_file,
        row_names = TRUE
      )

      file.copy(temp_file, file)
    }
  )

  output$download_pptx <- downloadHandler(
    filename = function() {

      inFile <- input$file1

      if(is.null(inFile)) {
        myfile <- "example_data.pptx"
      } else {
        myfile <- paste0(tools::file_path_sans_ext(inFile$name), '.pptx')
      }
      myfile

    },
    content = function(file) {

      # Generating each plot ----------------------------------------------------

      ggplotAllData <- clean_input_data(inputData())
      g <- list();

      withProgress(
        message = 'Downloading .pptx',
        value = 0,
        min = 0,
        max = (ncol(ggplotAllData) + 1), {

        for (i in 2:ncol(ggplotAllData)) {

          plot_data_temp <- ggplotAllData[,c(1, i)]
          label          <- colnames(plot_data_temp)[2]
          plot_n         <- i-1

          incProgress(
            amount = 1,
            message = "Generating Plots",
            detail = label)

          g[[plot_n]] <- create_single_plot(
            tempData        = plot_data_temp,
            label           = label,
            jitter_width    = ifelse(input$jitter, 0.1, 0),
            jotter          = input$jotter,
            MFIlogScale     = input$MFI_logscale,
            colours         = input$colour_palette_picker,
            font.size       = input$font_size,
            pt.size         = input$pt_size,
            plot.boxplot    = input$plot_boxplot,
            plot.bar        = input$plot_bar,
            plot.mean       = input$plot_mean,
            plot.se.of.mean = input$plot_se_of_the_mean,
            axis.text.x     = as.numeric(input$xlab_angle),
            chosen.theme    = input$ggtheme)
        }

          incProgress(
            amount = 1,
            message = "Saving Plots to .pptx...",
            detail = "~1 second per image")

        g_dml <- purrr::map(g, create_dml)

        temp_file <- paste0(tempfile(), ".pptx")

        purrr::map(
          g_dml,
          create_pptx,
          path = temp_file)
      })

      file.copy(temp_file, file)
    }
  )

  output$estimated_master_image_download_time <- renderText({
    inputData() %>%
      clean_input_data %>%
      ncol %>%
      magrittr::divide_by(6) %>%
      round(digits = 1) %>%
      paste0("s") %>%
      return
  })

  output$estimated_pptx_format_download_time <- renderText({
    inputData() %>%
      clean_input_data %>%
      ncol %>%
      magrittr::divide_by(1.8) %>%
      round(digits = 1) %>%
      paste0("s") %>%
      return
  })

  output$estimated_prism_format_download_time <- renderText({
    inputData() %>%
      clean_input_data %>%
      ncol %>%
      magrittr::divide_by(50) %>%
      round(digits = 1) %>%
      paste0("s") %>%
      return
  })


})
