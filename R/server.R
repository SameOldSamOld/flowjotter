flowjotter_server <- shiny::shinyServer(function(input, output, session) {

  samples_var <- "Samples"

  # Reactively replace data -------------------------------------------------

  inputData <- shiny::reactive({
    # input$file1 will need to be filled with example data initially
    inFile <- input$file1
    errorC <- ""

    if (is.null(inFile)) {
      data <- dplyr::tibble(
        `Samples` = c("PBS_1.fcs", "PBS_2.fcs", "PBS_3.fcs", "HDM_1.fcs", "HDM_2.fcs", "HDM_3.fcs"),
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
      data   <- readxl::read_excel(inFile$datapath, sheet = sheets[1])
      data   <- dplyr::rename(data, `Samples` = 1)
      data   <- dplyr::select(data, dplyr::where(~ !all(is.na(.x))))
      # functionality to remove plots as determined by "plot" row
      data   <- check_plotrow(xs = data, sheet = sheets[1])

      ## Abbie Suggestion: Add 'sheet' ID into column name!
      if (input$append_sheet) {
        colnames(data)[2:ncol(data)] <- paste(colnames(data)[2:ncol(data)], sheets[1])
      }

      # Merge Additional excel sheets for plotting
      if (length(sheets) > 1) {
        for (i in 2:length(sheets)) {
          additional_sheet <- readxl::read_excel(path = inFile$datapath, sheet = sheets[i])
          additional_sheet <- dplyr::mutate(additional_sheet, `empty_sheet_fix` = NA)
          additional_sheet <- dplyr::rename(additional_sheet, `Samples` = 1)

          samplesA <- gsub("\\_.*", "", data$Samples)
          samplesB <- gsub("\\_.*", "", additional_sheet$Samples)

          # Check to ensure Samples columns match for new sheets
          if (identical(samplesA, samplesB)) {
            additional_sheet <- dplyr::select(additional_sheet, dplyr::where(~ !all(is.na(.x))))
            additional_sheet <- check_plotrow(xs = additional_sheet, sheet = sheets[i])

            ## Abbie suggestion: Add `sheet` ID into column name!
            if (input$append_sheet) {
              colnames(additional_sheet)[2:ncol(additional_sheet)] <- paste(
                colnames(additional_sheet)[2:ncol(additional_sheet)], sheets[i]
              )
            }
            data <- dplyr::left_join(data, additional_sheet, by = "Samples")
          } else {
            print("Warning: Samples did not match between all excel sheets.")
            errorC <- "WARNING: Samples did not match between all excel sheets."
          }
        }
      }
    }

    # Filter columns with error prone names
    data <- clean_columns(data)

    # Convert all columns bar 'Samples' to numeric ----------------------------

    # Clean out all % columns with '%' and whitespace data in it; thanks Hannah!
    data[, stringr::str_detect(data[1, 1:ncol(data)], "%")] <- dplyr::as_tibble(
      sapply(
        data[, stringr::str_detect(data[1, 1:ncol(data)], "%")],
        function(x) as.numeric(gsub(" ", "", gsub("%", "", x)))
      )
    )

    numeric_cols <- colnames(data)[!colnames(data) %in% "Samples"]
    data[numeric_cols] <- dplyr::as_tibble(sapply(data[numeric_cols], as.numeric))

    return(list(data = data, errorC = errorC))
  })


  # Render Images -----------------------------------------------------------

  output$data_to_header_table <- DT::renderDT({
    table_data <- inputData()[["data"]] |>
      round_df(digits = 2)

    # Add new hidden columns that decides if a column is plotted as white or red
    hidden_column_n <- ncol(table_data) - 1
    actual_column_data <- colnames(table_data)[2:ncol(table_data)]
    hidden_column_index <- (hidden_column_n + 2):(hidden_column_n * 2 + 1)

    for (i in 1:hidden_column_n) {
      actual_index <- actual_column_data[i]
      hidden_index <- hidden_column_index[i]

      # This colours the columns based on correct formatting or not
      table_data[hidden_index] <- substr(actual_index, 0, 1) %in% c("M", "N", "%")
    }

    DT::datatable(
      table_data,
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,

        # Keep the 'l'ength changing input control and 't'able
        dom = "lt",

        # Change font & background of header
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#303030', 'color': 'white'});",
          "}"
        ),

        # Hide indicator (logical) columns
        columnDefs = list(
          list(
            visible = FALSE,
            targets = hidden_column_index
          )
        )
      )
    ) |>
      # Correct formatting of 'Samples' Column
      DT::formatStyle(
        columns = "Samples",
        backgroundColor = "#303030",
        color = "white"
      ) |>
      # Conditional colouring of Columns
      DT::formatStyle(
        columns = actual_column_data,
        valueColumns = hidden_column_index,
        backgroundColor = DT::styleEqual(c(0, 1), c("red", "#222222")),
        color = "white"
      ) #|>
    # return()
  })
  # , options = list(scrollX = TRUE, sScrollY = '75vh', scrollCollapse = TRUE), extensions = list("Scroller"))


  ## ggAllPlots is no longer called. I did fix it up for error catching though.
  # output$ggAllPlots <- shiny::renderPlot(
  #   {
  #     # Write all valid columns of PLOT_DATA to ggplot grid ---------------------
  #
  #     chosen_legend <- input$legend
  #     input_data    <- inputData()
  #     ggplotAllData <- input_data[["data"]]
  #     ggplotErrors  <- input_data[["errorC"]]
  #     g             <- list()
  #
  #
  #     # Change the data format from character columns to integer ----------------
  #
  #     ggplotAllData[, -1] <- lapply(ggplotAllData[, -1], function(x) {
  #       if (is.character(x)) as.numeric(as.character(x)) else x
  #     })
  #
  #     # Factorise `Samples` for plot grouping -----------------------------------
  #     ggplotAllData <- plyr::mutate(
  #       ggplotAllData,
  #       Samples = factor(gsub("\\_.*", "",  ggplotAllData[[samples_var]])))
  #
  #
  #     # Plan grid size ----------------------------------------------------------
  #
  #     n_plots <- ncol(ggplotAllData) - 1
  #     if (!ggplotErrors == "") {
  #       n_plots <- n_plots + 1
  #
  #       # Create empty plot that shows the error caught
  #       g[[n_plots]] <- ggplot2::ggplot(
  #         data.frame(1),
  #         aes(
  #           x = 1,
  #           y = 1,
  #           label = !!ggplotErrors)) +
  #         ggplot2::geom_text() +
  #         ggplot2::theme_void()
  #     }
  #     n_width <- ceiling(sqrt(n_plots))
  #     n_height <- ceiling(n_plots / n_width)
  #
  #
  #     # Generating each plot ----------------------------------------------------
  #
  #     shiny::withProgress(message = "Building plot", value = 0, {
  #       for (i in 2:ncol(ggplotAllData)) {
  #         plot_data_temp <- ggplotAllData[, c(1, i)]
  #         label <- colnames(plot_data_temp)[2]
  #         plot_n <- i - 1
  #
  #         shiny::incProgress(1 / (ncol(ggplotAllData) + 1), detail = paste("Plotting", label))
  #
  #         g[[plot_n]] <- fj_plot(
  #           tempData        = plot_data_temp,
  #           # label           = label,
  #           jitter_width    = ifelse(input$jitter, 0.1, 0),
  #           jotter          = input$jotter,
  #           # MFIlogScale     = input$MFI_logscale,
  #           colours         = input$colour_palette_picker,
  #           font.size       = input$font_size,
  #           pt.size         = input$pt_size,
  #           plot.boxplot    = input$plot_boxplot,
  #           plot.bar        = input$plot_bar,
  #           plot.mean       = input$plot_mean,
  #           plot.se.of.mean = input$plot_se_of_the_mean,
  #           axis.text.x     = as.numeric(input$xlab_angle),
  #           chosen.theme    = input$ggtheme
  #         )
  #       }
  #
  #       shiny::incProgress(1 / (ncol(ggplotAllData) + 1), detail = "Compiling Full Image")
  #
  #       gg_fin <- ggpubr::ggarrange(
  #         plotlist      = g,
  #         ncol          = n_height,
  #         nrow          = n_width,
  #         common.legend = TRUE,
  #         legend        = chosen_legend
  #       ) +
  #         ggpubr::bgcolor("#262626")
  #
  #       shiny::incProgress(1 / (ncol(ggplotAllData) + 1), detail = "Done")
  #     })
  #
  #     return(gg_fin)
  #   },
  #   height = 800
  # ) # , height = 800*n_width/n_height)

  output$ggSinglePlot <- shiny::renderPlot(
    {
      # Write all valid columns of PLOT_DATA to ggplot grid ---------------------
      # Does this need to catch errors? Actually... yes! Would be helpful

      shiny::req(input$choose_graph)

      ggplotAllData <- clean_input_data(inputData()[["data"]])
      chosen_legend <- input$legend
      selected_plot <- input$choose_graph

      shiny::req(selected_plot %in% colnames(ggplotAllData))

      # Generate chosen image ---------------------------------------------------

      # plot_data_temp <- dplyr::select(ggplotAllData, c(`Samples`, dplyr::all_of(selected_plot)))
      plot_data_temp <- dplyr::select(ggplotAllData, c(`samples_var`, !!selected_plot))

      return(
        fj_plot(
          tempData        = plot_data_temp,
          # label           = selected_plot,
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
          chosen.theme    = input$ggtheme
        ) +
          ggplot2::theme(legend.position = input$legend)
      )
    },
    width = shiny::reactive(input$fig_width),
    height = shiny::reactive(input$fig_height)
  )


  # UI Elements -------------------------------------------------------------

  output$graphSelectControls <- shiny::renderUI({
    choice_names <- colnames(inputData()[["data"]])
    choice_names <- choice_names[2:length(choice_names)]

    htmltools::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::selectInput(
            inputId  = "choose_graph",
            label    = "Selected Plot",
            choices  = choice_names,
            selected = choice_names[1],
            multiple = FALSE
          )
        ),
        shiny::column(
          width = 6,
          shiny::downloadButton(
            outputId = "download_single_image",
            label    = "Download Selected Image"
          )
        )
      ),
      shiny::plotOutput(
        "ggSinglePlot",
        # height = 950
        width  = (input$fig_width),
        height = (input$fig_height)
      ),
      ## TESTING VALIDATE ERROR CALLS
      shiny::textOutput("validate_inputdata")
    )
  })

  output$validate_inputdata <- shiny::renderText({
    inputData()[["errorC"]]
  })


  # Download sections -------------------------------------------------------

  output$download_master_image <- shiny::downloadHandler(
    filename = function() {
      inFile <- input$file1

      if (is.null(inFile)) {
        myfile <- "example_data.pdf"
      } else {
        myfile <- paste0(tools::file_path_sans_ext(inFile$name), ".pdf")
      }
      myfile
    },
    content = function(file) {
      ggplotAllData <- clean_input_data(inputData()[["data"]])
      g             <- list()

      # Plan grid size ----------------------------------------------------------

      n_plots  <- ncol(ggplotAllData) - 1
      n_width <- ceiling(sqrt(n_plots))
      n_height <- ceiling(n_plots / n_width)

      # Generating each plot ----------------------------------------------------

      for (i in 2:ncol(ggplotAllData)) {
        plot_data_temp <- ggplotAllData[, c(1, i)]
        label <- colnames(plot_data_temp)[2]
        plot_n <- i - 1

        g[[plot_n]] <- fj_plot(
          tempData        = plot_data_temp,
          # label           = label,
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
          chosen.theme    = input$ggtheme
        )
      }

      gg_fin <- ggpubr::ggarrange(
        plotlist      = g,
        ncol          = n_height,
        nrow          = n_width,
        common.legend = TRUE,
        legend        = input$legend
      ) +
        ggpubr::bgcolor("#262626")

      ggplot2::ggsave(
        file,
        plot      = gg_fin,
        device    = "pdf",
        width     = input$fig_width * n_width,
        height    = input$fig_height * n_height,
        limitsize = FALSE,
        units     = "px",
        dpi       = 72
      )
    }
  )

  output$download_single_image <- shiny::downloadHandler(
    filename = function() {
      inFile <- input$file1

      if (is.null(inFile)) {
        myfile <- paste0(
          "example_data_",
          input$choose_graph, ".pdf"
        )
      } else {
        myfile <- paste0(
          tools::file_path_sans_ext(inFile$name), "_",
          input$choose_graph, ".pdf"
        )
      }
      myfile
    },
    content = function(file) {

      ggplotAllData <- clean_input_data(inputData()[["data"]])
      tempData <- ggplotAllData[, c("Samples", input$choose_graph)]

      g_single <- fj_plot(
        tempData        = tempData,
        # label           = input$choose_graph,
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
        chosen.theme    = input$ggtheme
      )

      ggplot2::ggsave(
        file,
        plot      = g_single,
        device    = "pdf",
        width     = input$fig_width,
        height    = input$fig_height,
        limitsize = FALSE,
        units     = "px",
        dpi       = 72
      )
    }
  )

  output$download_prism <- shiny::downloadHandler(
    filename = function() {
      inFile <- input$file1

      if (is.null(inFile)) {
        # myfile <- "example_data.pzfx"
        # 7.Dec.2023 changed the file format to .prism for latest prism format
        myfile <- "example_data.prism"
      } else {
        # myfile <- paste0(tools::file_path_sans_ext(inFile$name), ".pzfx")
        myfile <- paste0(tools::file_path_sans_ext(inFile$name), ".prism")
      }
      myfile
    },
    content = function(file) {
      data <- inputData()[["data"]]
      groups <- gsub("\\_.*", "", data$Samples)

      # Correctly format data for prism download ----------------------------

      prism_list <- list()
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

        for (i in 1:nrow(data)) {
          value <- as.numeric(data[i, uniq_coln])
          temp_col <- groups[i]

          for (j in 1:nrow(temp_dat)) {
            if (is.na(temp_dat[j, temp_col])) {
              temp_dat[j, temp_col] <- value
              value <- NA
            }
          }
        }

        if (input$transpose_prism) {
          temp_dat <- t(temp_dat)
        }
        prism_list[[uniq_coln]] <- temp_dat
      }

      temp_file <- tempfile()
      pzfx::write_pzfx(
        x = prism_list,
        path = temp_file,
        row_names = TRUE
      )

      file.copy(temp_file, file)
    }
  )

  output$download_pptx <- shiny::downloadHandler(
    filename = function() {
      inFile <- input$file1

      if (is.null(inFile)) {
        myfile <- "example_data.pptx"
      } else {
        myfile <- paste0(tools::file_path_sans_ext(inFile$name), ".pptx")
      }
      myfile
    },
    content = function(file) {
      # Generating each plot ----------------------------------------------------

      ggplotAllData <- clean_input_data(inputData()[["data"]])
      g <- list()

      shiny::withProgress(
        message = "Downloading .pptx",
        value = 0,
        min = 0,
        max = (ncol(ggplotAllData) + 1),
        {
          for (i in 2:ncol(ggplotAllData)) {
            plot_data_temp <- ggplotAllData[, c(1, i)]
            label <- colnames(plot_data_temp)[2]
            plot_n <- i - 1

            shiny::incProgress(
              amount = 1,
              message = "Generating Plots",
              detail = label
            )

            g[[plot_n]] <- fj_plot(
              tempData        = plot_data_temp,
              # label           = label,
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
              chosen.theme    = input$ggtheme
            )
          }

          shiny::incProgress(
            amount = 1,
            message = "Saving Plots to .pptx...",
            detail = "~1 second per image"
          )

          g_dml <- purrr::map(g, create_dml)
          temp_file <- paste0(tempfile(), ".pptx")

          purrr::map(
            g_dml,
            create_pptx,
            path = temp_file
          )
        }
      )

      file.copy(temp_file, file)
    }
  )

  output$estimated_master_image_download_time <- shiny::renderText({
    inputData()[["data"]] |>
      clean_input_data() |>
      ncol() |>
      magrittr::divide_by(6) |>
      round(digits = 1) |>
      paste0("s") #|>
    # return()
  })

  output$estimated_pptx_format_download_time <- shiny::renderText({
    inputData()[["data"]] |>
      clean_input_data() |>
      ncol() |>
      magrittr::divide_by(1.8) |>
      round(digits = 1) |>
      paste0("s") #|>
    # return()
  })

  output$estimated_prism_format_download_time <- shiny::renderText({
    inputData()[["data"]] |>
      clean_input_data() |>
      ncol() |>
      magrittr::divide_by(50) |>
      round(digits = 1) |>
      paste0("s") #|>
    # return()
  })
})
