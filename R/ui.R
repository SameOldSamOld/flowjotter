#' Shiny App UI flowjotter
#'
#' User Interface for an app for visualising Flow Cytometry data exported from Flowjo
#' A project started 2nd Februaryr 2023, Sam Old.
#'
#' @importFrom bslib bs_theme
flowjotter_ui <- shiny::sidebarLayout(
  shiny::sidebarPanel(
    width = 4,

    # Otter Image + File Input
    htmltools::img(
      # src = system.file("logos/flowj_otter_lightblue.jpg", package = "flowjotter"),
      # src = company_logo(),
      src = "logos/flowj_otter_lightblue.jpg",
      width = "100%", height = "100%", align = "margin-left"),
    htmltools::tags$figcaption(htmltools::HTML("<em>Flow Jotter v1.0.0</em>"), align = "right", alt = "By my friend Dime :)"),
    shiny::fileInput(
      inputId = "file1",
      label   = "Choose Excel file",
      accept  = c(".xls", ".xlsx")
    ),
    htmltools::tags$hr(),


    # Plotting Options [Menu] -------------------------------------------------
    htmltools::h2("Plot Options"),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::checkboxInput(
          inputId = "jitter",
          label = "Jitter",
          value = TRUE
        )
      ),
      shiny::column(
        width = 6,
        shiny::checkboxInput(
          inputId = "jotter",
          label = "Jotter",
          value = TRUE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::checkboxInput(
          inputId = "plot_bar",
          label = "Bar",
          value = FALSE
        )
      ),
      shiny::column(
        width = 6,
        shiny::checkboxInput(
          inputId = "plot_boxplot",
          label = "Boxplot",
          value = FALSE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::checkboxInput(
          inputId = "plot_mean",
          label = "Mean",
          value = FALSE
        )
      ),
      shiny::column(
        width = 6,
        shiny::checkboxInput(
          inputId = "plot_se_of_the_mean",
          label = "\u00b1 SE",
          value = FALSE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        # shiny::checkboxInput(
        #   inputId = "append_sheet",
        #   label = "Append Sheet Name in Prism",
        #   value = FALSE
        # )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::numericInput(
          inputId = "fig_width",
          label = "Width px",
          min = 100, max = 2000,
          step = 100, value = 700
        )
      ),
      shiny::column(
        width = 6,
        shiny::numericInput(
          inputId = "fig_height",
          label = "Height px",
          min = 100, max = 20000,
          step = 100, value = 500
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::numericInput(
          inputId = "font_size",
          label = "Font size",
          min = 6, max = 100,
          step = 2, value = 24
        )
      ),
      shiny::column(
        width = 6,
        shiny::numericInput(
          inputId = "pt_size",
          label = "Point size",
          min = 0.1, max = 20,
          step = 1, value = 5
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::selectInput(
          inputId = "legend",
          label = "Legend",
          choices = c("top", "bottom", "left", "right", "none"),
          selected = "right",
          multiple = FALSE
        )
      ),
      shiny::column(
        width = 6,
        shiny::selectInput(
          inputId = "colour_palette_picker",
          label = "Palette",
          choices = c(
            "Black & White",
            "Set1", "Set2", "Set3",
            "Pastel1", "Pastel2",
            "Paired",
            "Dark2",
            "Accent"
          ),
          selected = "Set1"
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::selectInput(
          inputId = "xlab_angle",
          label = "x-axis Angle",
          choices = c(0, 30, 45, 90, 270),
          selected = 45,
          multiple = FALSE
        )
      ),
      shiny::column(
        width = 6,
        shiny::selectInput(
          inputId = "ggtheme",
          label = "Theme",
          choices = c("light", "classic"),
          selected = "light",
          multiple = FALSE
        )
      )
    ),

    # Line break
    htmltools::tags$hr(),


    # Download Options [Menu] -------------------------------------------------
    htmltools::h2("Download Options"),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::checkboxInput(
          inputId = "append_sheet",
          label = "Prism: Append Sheet Name",
          value = FALSE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::checkboxInput(
          inputId = "transpose_prism",
          label = "Prism: Transpose Output",
          value = FALSE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 9,
        shiny::downloadButton(
          outputId = "download_master_image",
          label = "Master Image",
          style = "width:100%;"
        )
      ),
      shiny::column(
        width = 2,
        shiny::textOutput("estimated_master_image_download_time")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 9,
        shiny::downloadButton(
          outputId = "download_pptx",
          label = ".pptx format",
          style = "width:100%;"
        )
      ),
      shiny::column(
        width = 2,
        shiny::textOutput("estimated_pptx_format_download_time")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 9,
        shiny::downloadButton(
          outputId = "download_prism",
          label = "Prism format",
          style = "width:100%;"
        )
      ),
      shiny::column(
        width = 2,
        shiny::textOutput("estimated_prism_format_download_time")
      )
    )
  ),


  # [Main] ------------------------------------------------------------------
  shiny::mainPanel(
    width = 8,
    shiny::navbarPage(
      title = "Flow Jotter",
      fluid = TRUE,
      theme = bslib::bs_theme(bootswatch = "darkly"),
      windowTitle = "Flow Jotter",
      collapsible = TRUE,
      footer = htmltools::HTML("<i>siold@malaghan.org.nz</i>"),
      shiny::tabPanel(
        title = "Info Page",
        DT::DTOutput("data_to_header_table"),
        htmltools::HTML("<br>"),
        htmltools::h2("Choosing Graph types"),
        htmltools::p("Graph type is decided by the first character of the column title"),
        htmltools::tags$div(
          htmltools::tags$ul(
            htmltools::tags$li("% = Percentage Graph"),
            htmltools::tags$li("N = Number Graph"),
            htmltools::tags$li("M = MFI Plot"),
            htmltools::tags$li("Red columns highlight columns not starting with `%/N/M`"),
            htmltools::tags$li("Column names must be unique")
          )
        ),
        htmltools::h2("Choosing Groups"),
        htmltools::tags$div(
          htmltools::tags$ul(
            htmltools::tags$li("The left-most column ('Samples') dictates groups for samples"),
            htmltools::tags$li("Everything before the first underscore becomes grouped"),
            htmltools::tags$li("There is no limit on the number of groups")
          )
        ),
        htmltools::HTML("<br>"),
        htmltools::h2("FAQ"),
        htmltools::h3("Is there an example of how to setup an Excel file?"),
        htmltools::tags$div(htmltools::tags$ul(htmltools::tags$li(
          htmltools::a(
            href = "logos/flowjotter_example_data.xlsx",
            "A typical Excel counts layout can be found above or downloaded here",
            download = NA, target = "_blank"
          )
        ))),
        htmltools::h3("Can my excel file contain multiple sheets?"),
        htmltools::tags$div(htmltools::tags$ul(htmltools::tags$li(
          htmltools::h4("Yes:"),
          htmltools::tags$ul(htmltools::tags$li(
            htmltools::p("Each sheet must have a consistent and viable column name, group name and data.")
          ))
        ))),
        htmltools::tags$div(htmltools::tags$ul(htmltools::tags$li(
          htmltools::h4("No:"),
          htmltools::tags$ul(htmltools::tags$li(
            htmltools::p("If you are changing the groups that are to be plotted in each sheet, it will not work.")
          ), htmltools::tags$li(
            htmltools::p("Groups must stay the same throughout the excel sheet e.g. HDM vs Nb vs Ca")
          ), htmltools::tags$li(
            htmltools::p("To perform multiple comparisons e.g. (HDM vs Nb) & (HDM vs Ca) & (Nb vs Ca), you will need to create multiple excel files")
          ), htmltools::tags$li(
            htmltools::p("If multiple sheets are encountered, you may reach a warning if not correctly formatted.")
          ))
        ))),
        htmltools::h3("Why are my images so squished?"),
        htmltools::tags$div(
          htmltools::tags$ul(
            htmltools::tags$li(
              htmltools::p("You need to increase the 'Height px' of your image")
            )
          )
        ),
        htmltools::h3("My groups are getting oversplit into multiple groups"),
        htmltools::tags$div(htmltools::tags$ul(htmltools::tags$li(
          htmltools::p("Carefully read the figure legend, there may be a tiny typo")
        )))
      ),


      # Goodbye, slow graphing all plot page... ---------------------------------
      # tabPanel(
      #   title = "All Plots",
      #   plotOutput("ggAllPlots",
      #     height = "1200")
      # ),

      shiny::tabPanel(
        title = "Plot Preview",
        shiny::uiOutput("graphSelectControls")
      )
    )
  )
)
