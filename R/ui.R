
# Sam Old Shiny App -------------------------------------------------------
# For visualising Flow Cytometry data exported from FlowJo ----------------
# A project started 2nd February 2023 until...? ---------------------------


ui <- sidebarLayout(

  sidebarPanel(
    width = 4,


    # Otter Image + File Input
    img(src = "www/flowj_otter_lightblue_1.4.1.jpg", width = "100%", height = "100%", align = "margin-left"),
    tags$figcaption(HTML("<em>Flow Jotter v1.4.1</em>"), align = "right", alt = "By my friend Dime :)"),

    fileInput(
      inputId = "file1",
      label   = "Choose Excel file",
      accept  = c(".xls", ".xlsx")),
    tags$hr(),


    # Plotting Options [Menu] -------------------------------------------------
    h2("Plot Options"),

    fluidRow(
      column(
        width = 6,
        checkboxInput(
          inputId = "jitter",
          label = "Jitter",
          value = TRUE)
      ),
      column(
        width = 6,
        checkboxInput(
          inputId = "jotter",
          label = "Jotter",
          value = TRUE)
      )
    ),
    fluidRow(
        # column(
        #   width = 6,
        # checkboxInput(
        #   inputId = "MFI_logscale",
        #   label = "MFI log scale",
        #   value = TRUE)
      column(
        width = 6,
        checkboxInput(
          inputId = "plot_bar",
          label = "Bar",
          value = FALSE)
      ),
      column(
        width = 6,
        checkboxInput(
          inputId = "plot_boxplot",
          label = "Boxplot",
          value = FALSE)
      )
    ),
    fluidRow(
      column(
        width = 6,
        checkboxInput(
          inputId = "plot_mean",
          label = "Mean",
          value = FALSE)
        ),
      column(
        width = 6,
        checkboxInput(
          inputId = "plot_se_of_the_mean",
          label = "± SE",
          value = FALSE)
      )
    ),
    fluidRow(
      column(
        width = 12,
        checkboxInput(
          inputId = "append_sheet",
          label = "Append Sheet Name",
          value = FALSE))
    ),
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = "fig_width",
          label = "Width px",
          min = 100, max = 2000,
          step = 100, value = 700)
      ),
      column(
        width = 6,
        numericInput(
          inputId = "fig_height",
          label = "Height px",
          min = 100, max = 20000,
          step = 100, value = 500))
    ),
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = "font_size",
          label = "Font size",
          min = 6, max = 100,
          step = 2, value = 24)
      ),
      column(
        width = 6,
        numericInput(
          inputId = "pt_size",
          label = "Point size",
          min = 0.1, max = 20,
          step = 1, value = 5))
    ),
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = "legend",
          label = "Legend",
          choices = c("top", "bottom", "left", "right", "none"),
          selected = "right",
          multiple = FALSE)
      ),
      column(
        width = 6,
        selectInput(
          inputId = "colour_palette_picker",
          label = "Palette",
          choices = c(
            "Black & White",
            "Set1", "Set2", "Set3",
            "Pastel1", "Pastel2",
            "Paired",
            "Dark2",
            "Accent"),
          selected = "Set1")
      )
    ),
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = "xlab_angle",
          label = "x-axis Angle",
          choices = c(0,30, 45, 90, 270),
          selected = 45,
          multiple = FALSE)
      ),
      column(
        width = 6,
        selectInput(
          inputId = "ggtheme",
          label = "Theme",
          choices = c("light", "classic"),
          selected = "light",
          multiple = FALSE)
      )
    ),

    # Line break
    tags$hr(),


    # Download Options [Menu] -------------------------------------------------
    h2("Download Options"),

    fluidRow(
      column(
        width = 9,
        downloadButton(
          outputId = "download_master_image",
          label = "Master Image",
          style = "width:100%;")
      ),
      column(
        width = 2,
        textOutput("estimated_master_image_download_time")
      )
    ),
    fluidRow(
      column(
        width = 9,
        downloadButton(
          outputId = "download_pptx",
          label = ".pptx format",
          style = "width:100%;")
      ),
      column(
        width = 2,
        textOutput("estimated_pptx_format_download_time")
      )
    ),
    fluidRow(
      column(
        width = 9,
        downloadButton(
          outputId = "download_prism",
          label = "Prism format",
          style = "width:100%;")
      ),
      column(
        width = 2,
        textOutput("estimated_prism_format_download_time")
      )
    ),
    fluidRow(
      column(
        width = 12,
        checkboxInput(
          inputId = "transpose_prism",
          label = "Transpose Prism Output",
          value = FALSE))
    )
  ),


  # [Main] ------------------------------------------------------------------
  mainPanel(
    width = 8,
    navbarPage(
      title = "Flow Jotter",
      fluid = TRUE,
      theme = shinytheme("darkly"),
      windowTitle = "Flow Jotter",
      collapsible = TRUE,
      footer = HTML("<i>siold@malaghan.org.nz</i>"),

      tabPanel(
        title = "Info Page",

        DTOutput("data_to_header_table"),

        h2("Choosing Graph types"),
        p("Graph type is decided by the **first character** of the column title"),
        tags$div(
          tags$ul(
            tags$li("% = Percentage Graph"),
            tags$li("N = Number Graph"),
            tags$li("M = MFI Plot"),
            tags$li("Red columns highlight columns not starting with `%/N/M`"),
            tags$li("Column names must be unique"))),

        h2("Choosing Groups"),
        tags$div(
          tags$ul(
            tags$li("The left-most column ('Samples') dictates groups for samples"),
            tags$li("Everything before the first underscore becomes grouped"),
            tags$li("There is no limit on the number of groups"))),

        HTML("<br>"),
        h2("FAQ"),

        h3("Why are my images so squished?"),
        tags$div(
          tags$ul(
            tags$li(
              p("You need to increase the 'Height px' of your image")
            )
          )
        ),

        h3("Is there an example of how to setup an Excel file?"),
        tags$div(tags$ul(tags$li(
          a(href="flowjotter_example_data.xlsx",
            "A typical Excel counts layout can be found above or downloaded here",
            download=NA, target="_blank")))),

        h3("Can my excel file contain multiple sheets?"),
        tags$div(tags$ul(tags$li(
          h4("Yes:"),
          tags$ul(tags$li(
            p("Each sheet must have a consistent and viable column name, group name and data.")))))),
        tags$div(tags$ul(tags$li(
          h4("No:"),
          tags$ul(tags$li(
            p("If you are changing the groups that are to be plotted in each sheet, it will not work.")
          ), tags$li(
            p("Groups must stay the same throughout the excel sheet e.g. HDM vs Nb vs Ca")
          ), tags$li(
            p("To perform multiple comparisons e.g. (HDM vs Nb) & (HDM vs Ca) & (Nb vs Ca), you will need to create multiple excel files")
          ))))),

        h3("My groups are getting oversplit into multiple groups"),
        tags$div(tags$ul(tags$li(
          p("Carefully read the figure legend, there may be a tiny typo")
        )))
      ),


    # Goodbye, slow graphing all plot page... ---------------------------------
      # tabPanel(
      #   title = "All Plots",
      #   plotOutput("ggAllPlots",
      #     height = "1200")
      # ),

      tabPanel(
        title = "Plot Preview",
        uiOutput("graphSelectControls")
      )
    )
  )
)
