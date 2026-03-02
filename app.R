# -------------------------------------------------------------------------
# Chemical Space Plot (CSP) Shiny Application
# -------------------------------------------------------------------------
# Description: This Shiny app generates Chemical Space Plots (CSPs) to
#              visualize the partitioning properties of chemicals in the
#              environment (Air, Water, Octanol/Organic Matter).
# Author:      Eric Wootton
# -------------------------------------------------------------------------

# Load required packages --------------------------------------------------
library(shiny)
library(dplyr) # Data manipulation
library(ggplot2) # Plotting
library(ggrepel) # Non-overlapping text labels
library(ragg) # AGG graphic backend
library(shinyjs) # JavaScript operations in Shiny
library(colourpicker) # Colour input widget
library(shinycssloaders) # Loading spinners

# Global Options ----------------------------------------------------------
# Change graphical backend to ragg for better performance/rendering
options(shiny.useragg = TRUE)

# Force all text labels to show in ggrepel
options(ggrepel.max.overlaps = Inf)

# Data Structures ---------------------------------------------------------
# Create template dataframe for CSV export
df_template <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(df_template) <- c("Compound", "logKow", "logKoa", "logKaw", "Group")

# Helper Functions --------------------------------------------------------
# Format labels with one decimal point
format_decimal <- function(x) {
  sprintf("%.1f", x)
}

# UI Components -----------------------------------------------------------

# 1. Intro Panel
intro_panel <- tabPanel(
  "About the CSP",
  p("The Chemical Space Plot (CSP) is a graphical tool which illustrates the partitioning properties of chemicals in the environment. These partitioning tendencies are largely determined by three fundamental partition ratios (formerly coefficients), describing the equilibrium distribution of a chemical between any two of air, water, and n-octanol (hereafter octanol). These are Kaw, Kow, and Koa, where A, W and O refer to air, water and octanol, respectively. These ratios are commonly used to estimate chemical fate between the atmosphere, water bodies, and soils, sediments and biota. Generally, water-saturated octanol serves as a surrogate for hydrophobic media such as organic carbon in soils and lipid-rich tissues in biota. Knowledge of these three partition ratios uniquely characterizes the 'partitioning personality' of a given chemical from the point of view of its equilibrium partitioning properties in the environment. These three ratios are not independent, and only two are needed to define the partitioning properties of a chemical in the organic-water-air space."),
  p("The CSP was developed by Mackay and co-workers [1,2] to graphically illustrate the partitioning properties of chemicals. The typical CSP is a rectilinear plot of logKaw vs logKow, sometimes with the corresponding calculated values of logKoa inscribed as diagonal lines on the plot. Any chemical may be situated on the CSP via its unique 'characteristic' point, defined by its logKaw and logKow values. A typical CSP has threshold lines that delineate regions of the partitioning space within which greater than some threshold mole % of the chemical will be found in a given medium. These thresholds are commonly set to one or more of 1, 33, 50 or 99 mole %. The intersection of these threshold lines define a central region in which characteristic points of 'multimedia' chemicals lie, those that simultaneously partition to all three media to a significant extent."),
  p("To generate a CSP, it is necessary to define the relative volumes of the three media phases. These might be, for example: 650,000:1300:1 (Air:Water:Organic Matter), as in the first published CSP of this type by Gouin et al. (2). As a result, CSPs differ in the position of their threshold lines due to differences in the assumed relative volumes of the three media. Recognition of the fact that the position of the threshold mole fraction lines is a mathematical function of the relative volumes of the three environmental compartments was the main impetus for developing this CSP app. Each unique set of relative volumes requires the generation of a new set of threshold lines, which can be a laborious task if done 'by hand' using a spreadsheet. It was primarily the desire to eliminate this task that prompted the development of this CSP app."),
  p("This app also offers the option of adding one or more threshold indicator figures to the traditional CSP. Each of these figures shares the attribute that their ends or angles are located at a position where the amount of chemical in one medium is at a certain threshold value, say 99 mole percent, and the remainder is divided equally between the other two media. These additional figures may aid in the interpretation of the relative position of characteristic points associated with closely related or homologous series chemicals. Note that of the three figures offered, the triangular figure has sides that are not physically interpretable in any meaningful way, whereas the curved figure has sides which correspond to a fixed mole fraction in two of the media, with the remainder in the third medium."),
  strong(p("References")),
  p("[1] S.C. Lee, D. Mackay, Environ. Sci. Technol. 14 (1995) 1839-1846."),
  p("[2] T. Gouin, D. Mackay, E. Webster, F. Wania, Environ, Sci. Technol. 34 (2000) 881-884."),
  br(),
  p("This app was created by Eric Wootton (ericwootton[at]mail.mcgill.ca)"),
  br(),
  downloadButton("download_app", label = "Download App Source Code"),
  br(),
  br()
)

# 2. How To Use Panel
howto_panel <- tabPanel(
  "How to Use",
  strong(p("Changing Input Values")),
  p("A preview of your CSP will generate automatically upon navigation to the 'Generate Multiphase Plot' tab."),
  p("Air, water, and octanol compartment volumes can all be changed under 'Compartment Volumes' to suit the needs of your desired simulation."),
  p("Fractional concentration lines can be modified under 'Molar Fractions'. By default, Line 3 is toggled off."),
  p("Axis values can be changed to encompass your data and desired chemical space under 'Minimum Values' and 'Maximum Values'."),
  p("To restore all values to defaults, click on 'Reset to Default Values'."),
  br(),
  strong(p("Adding Chemical Data")),
  p("To plot the characteristic points for one or more molecules, download the provided CSV template and fill it out with the corresponding logKow, logKaw, and/or logKoa values and compound names or identifiers. Only two out of three values are required for each compound. Any missing logKow, logKaw, or logKoa values will be calculated automatically."),
  p("If you wish to colour some molecules categorically, you may fill in the 'Group' column with each compound's category."),
  p("Upload your completed CSV file and toggle 'Show points' under 'Data Options'. Toggle 'Colour points with default palette' under the same heading if you wish to distinguish the groups in your data file by colour."),
  p("Under 'Point Colour Palette', you can change the colour palette used for grouping. Qualitative palettes are best for differentiating between distinct groups, while divergent palettes are best for bimodal grouping."),
  p("If you wish to use your own colour palette, toggle 'Colour points with custom palette'. This option requires a hexadecimal colour code in each row of the 'Group' column of your CSV file. A custom colour picker is included under to help choose hexadecimal codes."),
  br(),
  strong(p("Toggle Labels")),
  p("Labels for media, molar fraction lines, and added data points can all be toggled on or off under 'Toggle Labels'."),
  p("Media labels are positioned automatically, but they can be moved by toggling 'Use custom positions' under 'Media Label Positions' and specifying custom coordinates."),
  p("To alter the degree of data point label crowding, use the slider under 'Strength of Label Repulsion'."),
  br(),
  strong(p("Toggle Threshold Lines and Threshold Indicators")),
  p("Up to three different molar fraction lines can be toggled on or off under the 'Toggle Threshold Lines' heading. These lines indicate the characteristic points for a chemical where the molar fraction in a given medium achieves a threshold value set by the user under 'Molar Fractions', and the remainder of the chemical is distributed between the remaining two media."),
  p("There are also three types of threshold indicators that may be used to illustrate characteristic points where the concentration of a chemical achieves the \"Line 1\" mole fractions in one medium, and the remainder is partitioned equally in the remaining two media. These characteristic points are indicated by the apices or ends of the figures, as appropriate. These indicators can be toggled on or off under 'Toggle Threshold Indicators'. By default, three arrows are used to identify these characteristic points. However, a more accurate set of curved lines can be toggled on or off as well, the sides of which indicate characteristic points where the chemical has achieved the \"Line 1\" mole fraction threshold value in two of the media, and the remainder of the chemical is in the third medium."),
  p("Please note that all threshold indicators are governed by the fractional concentration of Line 1."),
  br(),
  strong(p("Additional Aesthetic Options")),
  p("There are numerous additional options for altering the appearance of your CSP. These include sliders to vary line thickness as well as the ability to change the representative colours for each medium. A background slope of 1 can be toggled on or off under 'Additional Aesthetics'. Point and label size can be changed under 'Data Aesthetics'. The number of axis tick labels can be adjusted under 'Number of Axis Ticks'."),
  br(),
  strong(p("Exploring and Exporting")),
  p("By hovering with your cursor, coordinates will automatically generate to the left of the plot preview. Similarly, the vertices of your chemical space are reported underneath."),
  p("To download a high resolution image of your CSP, select your preferred format (PNG or SVG) and click 'Download Plot'. SVG is recommended for publications as it produces a scalable vector image."),
  br(),
  strong(p("Generating an Atmospheric Plot")),
  p("Under the 'Generate Atmospheric Plot' tab, an atmospheric chemistry relevant CSP can be made as a plot of logKaw vs logKoa."),
  br()
)

# Create multi plot panel
multi_plot_panel <- tabPanel(
  "Generate Multiphase Plot",
  useShinyjs(),
  fluidRow(
    column(
      3,
      div(
        style = "white-space: nowrap;",
        h4(strong("Compartment Volumes"))
      ),
      numericInput("vol_air", label = "Air", value = 641848.5237),
      numericInput("vol_water", label = "Water", value = 1283.697047),
      numericInput("vol_octanol", label = "Organic Matter", value = 1),
    ),
    column(
      3,
      h4(strong("Molar Fractions")),
      numericInput("molperc_B", label = "Threshold Line 1", value = 0.99),
      numericInput("molperc_A", label = "Threshold Line 2", value = 0.5),
      numericInput("molperc_C", label = "Threshold Line 3", value = 0.1)
    ),
    column(
      3,
      h4(strong("Minimum Values")),
      numericInput("x_min", label = "logKow", value = -6),
      numericInput("y_min", label = "logKaw", value = -12),
    ),
    column(
      3,
      h4(strong("Maximum Values")),
      numericInput("x_max", label = "logKow", value = 12),
      numericInput("y_max", label = "logKaw", value = 6)
    ),
  ),
  fluidRow(
    hr(),
    column(
      4,
      actionButton("reset", label = "Reset to Default Values"),
      br(),
      br(),
      fileInput("file1", "Choose CSV File",
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      downloadButton("template", label = "Download CSV Template"),
      br(),
      br(),
      radioButtons("download_format", label = "Download Format",
        choices = c("PNG" = "png", "SVG" = "svg"), selected = "png", inline = TRUE
      ),
      downloadButton("download", label = "Download Plot"),
      br(),
      br(),
      p(strong("Coordinates (Hover on Plot)")),
      div(style = "max-width: 250px;", verbatimTextOutput("info")),
      br(),
      p(strong("Vertices (logKow, logKaw)")),
      div(style = "max-width: 350px;", verbatimTextOutput("critical_points"))
    ),
    column(
      8,
      h4(strong("Multiphase Plot Preview")),
      withSpinner(plotOutput(height = "auto", width = "auto", "plot1", hover = "plot_hover"))
    )
  ),
  fluidRow(
    hr(),
    column(
      3,
      p(strong("Data Options")),
      checkboxInput("point_switch", label = "Show points", value = FALSE),
      checkboxInput("colour_switch", label = "Colour points with default palette", value = FALSE),
      checkboxInput("custom_colour_switch", label = "Colour points with custom palette", value = FALSE),
      p(strong("Additional Aesthetics")),
      checkboxInput("ribbon_switch", label = "Shade media", value = TRUE),
      checkboxInput("diagonal_switch", label = "Show slope = 1", value = FALSE)
    ),
    column(
      3,
      p(strong("Toggle Threshold Lines")),
      checkboxInput("line1_switch", label = "Show threshold line 1", value = TRUE),
      checkboxInput("line2_switch", label = "Show threshold line 2", value = TRUE),
      checkboxInput("line3_switch", label = "Show threshold line 3", value = FALSE),
      p(strong("Toggle Threshold Indicators")),
      checkboxInput("centroid_switch", label = "Show centroid threshold indicators", value = TRUE),
      checkboxInput("triangle_switch", label = "Show triangular threshold indicators", value = FALSE),
      checkboxInput("egg_switch", label = "Show curved threshold indicators", value = FALSE)
    ),
    column(
      3,
      p(strong("Toggle Labels")),
      checkboxInput("label_switch", label = "Label points", value = FALSE),
      checkboxInput("media_switch", label = "Label media", value = TRUE),
      checkboxInput("linelab1_switch", label = "Label threshold line 1", value = TRUE),
      checkboxInput("linelab2_switch", label = "Label threshold line 2", value = TRUE),
      checkboxInput("linelab3_switch", label = "Label threshold line 3", value = FALSE),
      sliderInput("numTicks", label = "Number of Axis Ticks", min = 0, max = 20, value = 7)
    )
  ),
  hr(),
  fluidRow(
    column(
      3,
      h4(strong("Media Colours")),
      colourInput("col_air", label = "Air", value = "#ffa500", showColour = c("both", "text", "background")),
      colourInput("col_water", label = "Water", value = "#1010ff", showColour = c("both", "text", "background")),
      colourInput("col_octanol", label = "Organic Matter", value = "#a9a9a9", showColour = c("both", "text", "background"))
    ),
    column(
      3,
      h4(strong("Point Colour Palette")),
      radioButtons("palette",
        label = "Default Palettes", choices =
          c(
            "Qualitative Bold" = "Dark2",
            "Qualitative Light" = "Accent",
            "Qualitative Paired" = "Paired",
            "Divergent Spectral" = "Spectral",
            "Divergent Red-Grey" = "RdGy",
            "Divergent Purple-Orange" = "PuOr"
          )
      ),
      colourInput("BLANK,", label = "Custom Colour Picker", value = "#21613B", showColour = c("both", "text", "background"))
    ),
    column(
      4,
      h4(strong("Media Label Positions")),
      checkboxInput("medium_label_switch", label = "Use custom positions", value = FALSE),
      column(
        3,
        br(),
        br(),
        p(strong("Air")),
        br(),
        p(strong("Water")),
        br(),
        p(strong("Organic Matter"))
      ),
      column(
        4,
        p(strong("logKow")),
        numericInput("Air_Kow", label = NULL, value = 1, width = 100),
        numericInput("Water_Kow", label = NULL, value = -2, width = 100),
        numericInput("Octanol_Kow", label = NULL, value = 9, width = 100)
      ),
      column(
        4,
        p(strong("logKaw")),
        numericInput("Air_Kaw", label = NULL, value = 2, width = 100),
        numericInput("Water_Kaw", label = NULL, value = -7, width = 100),
        numericInput("Octanol_Kaw", label = NULL, value = -5, width = 100)
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      3,
      h4(strong("Data Aesthetics")),
      sliderInput("point_size", "Point Size", min = 0.5, max = 5, value = 1, step = 0.25),
      sliderInput("label_size", "Label Size", min = 0, max = 6, value = 3, step = 0.25),
      sliderInput("padding", "Strength of Label Repulsion", min = 0.2, max = 0.6, value = 0.4, step = 0.025)
    ),
    column(
      3,
      div(
        style = "white-space: nowrap;",
        h4(strong("Line Thickness"))
      ),
      sliderInput("th_line1", "Threshold Line 1", min = 0, max = 2, value = 1, step = 0.1),
      sliderInput("th_line2", "Threshold Line 2", min = 0, max = 2, value = 1, step = 0.1),
      sliderInput("th_line3", "Threshold Line 3", min = 0, max = 2, value = 1, step = 0.1)
    ),
    column(
      3,
      br(),
      br(),
      div(
        style = "white-space: nowrap;",
        sliderInput("th_centroid", "Centroid Threshold Indicators", min = 0, max = 2, value = 1, step = 0.1),
        sliderInput("th_triangle", "Triangular Threshold Indicators", min = 0, max = 2, value = 1, step = 0.1),
        sliderInput("th_egg", "Curved Threshold Indicators", min = 0, max = 2, value = 1, step = 0.1)
      )
    ),
    column(
      3,
      br(),
      br(),
      sliderInput("media_label_size", "Media Label Size", min = 0, max = 8, value = 4, step = 0.5),
      sliderInput("line_label_size", "Line Label Size", min = 0, max = 4, value = 2, step = 0.2)
    )
  )
)

# Create atmospheric plot panel
atm_plot_panel <- tabPanel(
  "Generate Atmospheric Plot",
  useShinyjs(),
  fluidRow(
    column(
      3,
      div(
        style = "white-space: nowrap;",
        h4(strong("Compartment Volumes"))
      ),
      numericInput("vol_air_atm", label = "Air", value = 10^11),
      numericInput("vol_water_atm", label = "Water", value = 1),
      numericInput("vol_octanol_atm", label = "Organic Matter", value = 1),
    ),
    column(
      3,
      h4(strong("Molar Fractions")),
      numericInput("molperc_B_atm", label = "Threshold Line 1", value = 0.99),
      numericInput("molperc_A_atm", label = "Threshold Line 2", value = 0.5),
      numericInput("molperc_C_atm", label = "Threshold Line 3", value = 0.1)
    ),
    column(
      3,
      h4(strong("Minimum Values")),
      numericInput("x_min_atm", label = "logKoa", value = 2),
      numericInput("y_min_atm", label = "logKaw", value = -20),
    ),
    column(
      3,
      h4(strong("Maximum Values")),
      numericInput("x_max_atm", label = "logKoa", value = 20),
      numericInput("y_max_atm", label = "logKaw", value = -2)
    ),
  ),
  fluidRow(
    hr(),
    column(
      4,
      actionButton("reset_atm", label = "Reset to Default Values"),
      br(),
      br(),
      radioButtons("download_format_atm", label = "Download Format",
        choices = c("PNG" = "png", "SVG" = "svg"), selected = "png", inline = TRUE
      ),
      downloadButton("download_atm", label = "Download Plot"),
      br(),
      br(),
      p(strong("Coordinates (Hover on Plot)")),
      div(style = "max-width: 250px;", verbatimTextOutput("info_atm"))
    ),
    column(
      8,
      h4(strong("Atmospheric Plot Preview")),
      withSpinner(plotOutput(height = "auto", width = "auto", "plot2", hover = "plot_hover_atm"))
    )
  ),
  fluidRow(
    hr(),
    column(
      3,
      p(strong("Data Options")),
      checkboxInput("point_switch_atm", label = "Show points", value = FALSE),
      checkboxInput("colour_switch_atm", label = "Colour points with default palette", value = FALSE),
      checkboxInput("custom_colour_switch_atm", label = "Colour points with custom palette", value = FALSE),
      p(strong("Additional Aesthetics")),
      checkboxInput("ribbon_switch_atm", label = "Shade media", value = TRUE),
      checkboxInput("diagonal_switch_atm", label = "Show slope = 1", value = FALSE)
    ),
    column(
      3,
      p(strong("Toggle Threshold Lines")),
      checkboxInput("line1_switch_atm", label = "Show threshold line 1", value = TRUE),
      checkboxInput("line2_switch_atm", label = "Show threshold line 2", value = TRUE),
      checkboxInput("line3_switch_atm", label = "Show threshold line 3", value = FALSE),
      checkboxInput("cloud_switch_atm", label = "Show cloud threshold lines (50%)", value = FALSE)
    ),
    column(
      3,
      p(strong("Toggle Labels")),
      checkboxInput("label_switch_atm", label = "Label points", value = FALSE),
      checkboxInput("media_switch_atm", label = "Label media", value = TRUE),
      checkboxInput("linelab1_switch_atm", label = "Label threshold line 1", value = TRUE),
      checkboxInput("linelab2_switch_atm", label = "Label threshold line 2", value = TRUE),
      checkboxInput("linelab3_switch_atm", label = "Label threshold line 3", value = FALSE),
      sliderInput("numTicks_atm", label = "Number of Axis Ticks", min = 0, max = 20, value = 7)
    )
  ),
  hr(),
  fluidRow(
    column(
      3,
      h4(strong("Media Colours")),
      colourInput("col_air_atm", label = "Air", value = "#ffa500", showColour = c("both", "text", "background")),
      colourInput("col_water_atm", label = "Water", value = "#1010ff", showColour = c("both", "text", "background")),
      colourInput("col_octanol_atm", label = "Organic Matter", value = "#a9a9a9", showColour = c("both", "text", "background"))
    ),
    column(
      3,
      h4(strong("Point Colour Palette")),
      radioButtons("palette_atm",
        label = "Default Palettes", choices =
          c(
            "Qualitative Bold" = "Dark2",
            "Qualitative Light" = "Accent",
            "Qualitative Paired" = "Paired",
            "Divergent Spectral" = "Spectral",
            "Divergent Red-Grey" = "RdGy",
            "Divergent Purple-Orange" = "PuOr"
          )
      ),
      colourInput("BLANK,", label = "Custom Colour Picker", value = "#21613B", showColour = c("both", "text", "background"))
    ),
    column(
      4,
      h4(strong("Media Label Positions")),
      column(
        3,
        br(),
        br(),
        p(strong("Air")),
        br(),
        p(strong("Water")),
        br(),
        p(strong("Organic Matter"))
      ),
      column(
        4,
        p(strong("logKoa")),
        numericInput("Air_Koa_atm", label = NULL, value = 6, width = 100),
        numericInput("Water_Koa_atm", label = NULL, value = 8, width = 100),
        numericInput("Octanol_Koa_atm", label = NULL, value = 17, width = 100)
      ),
      column(
        4,
        p(strong("logKaw")),
        numericInput("Air_Kaw_atm", label = NULL, value = -6, width = 100),
        numericInput("Water_Kaw_atm", label = NULL, value = -16, width = 100),
        numericInput("Octanol_Kaw_atm", label = NULL, value = -9, width = 100)
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      3,
      h4(strong("Data Aesthetics")),
      sliderInput("point_size_atm", "Point Size", min = 0.5, max = 5, value = 1, step = 0.25),
      sliderInput("label_size_atm", "Label Size", min = 0, max = 6, value = 3, step = 0.25),
      sliderInput("padding_atm", "Strength of Label Repulsion", min = 0.2, max = 0.6, value = 0.4, step = 0.025)
    ),
    column(
      3,
      div(
        style = "white-space: nowrap;",
        h4(strong("Line Thickness"))
      ),
      sliderInput("th_line1_atm", "Threshold Line 1", min = 0, max = 2, value = 1, step = 0.1),
      sliderInput("th_line2_atm", "Threshold Line 2", min = 0, max = 2, value = 1, step = 0.1),
      sliderInput("th_line3_atm", "Threshold Line 3", min = 0, max = 2, value = 1, step = 0.1)
    ),
    column(
      3,
      br(),
      br(),
      sliderInput("media_label_size_atm", "Media Label Size", min = 0, max = 8, value = 4, step = 0.5),
      sliderInput("line_label_size_atm", "Line Label Size", min = 0, max = 4, value = 2, step = 0.2)
    )
  )
)

# Create the ui
ui <- navbarPage(
  "Chemical Space Plot Generator",
  intro_panel,
  howto_panel,
  multi_plot_panel,
  atm_plot_panel
)

# Server Logic ------------------------------------------------------------
server <- function(input, output, session) {

  # 1. Data Loading and Processing ----------------------------------------

  # Read uploaded CSV file
  df_p <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })

  # Fill missing partition ratios (only 2 of 3 required)
  fill_missing <- function(df) {
    df %>%
      rowwise() %>%
      mutate(
        logKow = ifelse(is.na(logKow), logKoa + logKaw, logKow),
        logKoa = ifelse(is.na(logKoa), logKow - logKaw, logKoa),
        logKaw = ifelse(is.na(logKaw), logKow - logKoa, logKaw)
      ) %>%
      ungroup()
  }

  updatedData <- reactive({
    req(df_p())
    fill_missing(df_p())
  })

  # Replace NA Group values with "Unassigned" (only in Group column)
  df_prop <- reactive({
    req(updatedData())
    updatedData() %>%
      mutate(Group = ifelse(is.na(Group), "Unassigned", Group))
  })

  # 2. Multiphase Plot: Line Generation -----------------------------------

  # Generate axis sequences and all distribution lines in one pass
  df_lines_alt <- reactive({
    kaw_seq <- seq(input$y_min, input$y_max, by = 0.001)
    kow_seq <- seq(input$x_min, input$x_max, by = 0.001)

    d <- bind_rows(
      data.frame(Kaw = kaw_seq),
      data.frame(Kow = kow_seq)
    )

    va <- input$vol_air
    vw <- input$vol_water
    vo <- input$vol_octanol
    mA <- input$molperc_A
    mB <- input$molperc_B
    mC <- input$molperc_C

    d %>% mutate(
      # Air lines: Kaw as f(Kow)
      kaw_air_A = log10((-vw - 10^Kow * vo) / (va * (1 - 1 / mA))),
      kaw_air_B = log10((-vw - 10^Kow * vo) / (va * (1 - 1 / mB))),
      kaw_air_C = log10((-vw - 10^Kow * vo) / (va * (1 - 1 / mC))),
      # Air lines: Kow as f(Kaw)
      kow_air_A = log10((-vw - 10^Kaw * va * (1 - 1 / mA)) / vo),
      kow_air_B = log10((-vw - 10^Kaw * va * (1 - 1 / mB)) / vo),
      kow_air_C = log10((-vw - 10^Kaw * va * (1 - 1 / mC)) / vo),
      # Water lines: Kow as f(Kaw)
      kow_water_A = log10((vw / mA - 10^Kaw * va - vw) / vo),
      kow_water_B = log10((vw / mB - 10^Kaw * va - vw) / vo),
      kow_water_C = log10((vw / mC - 10^Kaw * va - vw) / vo),
      # Water lines: Kaw as f(Kow)
      kaw_water_A = log10((vw / mA - 10^Kow * vo - vw) / va),
      kaw_water_B = log10((vw / mB - 10^Kow * vo - vw) / va),
      kaw_water_C = log10((vw / mC - 10^Kow * vo - vw) / va),
      # Octanol lines: Kow as f(Kaw)
      kow_octanol_A = log10((-10^Kaw * va - vw) / (vo * (1 - 1 / mA))),
      kow_octanol_B = log10((-10^Kaw * va - vw) / (vo * (1 - 1 / mB))),
      kow_octanol_C = log10((-10^Kaw * va - vw) / (vo * (1 - 1 / mC))),
      # Octanol lines: Kaw as f(Kow)
      kaw_octanol_A = log10((-vo * (1 - 1 / mA) * 10^Kow - vw) / va),
      kaw_octanol_B = log10((-vo * (1 - 1 / mB) * 10^Kow - vw) / va),
      kaw_octanol_C = log10((-vo * (1 - 1 / mC) * 10^Kow - vw) / va)
    )
  })

  # Egg (curved threshold) lines
  df_egg_lines <- reactive({
    kow_seq <- seq(input$x_min, input$x_max, by = 0.001)
    d <- data.frame(Kow = kow_seq)

    va <- input$vol_air
    vw <- input$vol_water
    vo <- input$vol_octanol
    mB <- input$molperc_B

    d %>% mutate(
      kaw_air_A = log10((-vw - 10^Kow * vo) / (va * (1 - 1 / (1 - mB)))),
      kaw_water_A = log10((vw / (1 - mB) - 10^Kow * vo - vw) / va),
      kaw_octanol_A = log10((-vo * (1 - 1 / (1 - mB)) * 10^Kow - vw) / va)
    )
  })

  # 3. Multiphase Plot: Geometry Generation -------------------------------

  # Helper: compute log10 of threshold ratio
  log_thresh <- reactive({
    log10(input$molperc_B / ((1 - input$molperc_B) / 2))
  })

  log_thresh_full <- reactive({
    log10(input$molperc_B / (1 - input$molperc_B))
  })

  log_vw_vo <- reactive({
    log10(input$vol_water / input$vol_octanol)
  })

  log_vw_va <- reactive({
    log10(input$vol_water / input$vol_air)
  })

  # Vertex points for the chemical space polygon
  df_vertex <- reactive({
    lt <- log_thresh()
    lwo <- log_vw_vo()
    lwa <- log_vw_va()

    data.frame(
      kow_aw = c(lwo, lwo - lt),
      kaw_aw = c(lwa + lt, lwa - lt),
      kow_wo = c(lwo - lt, lwo + lt),
      kaw_wo = c(lwa - lt, lwa),
      kow_oa = c(lwo, lwo + lt),
      kaw_oa = c(lwa, lwa + lt)
    )
  })

  # Named vertex coordinates for display
  right_x <- reactive(log_vw_vo() + log_thresh())
  left_x <- reactive(log_vw_vo() - log_thresh())
  right_x_alt <- reactive(log_vw_vo() + log_thresh_full())
  left_x_alt <- reactive(log_vw_vo() - log_thresh_full())
  top_x <- reactive(log_vw_vo())
  right_y <- reactive(log_vw_va())
  left_y <- reactive(log_vw_va() - log_thresh())
  top_y <- reactive(log_vw_va() + log_thresh())
  left_y_alt <- reactive(log_vw_va() - log_thresh_full())
  top_y_alt <- reactive(log_vw_va() + log_thresh_full())

  # Centroid arrow lines
  df_centroid1 <- reactive({
    data.frame(
      kow_centre = c(log_vw_vo(), log_vw_vo() - log_thresh()),
      kaw_centre = c(log_vw_va(), log_vw_va() - log_thresh())
    )
  })

  df_centroid2 <- reactive({
    data.frame(
      kow_centre = c(log_vw_vo(), log_vw_vo() + log_thresh()),
      kaw_centre = c(log_vw_va(), log_vw_va())
    )
  })

  df_centroid3 <- reactive({
    data.frame(
      kow_centre = c(log_vw_vo(), log_vw_vo()),
      kaw_centre = c(log_vw_va(), log_vw_va() + log_thresh())
    )
  })

  # 4. Multiphase Plot: Rendering -----------------------------------------

  p1 <- reactive({
    dl <- df_lines_alt()

    ggplot() +
      {
        if (input$diagonal_switch) {
          geom_abline(intercept = seq(input$y_min * input$x_max, -input$y_min * input$x_max, by = 1), slope = 1, colour = "#ebebeb")
        }
      } +
      # Triangle threshold indicators
      {
        if (input$triangle_switch) {
          geom_line(data = df_vertex(), colour = input$col_water, linewidth = input$th_triangle, aes(x = kow_aw, y = kaw_aw))
        }
      } +
      {
        if (input$triangle_switch) {
          geom_line(data = df_vertex(), colour = input$col_octanol, linewidth = input$th_triangle, aes(x = kow_wo, y = kaw_wo))
        }
      } +
      {
        if (input$triangle_switch) {
          geom_line(data = df_vertex(), colour = input$col_air, linewidth = input$th_triangle, aes(x = kow_oa, y = kaw_oa))
        }
      } +
      # Centroid threshold indicators
      {
        if (input$centroid_switch) {
          geom_line(data = df_centroid1(), colour = input$col_water, linewidth = input$th_centroid, aes(x = kow_centre, y = kaw_centre), arrow = arrow(length = unit(0.03, "npc"), ends = "first"))
        }
      } +
      {
        if (input$centroid_switch) {
          geom_line(data = df_centroid2(), colour = input$col_octanol, linewidth = input$th_centroid, aes(x = kow_centre, y = kaw_centre), arrow = arrow(length = unit(0.03, "npc")))
        }
      } +
      {
        if (input$centroid_switch) {
          geom_line(data = df_centroid3(), colour = input$col_air, linewidth = input$th_centroid, aes(x = kow_centre, y = kaw_centre), arrow = arrow(length = unit(0.03, "npc")))
        }
      } +
      # Media shading ribbons
      {
        if (input$ribbon_switch) {
          geom_ribbon(data = dl, fill = input$col_air, alpha = 0.2, aes(x = Kow, y = kaw_air_B, ymin = kaw_air_B, ymax = input$y_max + 1))
        }
      } +
      {
        if (input$ribbon_switch) {
          geom_ribbon(data = dl, fill = input$col_water, alpha = 0.2, aes(x = Kow, y = kaw_water_B, ymin = input$y_min - 1, ymax = kaw_water_B))
        }
      } +
      {
        if (input$ribbon_switch) {
          geom_ribbon(data = dl, fill = input$col_octanol, alpha = 0.2, aes(x = Kow, y = kaw_octanol_B, ymin = input$y_min - 1, ymax = kaw_octanol_B))
        }
      } +
      # Media labels
      {
        if (input$media_switch) {
          if (!input$medium_label_switch) {
            annotate(
              geom = "text", x = min(df_vertex()$kow_oa) - (abs(input$x_max) + abs(input$x_min)) / 8, y = max(df_vertex()$kaw_oa) + (abs(input$y_max) + abs(input$y_min)) / 8,
              label = "Air", colour = "black", fontface = "bold", hjust = 0.5, size = input$media_label_size
            )
          } else {
            annotate(
              geom = "text", x = input$Air_Kow, y = input$Air_Kaw,
              label = "Air", colour = "black", fontface = "bold", hjust = 0.5, size = input$media_label_size
            )
          }
        }
      } +
      {
        if (input$media_switch) {
          if (!input$medium_label_switch) {
            annotate(
              geom = "text", x = min(df_vertex()$kow_aw) - (abs(input$x_max) + abs(input$x_min)) / 6, y = min(df_vertex()$kaw_aw) - (abs(input$y_max) + abs(input$y_min)) / 8,
              label = "Water", colour = "black", fontface = "bold", hjust = 0.5, size = input$media_label_size
            )
          } else {
            annotate(
              geom = "text", x = input$Water_Kow, y = input$Water_Kaw,
              label = "Water", colour = "black", fontface = "bold", hjust = 0.5, size = input$media_label_size
            )
          }
        }
      } +
      {
        if (input$media_switch) {
          if (!input$medium_label_switch) {
            annotate(
              geom = "text", x = max(df_vertex()$kow_wo) + (abs(input$x_max) + abs(input$x_min)) / 4, y = max(df_vertex()$kaw_wo) - (abs(input$y_max) + abs(input$y_min)) / 8,
              label = "Organic\nMatter", colour = "black", fontface = "bold", hjust = 0.5, size = input$media_label_size
            )
          } else {
            annotate(
              geom = "text", x = input$Octanol_Kow, y = input$Octanol_Kaw,
              label = "Organic\nMatter", colour = "black", fontface = "bold", hjust = 0.5
            )
          }
        }
      } +
      # Egg (curved) threshold indicators
      {
        if (input$egg_switch) {
          geom_line(data = df_egg_lines(), colour = input$col_octanol, linewidth = input$th_egg, aes(x = Kow, y = kaw_air_A))
        }
      } +
      {
        if (input$egg_switch) {
          geom_line(data = df_egg_lines(), colour = input$col_air, linewidth = input$th_egg, aes(x = Kow, y = kaw_water_A))
        }
      } +
      {
        if (input$egg_switch) {
          geom_line(data = df_egg_lines(), colour = input$col_water, linewidth = input$th_egg, aes(x = Kow, y = kaw_octanol_A))
        }
      } +
      # Egg: Line 1 threshold lines (drawn when egg is active)
      {
        if (input$egg_switch) {
          geom_line(data = dl, colour = input$col_air, linewidth = input$th_line1, aes(x = Kow, y = kaw_air_B))
        }
      } +
      {
        if (input$egg_switch) {
          geom_line(data = dl, colour = input$col_air, linewidth = input$th_line1, aes(x = kow_air_B, y = Kaw))
        }
      } +
      {
        if (input$egg_switch) {
          geom_line(data = dl, colour = input$col_water, linewidth = input$th_line1, aes(x = kow_water_B, y = Kaw))
        }
      } +
      {
        if (input$egg_switch) {
          geom_line(data = dl, colour = input$col_water, linewidth = input$th_line1, aes(x = Kow, y = kaw_water_B))
        }
      } +
      {
        if (input$egg_switch) {
          geom_line(data = dl, colour = input$col_octanol, linewidth = input$th_line1, aes(x = kow_octanol_B, y = Kaw))
        }
      } +
      {
        if (input$egg_switch) {
          geom_line(data = dl, colour = input$col_octanol, linewidth = input$th_line1, aes(x = Kow, y = kaw_octanol_B))
        }
      } +
      # Threshold line 2 (molperc_A)
      {
        if (input$line2_switch) {
          geom_line(data = dl, colour = input$col_air, linewidth = input$th_line2, aes(x = Kow, y = kaw_air_A))
        }
      } +
      # Threshold line 1 (molperc_B)
      {
        if (input$line1_switch) {
          geom_line(data = dl, colour = input$col_air, linewidth = input$th_line1, aes(x = Kow, y = kaw_air_B))
        }
      } +
      # Threshold line 3 (molperc_C)
      {
        if (input$line3_switch) {
          geom_line(data = dl, colour = input$col_air, linewidth = input$th_line3, aes(x = Kow, y = kaw_air_C))
        }
      } +
      {
        if (input$line2_switch) {
          geom_line(data = dl, colour = input$col_air, linewidth = input$th_line2, aes(x = kow_air_A, y = Kaw))
        }
      } +
      {
        if (input$line1_switch) {
          geom_line(data = dl, colour = input$col_air, linewidth = input$th_line1, aes(x = kow_air_B, y = Kaw))
        }
      } +
      {
        if (input$line3_switch) {
          geom_line(data = dl, colour = input$col_air, linewidth = input$th_line3, aes(x = kow_air_C, y = Kaw))
        }
      } +
      {
        if (input$line2_switch) {
          geom_line(data = dl, colour = input$col_water, linewidth = input$th_line2, aes(x = kow_water_A, y = Kaw))
        }
      } +
      {
        if (input$line1_switch) {
          geom_line(data = dl, colour = input$col_water, linewidth = input$th_line1, aes(x = kow_water_B, y = Kaw))
        }
      } +
      {
        if (input$line3_switch) {
          geom_line(data = dl, colour = input$col_water, linewidth = input$th_line3, aes(x = kow_water_C, y = Kaw))
        }
      } +
      {
        if (input$line2_switch) {
          geom_line(data = dl, colour = input$col_water, linewidth = input$th_line2, aes(x = Kow, y = kaw_water_A))
        }
      } +
      {
        if (input$line1_switch) {
          geom_line(data = dl, colour = input$col_water, linewidth = input$th_line1, aes(x = Kow, y = kaw_water_B))
        }
      } +
      {
        if (input$line3_switch) {
          geom_line(data = dl, colour = input$col_water, linewidth = input$th_line3, aes(x = Kow, y = kaw_water_C))
        }
      } +
      {
        if (input$line2_switch) {
          geom_line(data = dl, colour = input$col_octanol, linewidth = input$th_line2, aes(x = kow_octanol_A, y = Kaw))
        }
      } +
      {
        if (input$line1_switch) {
          geom_line(data = dl, colour = input$col_octanol, linewidth = input$th_line1, aes(x = kow_octanol_B, y = Kaw))
        }
      } +
      {
        if (input$line3_switch) {
          geom_line(data = dl, colour = input$col_octanol, linewidth = input$th_line3, aes(x = kow_octanol_C, y = Kaw))
        }
      } +
      {
        if (input$line2_switch) {
          geom_line(data = dl, colour = input$col_octanol, linewidth = input$th_line2, aes(x = Kow, y = kaw_octanol_A))
        }
      } +
      {
        if (input$line1_switch) {
          geom_line(data = dl, colour = input$col_octanol, linewidth = input$th_line1, aes(x = Kow, y = kaw_octanol_B))
        }
      } +
      {
        if (input$line3_switch) {
          geom_line(data = dl, colour = input$col_octanol, linewidth = input$th_line3, aes(x = Kow, y = kaw_octanol_C))
        }
      } +
      # Line labels
      {
        if (input$linelab1_switch) {
          annotate(
            geom = "label", label.padding = unit(0.1, "lines"), x = input$x_min + (abs(input$x_max) + abs(input$x_min)) / 8, y = min(na.omit(dl$kaw_air_B)),
            label = paste0(input$molperc_B * 100, "%"), colour = "black", fontface = "bold", hjust = 0.5, size = input$line_label_size
          )
        }
      } +
      {
        if (input$linelab1_switch) {
          annotate(
            geom = "label", label.padding = unit(0.1, "lines"), x = input$x_min + (abs(input$x_max) + abs(input$x_min)) / 8, y = max(na.omit(dl$kaw_water_B)),
            label = paste0(input$molperc_B * 100, "%"), colour = "black", fontface = "bold", hjust = 0.5, size = input$line_label_size
          )
        }
      } +
      {
        if (input$linelab2_switch) {
          annotate(
            geom = "label", label.padding = unit(0.1, "lines"), x = input$x_min + (abs(input$x_max) + abs(input$x_min)) / 8, y = max(na.omit(dl$kaw_water_A)),
            label = paste0(input$molperc_A * 100, "%"), colour = "black", fontface = "bold", hjust = 0.5, size = input$line_label_size
          )
        }
      } +
      {
        if (input$linelab2_switch) {
          annotate(
            geom = "label", label.padding = unit(0.1, "lines"), x = input$x_min + (abs(input$x_max) + abs(input$x_min)) / 8, y = min(na.omit(dl$kaw_air_A)),
            label = paste0(input$molperc_A * 100, "%"), colour = "black", fontface = "bold", hjust = 0.5, size = input$line_label_size
          )
        }
      } +
      {
        if (input$linelab3_switch) {
          annotate(
            geom = "label", label.padding = unit(0.1, "lines"), x = input$x_min + (abs(input$x_max) + abs(input$x_min)) / 8, y = max(na.omit(dl$kaw_water_C)),
            label = paste0(input$molperc_C * 100, "%"), colour = "black", fontface = "bold", hjust = 0.5, size = input$line_label_size
          )
        }
      } +
      {
        if (input$linelab3_switch) {
          annotate(
            geom = "label", label.padding = unit(0.1, "lines"), x = input$x_min + (abs(input$x_max) + abs(input$x_min)) / 8, y = min(na.omit(dl$kaw_air_C)),
            label = paste0(input$molperc_C * 100, "%"), colour = "black", fontface = "bold", hjust = 0.5, size = input$line_label_size
          )
        }
      } +
      # Data point labels
      {
        if (input$label_switch) {
          geom_text_repel(
            data = df_prop(), size = input$label_size, max.iter = 1000000, segment.color = "dark grey",
            box.padding = unit(input$padding, "lines"),
            aes(x = logKow, y = logKaw, label = Compound, fontface = "bold")
          )
        }
      } +
      # Data points
      {
        if (input$point_switch) {
          geom_point(data = df_prop(), colour = "black", size = input$point_size, aes(x = logKow, y = logKaw))
        }
      } +
      {
        if (input$point_switch && input$colour_switch) {
          geom_point(data = df_prop(), size = input$point_size, aes(x = logKow, y = logKaw, colour = as.factor(Group)))
        }
      } +
      {
        if (input$point_switch && input$colour_switch) {
          scale_color_brewer(palette = input$palette)
        }
      } +
      {
        if (input$point_switch && input$custom_colour_switch) {
          geom_point(data = df_prop(), size = input$point_size, aes(x = logKow, y = logKaw, colour = as.factor(Group)))
        }
      } +
      {
        if (input$point_switch && input$custom_colour_switch) {
          scale_color_manual(values = df_prop()$Group, breaks = df_prop()$Group)
        }
      } +
      coord_cartesian(xlim = c(input$x_min, input$x_max), ylim = c(input$y_min, input$y_max)) +
      scale_x_continuous(expand = c(0, 0), name = bquote(logK[OW]), breaks = seq(input$x_min, input$x_max, length.out = input$numTicks), labels = format_decimal) +
      scale_y_continuous(expand = c(0, 0), name = bquote(logK[AW]), breaks = seq(input$y_min, input$y_max, length.out = input$numTicks), labels = format_decimal) +
      theme_bw() +
      theme(legend.position = "none", plot.margin = margin(r = 15, t = 15, l = 10, b = 10, unit = "pt"))
  })

  output$plot1 <- renderPlot(res = 125, height = reactive(30 * (input$y_max - input$y_min)), width = reactive(30 * (input$x_max - input$x_min)), {
    p1()
  })

  # Download plot (PNG or SVG)
  output$download <- downloadHandler(
    filename = function() {
      paste0("chemical_space_plot.", input$download_format)
    },
    content = function(file) {
      fmt <- input$download_format
      h <- 350 * (input$y_max - input$y_min)
      w <- 350 * (input$x_max - input$x_min)
      if (fmt == "svg") {
        ggsave(file, plot = p1(), device = "svg",
          height = h / 96, width = w / 96, units = "in"
        )
      } else {
        ggsave(file, plot = p1(), device = "png",
          height = h, width = w, units = "px", dpi = 1250
        )
      }
    }
  )

  # Download template
  output$template <- downloadHandler(
    filename = function() {
      "CSP_Template.csv"
    },
    content = function(fname) {
      write.csv(df_template, fname, row.names = FALSE)
    }
  )

  # Download app source code
  output$download_app <- downloadHandler(
    filename = function() {
      "app.R"
    },
    content = function(file) {
      file.copy("app.R", file)
    }
  )

  # Hover coordinates
  output$info <- renderText({
    paste0("logKow = ", input$plot_hover$x, "\nlogKaw = ", input$plot_hover$y)
  })

  # Show critical points
  output$critical_points <- renderText({
    paste0(
      "Air: (", top_x(), ",", top_y(), ")",
      "\nWater: (", left_x(), ",", left_y(), ")",
      "\nOctanol: (", right_x(), ",", right_y(), ")"
    )
  })

  # Reset button
  observeEvent(input$reset, {
    shinyjs::reset("vol_water")
    shinyjs::reset("vol_air")
    shinyjs::reset("vol_octanol")
    shinyjs::reset("molperc_A")
    shinyjs::reset("molperc_B")
    shinyjs::reset("molperc_C")
    shinyjs::reset("col_air")
    shinyjs::reset("col_water")
    shinyjs::reset("col_octanol")
    shinyjs::reset("th_line1")
    shinyjs::reset("th_line2")
    shinyjs::reset("th_line3")
    shinyjs::reset("th_triangle")
    shinyjs::reset("th_centroid")
    shinyjs::reset("th_egg")
    shinyjs::reset("padding")
    shinyjs::reset("Air_Kaw")
    shinyjs::reset("Air_Kow")
    shinyjs::reset("Water_Kaw")
    shinyjs::reset("Water_Kow")
    shinyjs::reset("Octanol_Kaw")
    shinyjs::reset("Octanol_Kow")
    shinyjs::reset("point_size")
    shinyjs::reset("label_size")
    shinyjs::reset("media_label_size")
    shinyjs::reset("line_label_size")
  })

  # 5. Atmospheric Plot: Line Generation ----------------------------------

  # Generate atmospheric axis sequences and all distribution lines in one pass
  df_lines_alt_atm <- reactive({
    kaw_seq <- seq(input$y_min_atm, input$y_max_atm, by = 0.001)
    koa_seq <- seq(input$x_min_atm, input$x_max_atm, by = 0.001)

    d <- bind_rows(
      data.frame(Kaw = kaw_seq),
      data.frame(Koa = koa_seq)
    )

    va <- input$vol_air_atm
    vw <- input$vol_water_atm
    vo <- input$vol_octanol_atm
    mA <- input$molperc_A_atm
    mB <- input$molperc_B_atm
    mC <- input$molperc_C_atm

    d %>% mutate(
      # Cloud lines (hardcoded 50% threshold with specific volumes)
      kaw_air_cloud = log10(30000 / ((1 / 0.5 - 1) * 100000000000 - 10^Koa * 1)),
      koa_air_cloud = log10(-100000000000 / 1 + 100000000000 / (0.5 * 1) - 10^(-Kaw) * 30000 / 1),
      koa_water_cloud = log10(((30000 / 0.5 - 1) - 10^Kaw * 100000000000) / (1 * 10^Kaw)),
      kaw_water_cloud = log10((30000 * (1 / 0.5 - 1)) / (100000000000 + 1 * 10^Koa)),
      koa_octanol_cloud = log10((30000 / 10^Kaw + 100000000000) / (1 * (1 / 0.5 - 1))),
      kaw_octanol_cloud = log10(30000 / ((1 / 0.5 - 1) * 1 * 10^Koa - 100000000000)),
      # Air lines: Kaw as f(Koa)
      kaw_air_A_atm = log10(vw / ((1 / mA - 1) * va - 10^Koa * vo)),
      kaw_air_B_atm = log10(vw / ((1 / mB - 1) * va - 10^Koa * vo)),
      kaw_air_C_atm = log10(vw / ((1 / mC - 1) * va - 10^Koa * vo)),
      # Air lines: Koa as f(Kaw)
      koa_air_A_atm = log10(-va / vo + va / (mA * vo) - 10^(-Kaw) * vw / vo),
      koa_air_B_atm = log10(-va / vo + va / (mB * vo) - 10^(-Kaw) * vw / vo),
      koa_air_C_atm = log10(-va / vo + va / (mC * vo) - 10^(-Kaw) * vw / vo),
      # Water lines: Koa as f(Kaw)
      koa_water_A_atm = log10(((vw / mA - 1) - 10^Kaw * va) / (vo * 10^Kaw)),
      koa_water_B_atm = log10(((vw / mB - 1) - 10^Kaw * va) / (vo * 10^Kaw)),
      koa_water_C_atm = log10(((vw / mC - 1) - 10^Kaw * va) / (vo * 10^Kaw)),
      # Water lines: Kaw as f(Koa)
      kaw_water_A_atm = log10((vw * (1 / mA - 1)) / (va + vo * 10^Koa)),
      kaw_water_B_atm = log10((vw * (1 / mB - 1)) / (va + vo * 10^Koa)),
      kaw_water_C_atm = log10((vw * (1 / mC - 1)) / (va + vo * 10^Koa)),
      # Octanol lines: Koa as f(Kaw)
      koa_octanol_A_atm = log10((vw / 10^Kaw + va) / (vo * (1 / mA - 1))),
      koa_octanol_B_atm = log10((vw / 10^Kaw + va) / (vo * (1 / mB - 1))),
      koa_octanol_C_atm = log10((vw / 10^Kaw + va) / (vo * (1 / mC - 1))),
      # Octanol lines: Kaw as f(Koa)
      kaw_octanol_A_atm = log10(vw / ((1 / mA - 1) * vo * 10^Koa - va)),
      kaw_octanol_B_atm = log10(vw / ((1 / mB - 1) * vo * 10^Koa - va)),
      kaw_octanol_C_atm = log10(vw / ((1 / mC - 1) * vo * 10^Koa - va))
    )
  })

  # 6. Atmospheric Plot: Geometry Generation ------------------------------

  # Helper: atmospheric threshold computations
  log_thresh_atm <- reactive({
    log10(input$molperc_B_atm / ((1 - input$molperc_B_atm) / 2))
  })

  log_thresh_full_atm <- reactive({
    log10(input$molperc_B_atm / (1 - input$molperc_B_atm))
  })

  log_vw_vo_atm <- reactive({
    log10(input$vol_water_atm / input$vol_octanol_atm)
  })

  log_vw_va_atm <- reactive({
    log10(input$vol_water_atm / input$vol_air_atm)
  })

  # Atmospheric vertex points
  df_vertex_atm <- reactive({
    lt <- log_thresh_atm()
    lwo <- log_vw_vo_atm()
    lwa <- log_vw_va_atm()

    data.frame(
      koa_aw_atm = c(lwo, lwo - lt),
      kaw_aw_atm = c(lwa + lt, lwa - lt),
      koa_wo_atm = c(lwo - lt, lwo + lt),
      kaw_wo_atm = c(lwa - lt, lwa),
      koa_oa_atm = c(lwo, lwo + lt),
      kaw_oa_atm = c(lwa, lwa + lt)
    )
  })

  # Atmospheric named vertex coordinates
  right_x_atm <- reactive(log_vw_vo_atm() + log_thresh_atm())
  left_x_atm <- reactive(log_vw_vo_atm() - log_thresh_atm())
  right_x_alt_atm <- reactive(log_vw_vo_atm() + log_thresh_full_atm())
  left_x_alt_atm <- reactive(log_vw_vo_atm() - log_thresh_full_atm())
  top_x_atm <- reactive(log_vw_vo_atm())
  right_y_atm <- reactive(log_vw_va_atm())
  left_y_atm <- reactive(log_vw_va_atm() - log_thresh_atm())
  top_y_atm <- reactive(log_vw_va_atm() + log_thresh_atm())
  left_y_alt_atm <- reactive(log_vw_va_atm() - log_thresh_full_atm())
  top_y_alt_atm <- reactive(log_vw_va_atm() + log_thresh_full_atm())

  # Atmospheric centroid arrow lines
  df_centroid1_atm <- reactive({
    data.frame(
      koa_centre_atm = c(log_vw_vo_atm(), log_vw_vo_atm() - log_thresh_atm()),
      kaw_centre_atm = c(log_vw_va_atm(), log_vw_va_atm() - log_thresh_atm())
    )
  })

  df_centroid2_atm <- reactive({
    data.frame(
      koa_centre_atm = c(log_vw_vo_atm(), log_vw_vo_atm() + log_thresh_atm()),
      kaw_centre_atm = c(log_vw_va_atm(), log_vw_va_atm())
    )
  })

  df_centroid3_atm <- reactive({
    data.frame(
      koa_centre_atm = c(log_vw_vo_atm(), log_vw_vo_atm()),
      kaw_centre_atm = c(log_vw_va_atm(), log_vw_va_atm() + log_thresh_atm())
    )
  })

  # Atmospheric egg lines
  df_egg_lines_atm <- reactive({
    koa_seq <- seq(input$x_min_atm, input$x_max_atm, by = 0.001)
    d <- data.frame(Koa = koa_seq)

    va <- input$vol_air_atm
    vw <- input$vol_water_atm
    vo <- input$vol_octanol_atm
    mB <- input$molperc_B_atm

    d %>% mutate(
      kaw_air_A_atm = log10((-vw - 10^Koa * vo) / (va * (1 - 1 / (1 - mB)))),
      kaw_water_A_atm = log10((vw / (1 - mB) - 10^Koa * vo - vw) / va),
      kaw_octanol_A_atm = log10((-vo * (1 - 1 / (1 - mB)) * 10^Koa - vw) / va)
    )
  })

  # 7. Atmospheric Plot: Rendering ----------------------------------------

  p2 <- reactive({
    dl <- df_lines_alt_atm()

    ggplot() +
      {
        if (input$diagonal_switch_atm) {
          geom_abline(intercept = seq(input$y_min_atm * input$x_max_atm, -input$y_min_atm * input$x_max_atm, by = 1), slope = 1, colour = "#ebebeb")
        }
      } +
      # Media shading ribbons
      {
        if (input$ribbon_switch_atm) {
          geom_ribbon(data = dl, fill = input$col_air_atm, alpha = 0.2, aes(x = Koa, y = kaw_air_B_atm, ymin = kaw_air_B_atm, ymax = input$y_max_atm + 1))
        }
      } +
      {
        if (input$ribbon_switch_atm) {
          geom_ribbon(data = dl, fill = input$col_water_atm, alpha = 0.2, aes(x = Koa, y = kaw_water_B_atm, ymin = input$y_min_atm - 1, ymax = kaw_water_B_atm))
        }
      } +
      {
        if (input$ribbon_switch_atm) {
          geom_ribbon(data = dl, fill = input$col_octanol_atm, alpha = 0.2, aes(x = Koa, y = kaw_octanol_B_atm, ymin = kaw_octanol_B_atm, ymax = input$y_max_atm + 1))
        }
      } +
      # Media labels
      {
        if (input$media_switch_atm) {
          annotate(
            geom = "text", x = input$Air_Koa_atm, y = input$Air_Kaw_atm,
            label = "Air", colour = "black", fontface = "bold", hjust = 0.5, size = input$media_label_size_atm
          )
        }
      } +
      {
        if (input$media_switch_atm) {
          annotate(
            geom = "text", x = input$Water_Koa_atm, y = input$Water_Kaw_atm,
            label = "Water", colour = "black", fontface = "bold", hjust = 0.5, size = input$media_label_size_atm
          )
        }
      } +
      {
        if (input$media_switch_atm) {
          annotate(
            geom = "text", x = input$Octanol_Koa_atm, y = input$Octanol_Kaw_atm,
            label = "Organic\nMatter", colour = "black", fontface = "bold", hjust = 0.5
          )
        }
      } +
      # Threshold line 2
      {
        if (input$line2_switch_atm) {
          geom_line(data = dl, colour = input$col_air_atm, linewidth = input$th_line2_atm, aes(x = Koa, y = kaw_air_A_atm))
        }
      } +
      # Threshold line 1
      {
        if (input$line1_switch_atm) {
          geom_line(data = dl, colour = input$col_air_atm, linewidth = input$th_line1_atm, aes(x = Koa, y = kaw_air_B_atm))
        }
      } +
      # Threshold line 3
      {
        if (input$line3_switch_atm) {
          geom_line(data = dl, colour = input$col_air_atm, linewidth = input$th_line3_atm, aes(x = Koa, y = kaw_air_C_atm))
        }
      } +
      {
        if (input$line2_switch_atm) {
          geom_line(data = dl, colour = input$col_air_atm, linewidth = input$th_line2_atm, aes(x = koa_air_A_atm, y = Kaw))
        }
      } +
      {
        if (input$line1_switch_atm) {
          geom_line(data = dl, colour = input$col_air_atm, linewidth = input$th_line1_atm, aes(x = koa_air_B_atm, y = Kaw))
        }
      } +
      {
        if (input$line3_switch_atm) {
          geom_line(data = dl, colour = input$col_air_atm, linewidth = input$th_line3_atm, aes(x = koa_air_C_atm, y = Kaw))
        }
      } +
      {
        if (input$line2_switch_atm) {
          geom_line(data = dl, colour = input$col_water_atm, linewidth = input$th_line2_atm, aes(x = Koa, y = kaw_water_A_atm))
        }
      } +
      {
        if (input$line1_switch_atm) {
          geom_line(data = dl, colour = input$col_water_atm, linewidth = input$th_line1_atm, aes(x = Koa, y = kaw_water_B_atm))
        }
      } +
      {
        if (input$line3_switch_atm) {
          geom_line(data = dl, colour = input$col_water_atm, linewidth = input$th_line3_atm, aes(x = Koa, y = kaw_water_C_atm))
        }
      } +
      {
        if (input$line2_switch_atm) {
          geom_line(data = dl, colour = input$col_octanol_atm, linewidth = input$th_line2_atm, aes(x = koa_octanol_A_atm, y = Kaw))
        }
      } +
      {
        if (input$line1_switch_atm) {
          geom_line(data = dl, colour = input$col_octanol_atm, linewidth = input$th_line1_atm, aes(x = koa_octanol_B_atm, y = Kaw))
        }
      } +
      {
        if (input$line3_switch_atm) {
          geom_line(data = dl, colour = input$col_octanol_atm, linewidth = input$th_line3_atm, aes(x = koa_octanol_C_atm, y = Kaw))
        }
      } +
      {
        if (input$line2_switch_atm) {
          geom_line(data = dl, colour = input$col_octanol_atm, linewidth = input$th_line2_atm, aes(x = Koa, y = kaw_octanol_A_atm))
        }
      } +
      {
        if (input$line1_switch_atm) {
          geom_line(data = dl, colour = input$col_octanol_atm, linewidth = input$th_line1_atm, aes(x = Koa, y = kaw_octanol_B_atm))
        }
      } +
      {
        if (input$line3_switch_atm) {
          geom_line(data = dl, colour = input$col_octanol_atm, linewidth = input$th_line3_atm, aes(x = Koa, y = kaw_octanol_C_atm))
        }
      } +
      # Cloud threshold lines
      {
        if (input$cloud_switch_atm) {
          geom_line(data = dl, colour = "black", linewidth = 0.5, linetype = "dashed", aes(x = koa_air_cloud, y = Kaw))
        }
      } +
      {
        if (input$cloud_switch_atm) {
          geom_line(data = dl, colour = "black", linewidth = 0.5, linetype = "dashed", aes(x = Koa, y = kaw_water_cloud))
        }
      } +
      {
        if (input$cloud_switch_atm) {
          geom_line(data = dl, colour = "black", linewidth = 0.5, linetype = "dashed", aes(x = koa_octanol_cloud, y = Kaw))
        }
      } +
      # Line labels
      {
        if (input$linelab1_switch_atm) {
          annotate(
            geom = "label", label.padding = unit(0.1, "lines"), x = input$x_min_atm + (abs(input$x_max_atm) + abs(input$x_min_atm)) / 8, y = min(na.omit(dl$kaw_air_B_atm)),
            label = paste0(input$molperc_B_atm * 100, "%"), colour = "black", fontface = "bold", hjust = 0.5, size = input$line_label_size_atm
          )
        }
      } +
      {
        if (input$linelab1_switch_atm) {
          annotate(
            geom = "label", label.padding = unit(0.1, "lines"), x = input$x_min_atm + (abs(input$x_max_atm) + abs(input$x_min_atm)) / 8, y = max(na.omit(dl$kaw_water_B_atm)),
            label = paste0(input$molperc_B_atm * 100, "%"), colour = "black", fontface = "bold", hjust = 0.5, size = input$line_label_size_atm
          )
        }
      } +
      {
        if (input$linelab2_switch_atm) {
          annotate(
            geom = "label", label.padding = unit(0.1, "lines"), x = input$x_min_atm + (abs(input$x_max_atm) + abs(input$x_min_atm)) / 8, y = max(na.omit(dl$kaw_water_A_atm)),
            label = paste0(input$molperc_A_atm * 100, "%"), colour = "black", fontface = "bold", hjust = 0.5, size = input$line_label_size_atm
          )
        }
      } +
      {
        if (input$linelab2_switch_atm) {
          annotate(
            geom = "label", label.padding = unit(0.1, "lines"), x = input$x_min_atm + (abs(input$x_max_atm) + abs(input$x_min_atm)) / 8, y = min(na.omit(dl$kaw_air_A_atm)),
            label = paste0(input$molperc_A_atm * 100, "%"), colour = "black", fontface = "bold", hjust = 0.5, size = input$line_label_size_atm
          )
        }
      } +
      {
        if (input$linelab3_switch_atm) {
          annotate(
            geom = "label", label.padding = unit(0.1, "lines"), x = input$x_min_atm + (abs(input$x_max_atm) + abs(input$x_min_atm)) / 8, y = max(na.omit(dl$kaw_water_C_atm)),
            label = paste0(input$molperc_C_atm * 100, "%"), colour = "black", fontface = "bold", hjust = 0.5, size = input$line_label_size_atm
          )
        }
      } +
      {
        if (input$linelab3_switch_atm) {
          annotate(
            geom = "label", label.padding = unit(0.1, "lines"), x = input$x_min_atm + (abs(input$x_max_atm) + abs(input$x_min_atm)) / 8, y = min(na.omit(dl$kaw_air_C_atm)),
            label = paste0(input$molperc_C_atm * 100, "%"), colour = "black", fontface = "bold", hjust = 0.5, size = input$line_label_size_atm
          )
        }
      } +
      # Data point labels
      {
        if (input$label_switch_atm) {
          geom_text_repel(
            data = df_prop(), size = input$label_size_atm, max.iter = 1000000, segment.color = "dark grey",
            box.padding = unit(input$padding_atm, "lines"),
            aes(x = logKoa, y = logKaw, label = Compound, fontface = "bold")
          )
        }
      } +
      # Data points
      {
        if (input$point_switch_atm) {
          geom_point(data = df_prop(), colour = "black", size = input$point_size_atm, aes(x = logKoa, y = logKaw))
        }
      } +
      {
        if (input$point_switch_atm && input$colour_switch_atm) {
          geom_point(data = df_prop(), size = input$point_size_atm, aes(x = logKoa, y = logKaw, colour = as.factor(Group)))
        }
      } +
      {
        if (input$point_switch_atm && input$colour_switch_atm) {
          scale_color_brewer(palette = input$palette_atm)
        }
      } +
      {
        if (input$point_switch_atm && input$custom_colour_switch_atm) {
          geom_point(data = df_prop(), size = input$point_size_atm, aes(x = logKoa, y = logKaw, colour = as.factor(Group)))
        }
      } +
      {
        if (input$point_switch_atm && input$custom_colour_switch_atm) {
          scale_color_manual(values = df_prop()$Group, breaks = df_prop()$Group)
        }
      } +
      coord_cartesian(xlim = c(input$x_min_atm, input$x_max_atm), ylim = c(input$y_min_atm, input$y_max_atm)) +
      scale_x_continuous(expand = c(0, 0), name = bquote(logK[OA]), breaks = seq(input$x_min_atm, input$x_max_atm, length.out = input$numTicks_atm), labels = format_decimal) +
      scale_y_continuous(expand = c(0, 0), name = bquote(logK[AW]), breaks = seq(input$y_min_atm, input$y_max_atm, length.out = input$numTicks_atm), labels = format_decimal) +
      theme_bw() +
      theme(legend.position = "none", plot.margin = margin(r = 15, t = 15, l = 10, b = 10, unit = "pt"))
  })

  output$plot2 <- renderPlot(res = 125, height = reactive(30 * (input$y_max_atm - input$y_min_atm)), width = reactive(30 * (input$x_max_atm - input$x_min_atm)), {
    p2()
  })

  # Download atmospheric plot (PNG or SVG)
  output$download_atm <- downloadHandler(
    filename = function() {
      paste0("atmospheric_plot.", input$download_format_atm)
    },
    content = function(file_atm) {
      fmt <- input$download_format_atm
      h <- 350 * (input$y_max_atm - input$y_min_atm)
      w <- 350 * (input$x_max_atm - input$x_min_atm)
      if (fmt == "svg") {
        ggsave(file_atm, plot = p2(), device = "svg",
          height = h / 96, width = w / 96, units = "in"
        )
      } else {
        ggsave(file_atm, plot = p2(), device = "png",
          height = h, width = w, units = "px", dpi = 1250
        )
      }
    }
  )

  # Hover on atmospheric plot
  output$info_atm <- renderText({
    paste0("logKoa = ", input$plot_hover_atm$x, "\nlogKaw = ", input$plot_hover_atm$y)
  })

  # Atmospheric reset button (fixed: was bound to input$reset instead of input$reset_atm)
  observeEvent(input$reset_atm, {
    shinyjs::reset("vol_water_atm")
    shinyjs::reset("vol_air_atm")
    shinyjs::reset("vol_octanol_atm")
    shinyjs::reset("molperc_A_atm")
    shinyjs::reset("molperc_B_atm")
    shinyjs::reset("molperc_C_atm")
    shinyjs::reset("col_air_atm")
    shinyjs::reset("col_water_atm")
    shinyjs::reset("col_octanol_atm")
    shinyjs::reset("th_line1_atm")
    shinyjs::reset("th_line2_atm")
    shinyjs::reset("th_line3_atm")
    shinyjs::reset("padding_atm")
    shinyjs::reset("Air_Kaw_atm")
    shinyjs::reset("Air_Koa_atm")
    shinyjs::reset("Water_Kaw_atm")
    shinyjs::reset("Water_Koa_atm")
    shinyjs::reset("Octanol_Kaw_atm")
    shinyjs::reset("Octanol_Koa_atm")
    shinyjs::reset("point_size_atm")
    shinyjs::reset("label_size_atm")
    shinyjs::reset("media_label_size_atm")
    shinyjs::reset("line_label_size_atm")
  })
}

shinyApp(ui, server)
