# Initialisation ----------------------------------------------------------

library(shiny)

plot_curve <- function(df,
                       BL_bounds = c(NA, NA),
                       ATP_bounds = c(),
                       ATP_peak = NA,
                       Iono_peak = NA,
                       mult_peaks = c()){
  # Plot curve
  #
  # Args:
  # df: dataframe for one experiment
  # BL_bounds: Basal level time bounds
  # ATP bounds: ATP response time bounds
  # ATP_peak: Time of ATP peak
  # Iono_peak: Time of Ionomycin peak
  # mult_peaks: peaks
  #
  # Returns:
  # Ggplot
  
  library(ggplot2)
  
  palette <- c("#009E73", "#56B4E9", "#F0E442", "#E69F00", "#D55E00")

  p <- ggplot() +
    geom_line(data = df,
              aes(x = t, y = y),
              size = 2) +
    labs(x = "Time", y = "Ratio") +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, max(df$y * 1.05, na.rm = TRUE))) +
    theme_classic(base_size = 20)
  
  # Basal level (green)
  p <- p + geom_segment(data = subset(df, t %in% BL_bounds),
                        aes(x = t, xend = t, y = y),
                        yend = 0,
                        colour = palette[1],
                        size = 2)
  if (sum(is.na(BL_bounds)) == 0) {
    p <- p + geom_area(data = subset(df, t >= min(BL_bounds) &  t <= max(BL_bounds)),
                       aes(x = t, y = y),
                       fill = palette[1],
                       alpha = .5)
  }
  
  # ATP response (blue)
  p <- p + geom_segment(data =  subset(df, t %in% ATP_bounds),
                        aes(x = t, xend = t, y = y),
                        yend = 0,
                        colour = palette[2],
                        size = 2)
  if (sum(is.na(ATP_bounds)) == 0) {
    p <- p + geom_area(data = subset(df, t >= min(ATP_bounds) &  t <= max(ATP_bounds)),
                       aes(x = t, y = y),
                       fill = palette[2],
                       alpha = .5)
  }
  
  # ATP peak (yellow)
  if (!is.na(ATP_peak)) {
    p <- p + geom_point(data = df[which.min(abs(df$y - ATP_peak)), ],
                        aes(x = t, y = y),
                        size = 4,
                        colour = palette[3])
  }
  
  # Ionomycin peak (yellow)
  if (!is.na(Iono_peak)) {
    p <- p + geom_point(data = df[which.min(abs(df$y - Iono_peak)), ],
                        aes(x = t, y = y),
                        size = 4,
                        colour = palette[4])
  }
  
  # Average peaks (red)
  if (length(mult_peaks) > 0) {
    tmp_peaks <- do.call(rbind,
                         lapply(mult_peaks,
                                function(pk) {
                                  df[which.min(abs(df$y - pk)), ]
                                }))
    
    p <- p + geom_point(data = tmp_peaks,
                        aes(x = t, y = y),
                        size = 4,
                        colour = palette[5])
    
  }
  
  return(p)
}

# User Interface ----------------------------------------------------------

ui <- fluidPage(
  titlePanel("Calcium Imaging Application"),
  tabsetPanel(
    tabPanel("Curve analysis",
             sidebarLayout(
               sidebarPanel(
                 tags$h3("Load data"),
                 # File Input
                 fileInput(inputId = "data",
                           label = "Choose CSV File",
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                 ),
                 # Header
                 checkboxInput("header", "Header", TRUE),
                 # Separator
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ","),
                 # Missing values
                 textInput("na","Treat as missing"),
                 "Please separate strings with a comma",
                 tags$hr(), # Horizontal line
                 tags$h3("Display data"),
                 checkboxInput("show_10", "Show all", FALSE), # Show full or head of the dataset
                 numericInput("dig", label = "Number of decimal places", value = 3), # Number of decimal places
                 tags$hr(), # Horizontal line
                 tags$h3("Process data"),
                 uiOutput("exp_select"), # Which experiment to select
                 uiOutput("click_select") # Radiobutton for selecting which feature to set
               ),
               mainPanel(
                 tableOutput("disp_data"), # View data
                 plotOutput("plot", click = "plot_click"), # Plot
                 tableOutput("disp_stats"), # Statistics
                 uiOutput("submit_btn"), # Submit
                 tableOutput("disp_results"), # Results
                 uiOutput("download") # Download data
               )
             )
    ),
    tabPanel("Help",
             tags$h2("User guide"),
             tags$p(
               "This application provides a framework to perform a semi-automatic curve analysis for calcium imaging data."
             ),
             tags$p(
               "It aims to facilitate the derivation of the basal level, ATP response duration, ATP peak and ionomycin peak."
             ),
             tags$h3("Preparing the data"),
             tags$p(
               "The data should be uploaded as a single", tags$b("csv file"), " with the first column representing time and the other columns representing experiment.
               Each experiment should have its own column: for instance, if there are 5 experiments, the csv file should consists in 6 columns."
             ),
             tags$p(
               "Click on ", tags$b("Browse"), " on the left panel to upload the csv file.
               If the file has a ", tags$b("header"), ", please click the corresponding box.
               Please select the ", tags$b("separator"), " used in the csv file.
               If ", tags$b("missing values"), " are present in the data, please enter the strings that should be considered as missing values in the corresponding text box."
             ),
             tags$p(
               "The data will be shown in the right panel.
               You will have the possibility to display the entire dataset by clicking on ", tags$b("Show all"),", otherwise, only the first 10 rows will be displayed.
               You can also change the ", tags$b("number of decimal places"), " to display in the table.
               If the data was processed correctly, the time series of the first experiment will be displayed under the table"
             ),
             tags$h3("Computing statistics"),
             tags$p(
               "Select the ", tags$b("experiment"), " you would like to extract the statistics from in the drop-down selection box.
               If an header is provided, experiments are named according to the header, otherwise, they are assigned a number:
               the first experiment corresponds to the second column (the first column is time), the second experiment corresponds to the third column, etc."
             ),
             tags$p(
               "The basal level is computed as the average response (y axis) between two points (bounds).
               To select the lower bound, click on ", tags$b("Basal level lower bound"), ", then click on the plot where to set this bound. 
               Once you have clicked, a vertical line will appear corresponding to the bound you have set.
               If you are not happy with this value, you can click again on the plot to change it.
               Repeat the process for the ", tags$b("Basal level upper bound"), ".
               When both the lower bound and upper bound are selected, the area where the average is computed is highlighted in green.
               The basal level will be displayed in the table below the graph."
             ),
             tags$p(
               "The ATP response duration is computed as the time difference between the ", tags$b("ATP response upper bound"), " and the ", tags$b("ATP response lower bound"), ".
               You can select these bounds similarly to how the basal level bounds are selected.
               The ATP response will appear highlighted in blue."
             ),
             tags$p(
               "To select the ATP peak, select ", tags$b("ATP peak"), " in the left panel and click on the plot where the peak is located.
               The identified point will appear in yellow and its value will be displayed on the table below the graph.
               If you have clicked close enough to the peak, the peak (maximum) will be automatically selected."
             ),
             tags$p(
               "The ionomycin peak can be derived similarly by selecting ", tags$b("Ionomycin peak"), " on the left panel.
               It will appear in orange."
             ),
             tags$h3("Saving results"),
             tags$p(
               "The derived statistics can be saved by clicking the button ", tags$b("submit"), ": a new table will appear containing the saved values.
               When this is done, you can change experiment in the drop-down selection box, compute new statistics and saved them; or export the results as a csv file by clicking on ", tags$b("Save"), "."),
             tags$h2("Notes"),
             tags$p(
               "This app is available for free but please acknowledge it in your work.
               The code (R language for statistical computing, using the Shiny package) is open-source and available on ",
               tags$a(href = "https://github.com/ghurault/calcium_imaging_app", "my GitHub"), "."
             ),
             tags$p(
               "Please report any issues ",
               tags$a(href = "https://github.com/ghurault/calcium_imaging_app/issues", "here"), "."
             ),
             tags$p(
               "For questions, email ",
               tags$a(href = "mailto:guillem.hurault@hotmail.fr", "guillem.hurault@hotmail.fr")
             )
    )
  )
  
  
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  # Read data
  df0 <- reactive({
    req(input$data)
    tmp <- read.csv(input$data$datapath,
                    header = input$header,
                    sep = input$sep,
                    na.strings = c(strsplit(input$na,",")[[1]], "", "NaN"))
    if (input$header) {
      colnames(tmp)[1] <- "t"
    } else {
      colnames(tmp) <- c("t", 1:(ncol(tmp) - 1))
    }
    
    return(tmp)
  })
  
  # Display input data
  output$disp_data <- renderTable({
    if (!input$show_10) {
      head(df0())
    } else {
      df0()
    }
  }, digits = function() {input$dig})
  
  # Select experiment
  output$exp_select <- renderUI({
    req(df0())
    if (is_valid()) {
      selectInput("experiment",
                  label = "Select experiment",
                  choices = colnames(df0())[-1],
                  multiple = FALSE)
    }
  })
  
  # Whether the data is valid
  is_valid <- reactive({
    req(df0())
    tmp <- ( ncol(df0()) > 1)
    for (i in 1:ncol(df0())) {
      tmp & is.numeric(df0()[, i])
    }
    return(tmp)
  })
  
  # Process data
  df <- reactive({
    req(is_valid(), input$experiment)
    if (is_valid()) {
      tmp <- reshape2::melt(df0(), id.vars = "t", variable.name = "Experiment", value.name = "y")
      subset(tmp, Experiment == input$experiment)
    }
  })
  
  # Display plot
  output$plot <- renderPlot({
    req(df())
    plot_curve(df(),
               BL_bounds = c(BL_lower(), BL_upper()),
               ATP_bounds = c(ATP_lower(), ATP_upper()),
               ATP_peak = ATP_peak(),
               Iono_peak = Iono_peak(),
               mult_peaks = mult_peaks()
    )
  })
  
  # Feature selection
  output$click_select <- renderUI({
    req(is_valid())
    if (is_valid()) {
      radioButtons("features",
                   label = "Select value to set",
                   choices = c("None" = "none",
                               "Basal level lower bound" = "BL_lower_btn",
                               "Basal level upper bound" = "BL_upper_btn",
                               "ATP response lower bound" = "ATP_lower_btn",
                               "ATP response upper bound" = "ATP_upper_btn",
                               "ATP peak" = "ATP_peak_btn",
                               "Ionomycin peak" = "Iono_peak_btn",
                               "Average peaks" = "avg_peak_btn"))
    }
  })
  
  # Values
  BL_lower <- reactiveVal(value = NA)
  BL_upper <- reactiveVal(value = NA)
  ATP_lower <- reactiveVal(value = NA)
  ATP_upper <- reactiveVal(value = NA)
  ATP_peak <- reactiveVal(value = NA)
  Iono_peak <- reactiveVal(value = NA)
  mult_peaks <- reactiveVal(value = c())
  # Reset values when changing experiment
  observeEvent(input$experiment, {
    BL_lower(NA)
    BL_upper(NA)
    ATP_lower(NA)
    ATP_upper(NA)
    ATP_peak(NA)
    Iono_peak(NA)
    mult_peaks(numeric(0))
  })
  
  # Action when there is a click
  observeEvent(input$plot_click,
               {
                 nearest <- nearPoints(df(), input$plot_click, maxpoints = 1, threshold = Inf)$t
                 max_around <- max(nearPoints(df(), input$plot_click, maxpoints = max(10, ceiling(nrow(df()) / 50)), threshold = Inf)$y)
                 if (input$features == "BL_lower_btn") {
                   BL_lower(nearest)
                 } else if (input$features == "BL_upper_btn") {
                   BL_upper(nearest)
                 } else if (input$features == "ATP_lower_btn") {
                   ATP_lower(nearest)
                 } else if (input$features == "ATP_upper_btn") {
                   ATP_upper(nearest)
                 } else if (input$features == "ATP_peak_btn") {
                   ATP_peak(max_around)
                 } else if (input$features == "Iono_peak_btn") {
                   Iono_peak(max_around)
                 } else if (input$features == "avg_peak_btn") {
                   mult_peaks(c(mult_peaks(), max_around))
                   # Do something
                 }
               }
  )
  
  # Basal level
  BL <- reactive({
    req(df())
    if (!is.na(BL_lower()) & !is.na(BL_upper())) {
      tmp <- c(BL_lower(), BL_upper())
      mean(df()$y[df()$t >= min(tmp) & df()$t <= max(tmp)], na.rm = TRUE)
    } else {
      NA
    }
  })
  
  # ATP response duration
  ATP <- reactive({
    req(df())
    if (!is.na(ATP_lower()) & !is.na(ATP_upper())) {
      abs(ATP_upper() - ATP_lower())
    } else {
      NA
    }
  })
  
  # Average peaks
  avg_peak <- reactive({
    req(df())
    mean(mult_peaks())
  })
  
  # Statistics
  stats <- reactive({
    req(df())
    tmp <- data.frame(BL(), ATP(), ATP_peak(), Iono_peak(), avg_peak())
    colnames(tmp) <- c("Basal level", "ATP response duration", "ATP peak", "Ionomycin peak", "Average peaks")
    return(tmp)
  })
  
  # Display statistics
  output$disp_stats <- renderTable({stats()}, digits = function() {input$dig})
  
  # Results
  res <- reactiveVal()

  # Submit button
  output$submit_btn <- renderUI({
    req(is_valid())
    actionButton("submit", "Submit")
  })
  
  # Submit action
  observeEvent(input$submit, {
    tmp <- cbind(data.frame(Experiment = input$experiment), stats())
    if (is.null(res())) {
      res(tmp)
    } else {
      res(rbind(tmp, res()))
    }
  })
  
  # Display results
  output$disp_results <- renderTable({res()}, digits = function() {input$dig})
  
  # Download data button
  output$download <- renderUI({
    if (!is.null(res())) {
      downloadButton("downloadData", "Save")
    }
  })
  
  # Download data
  output$downloadData <- downloadHandler(
    filename = function() {"download.csv"},
    content = function(file) {
      write.csv(res(), file, row.names = FALSE)
    }
  )
  
}

# App ---------------------------------------------------------------------

shinyApp(ui = ui, server = server)
