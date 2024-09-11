# Load necessary libraries
library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(metafor)
library(janitor)
library(ggplot2)

# Load the example data
example_summary_statistics <- read_csv("example_summary_statistics.csv")

# Define the UI
ui <- fluidPage(
  
  # Title
  titlePanel("SE/SD errors in meta-analyses"),
  tags$h3("Recalculate all permutations of meta-analysis effect size for N number of SE/SD errors"),
  tags$h4("Note that this takes a few seconds to run"),
  
  # Sidebar layout for file input and outputs
  sidebarLayout(
    
    # Sidebar for uploading the file
    sidebarPanel(
      downloadButton("downloadExample", "Download Example CSV"), 
      actionButton("useExampleData", "Use Example Data"),  # New button for using example data
      tags$hr(),
      fileInput("file1", "Upload CSV File", accept = ".csv"),
      numericInput("max_errors", "Max errors:", value = 6, min = 1)
    ),
    
    # Main panel to display the plot and table
    mainPanel(
      plotOutput("smdPlot"),
      tableOutput("smdTable")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive expression to read and store either the uploaded or example CSV data
  reactive_data <- reactive({
    if (!is.null(input$file1)) {
      df <- read_csv(input$file1$datapath)
    } else if (input$useExampleData > 0) {
      df <- example_summary_statistics
    } else {
      return(NULL)
    }
    
    return(df)
  })
  
  # Reactive expression to check if data is available
  data_available <- reactive({
    !is.null(reactive_data())
  })
  
  # Function to calculate random-effects meta-analysis
  rma_es <- function(dat) {
    es <- escalc(measure = "SMD", 
                 m1i = m1, 
                 sd1i = sd1, 
                 n1i = n1,
                 m2i = m2, 
                 sd2i = sd2, 
                 n2i = n2,
                 data = dat)
    
    res <- rma(yi = yi, vi = vi, data = es)
    
    return(res$b[, 1])
  }
  
  # Function to replace N out
  replace_mislabelled_se_with_sd <- function(dat, indices) {
    dat_corrected <- dat %>%
      mutate(sd1 = if_else(row_number() %in% indices, sd1_alt, sd1),
             sd2 = if_else(row_number() %in% indices, sd2_alt, sd2))
    
    res <- rma_es(dat = dat_corrected)
    
    return(res)
  }
  
  # Generate combinations of rows
  generate_combinations <- function(dat, n) {
    combn(nrow(dat), n, simplify = FALSE)
  }
  
  # Calculate corrected SMD for each combination
  smd_corrected_for_errors_single <- function(dat, n_errors) {
    
    # in each case, "sd" actually represents a mislabelled SE, so recalculate the real SD from the SE ("SD") and N
    dat <- dat %>%
      mutate(sd1_alt = sd1 * sqrt(n1),  
             sd2_alt = sd2 * sqrt(n2))
    
    combinations <- generate_combinations(dat, n_errors)
    
    corrected_smd_values <- map_dbl(combinations, ~ replace_mislabelled_se_with_sd(dat, .x))
    
    return(tibble(smd_corrected = corrected_smd_values))
  }
  
  # Calculate SMD for different levels of errors, for multiple values of n errors
  smd_corrected_for_errors <- function(dat, max_errors = NULL) {
    if (is.null(max_errors)) {
      max_errors <- nrow(dat)
    }
    
    res <- map_dfr(1:max_errors, function(n_errors) {
      smd_corrected_for_errors_single(dat = dat, n_errors = n_errors) |>
        mutate(n_errors = n_errors)
    })
    
    return(res)
  }
  
  # Observe file input or use example data
  observe({
    req(data_available())  # Ensure that no errors are displayed before data is available
    
    df <- reactive_data()  # Get the data (either uploaded or example)
    
    res <- smd_corrected_for_errors(df, max_errors = input$max_errors)
    
    res_summary <- res |>
      group_by(n_errors) |>
      summarize(smd_corrected_median = median(smd_corrected),
                smd_corrected_mad = mad(smd_corrected),
                smd_corrected_min = min(smd_corrected),
                smd_corrected_max = max(smd_corrected)) |>
      mutate(smd_original = rma_es(dat = df))
    
    # Output the plot
    output$smdPlot <- renderPlot({
      ggplot(res, aes(smd_corrected)) +
        geom_histogram(binwidth = 0.05, alpha = 0.7) +
        geom_vline(aes(xintercept = rma_es(dat = df)), linetype = "dashed", color = "blue") +
        geom_vline(data = res_summary, aes(xintercept = smd_corrected_median), linetype = "dashed", color = "red") +
        facet_wrap(~ n_errors, scales = "free_y") +
        theme_linedraw() +
        xlab("Standardized Mean Difference")
    })
    
    # Output the summary table
    output$smdTable <- renderTable({
      res_summary |>
        mutate(n_errors = as.factor(n_errors)) |>
        mutate(across(where(is.numeric), round_half_up, digits = 2)) |>
        select(`SMD original` = smd_original, 
               `N errors` = n_errors,
               `SMD corrected median` = smd_corrected_median,
               `SMD corrected MAD` = smd_corrected_mad,
               `SMD corrected min` = smd_corrected_min,
               `SMD corrected max` = smd_corrected_max)
    })
  })
  
  # Download handler for the preformatted CSV file
  output$downloadExample <- downloadHandler(
    filename = function() {
      "example_summary_statistics.csv"
    },
    content = function(file) {
      write.csv(example_summary_statistics, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
