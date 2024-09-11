# Load necessary libraries
library(shiny)
library(dplyr)
library(tidyr)
library(forcats)
library(purrr)
library(readr)
library(tibble)
library(metafor)
library(janitor)
library(ggplot2)
library(scales)
library(inflection)

example_summary_statistics <- read_csv("example_summary_statistics.csv")

# Define the UI
ui <- fluidPage(
  
  # Title
  titlePanel("SE/SD errors in meta-analyses"),
  tags$h3("Scree-like plot of heterogeneity assuming N number of SE/SD errors by Tau^2 if deleted"),
  
  # Sidebar layout for file input and outputs
  sidebarLayout(
    
    # Sidebar for uploading the file
    sidebarPanel(
      downloadButton("downloadExample", "Download Example CSV"), 
      actionButton("useExampleData", "Use Example Data"),  # New button for using example data
      tags$hr(),
      fileInput("file1", "Upload CSV File", accept = ".csv")
    ),
    
    # Main panel to display the plots and table
    mainPanel(
      plotOutput("tau2Plot"),
      plotOutput("h2Plot")
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
    
    return(tibble(smd = res$b[, 1],  # Meta effect size
                  ci_lb = res$ci.lb, # Lower bound of confidence interval
                  ci_ub = res$ci.ub, # Upper bound of confidence interval
                  tau2 = res$tau2,   # Tau-squared (between-study variance)
                  I2 = res$I2,       # I-squared (percentage of variability due to heterogeneity)
                  H2 = res$H2))      # H-squared (relative excess variability due to heterogeneity)
  }
  
  # Function to replace extreme effect sizes
  replace_mislabelled_se_with_sd <- function(dat, indices) {
    dat_corrected <- dat %>%
      mutate(sd1 = if_else(row_number() %in% indices, sd1_alt, sd1),
             sd2 = if_else(row_number() %in% indices, sd2_alt, sd2))
    
    es_corrected <- escalc(measure = "SMD", 
                           m1i = m1, 
                           sd1i = sd1, 
                           n1i = n1,
                           m2i = m2, 
                           sd2i = sd2, 
                           n2i = n2,
                           data = dat_corrected)
    
    res <- rma(yi = yi, vi = vi, data = es_corrected)
    
    return(tibble(tau2 = res$tau2, I2 = res$I2, H2 = res$H2))
  }
  
  # Identify the N most extreme effect sizes (in either direction)
  identify_extreme_studies <- function(dat, n_extremes) {
    es <- escalc(measure = "SMD", 
                 m1i = m1, 
                 sd1i = sd1, 
                 n1i = n1,
                 m2i = m2, 
                 sd2i = sd2, 
                 n2i = n2,
                 data = dat)
    
    # Run meta-analysis
    res <- rma(yi = yi, vi = vi, data = es)
    
    # Get tau2_if_deleted for all studies
    res_inf <- influence(res)
    
    # Get the indices of the N studies with the lowest tau2_if_deleted
    extreme_indices <- order(res_inf$inf$tau2.del, decreasing = FALSE)[1:n_extremes]
    
    return(extreme_indices)
  }
  
  # Function to compute Tau2, H2, and I2 values for all n_extremes from 0 to nrow(df)
  compute_metrics_across_extremes <- reactive({
    df <- reactive_data()
    req(df)
    
    df <- df %>%
      mutate(sd1_alt = sd1 * sqrt(n1),  
             sd2_alt = sd2 * sqrt(n2))
    
    n_studies <- nrow(df)
    
    metric_values <- map_df(0:n_studies, function(n_extremes) {
      if (n_extremes == 0) {
        return(tibble(n_extremes = 0, rma_es(df)[c("tau2", "H2", "I2")]))
      }
      
      extreme_indices <- identify_extreme_studies(df, n_extremes)
      metrics_res <- replace_mislabelled_se_with_sd(df, extreme_indices)
      
      return(tibble(n_extremes = n_extremes, metrics_res))
    })
    
    return(metric_values)
  })
  
  # Function to find the elbow point
  find_elbow <- function(values) {
    n <- length(values)
    x <- 0:(n - 1)  # Number of effects removed
    
    # Using the inflection package to find the elbow (maximum curvature)
    elbow <- inflection::uik(x, values)
    return(elbow - 1)  # Returns the index of the elbow, assuming that it always starts with zero.
  }
  
  # Render the plot with Tau2 values across extremes, including the elbow point
  output$tau2Plot <- renderPlot({
    metric_values <- compute_metrics_across_extremes()
    
    ggplot(metric_values, aes(x = n_extremes, y = tau2)) +
      geom_vline(xintercept = find_elbow(metric_values$tau2), linetype = "dashed", color = "red") +
      geom_vline(xintercept = find_elbow(metric_values$tau2) + 1, linetype = "dashed", color = "blue") +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = nrow(metric_values))) +  # Breaks at every integer
      labs(
        title = "Tau² Across Recalculated Studies by Tau²-if-deleted",
        x = "Number of Studies Recalculated",
        y = "Tau²"
      ) +
      theme_linedraw()
  })
  
  # Render the plot with H2 values across extremes, including the elbow point
  output$h2Plot <- renderPlot({
    metric_values <- compute_metrics_across_extremes()
    
    ggplot(metric_values, aes(x = n_extremes, y = H2)) +
      geom_vline(xintercept = find_elbow(metric_values$H2), linetype = "dashed", color = "red") +
      geom_vline(xintercept = find_elbow(metric_values$H2) + 1, linetype = "dashed", color = "blue") +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = nrow(metric_values))) +  # Breaks at every integer
      labs(
        title = "H² Across Recalculated Studies by Tau²-if-deleted",
        x = "Number of Studies Recalculated",
        y = "H²"
      ) +
      theme_linedraw()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
