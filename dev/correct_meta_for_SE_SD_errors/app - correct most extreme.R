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
library(ggstance)

example_summary_statistics <- read_csv("example_summary_statistics.csv")

# Define the UI
ui <- fluidPage(
  
  # Title
  titlePanel("SE/SD errors in meta-analyses"),
  tags$h3("Recalculate the N most extreme effect sizes as if they represent SE/SD errors"),
  
  
  # Sidebar layout for file input and outputs
  sidebarLayout(
    
    # Sidebar for uploading the file
    sidebarPanel(
      downloadButton("downloadExample", "Download Example CSV"), 
      actionButton("useExampleData", "Use Example Data"),  # New button for using example data
      tags$hr(),
      fileInput("file1", "Upload CSV File", accept = ".csv"),
      numericInput("n_extremes", "Number of extreme studies to recalculate:", value = 1, min = 1, max = Inf)
    ),
    
    # Main panel to display the plot and table
    mainPanel(
      plotOutput("smdPlot"),
      tableOutput("smdTable"),
      tableOutput("correctedStudies")  # Table output for corrected study names and SMD effect sizes
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
    
    # Update the max value of n_extremes based on the number of studies in the data frame
    updateNumericInput(session, "n_extremes", max = nrow(df))
    
    return(df)
  })
  
  # Reactive expression to check if data is available
  data_available <- reactive({
    !is.null(reactive_data())
  })
  
  # Function to calculate random-effects meta-analysis
  rma_es <- function(dat){
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
    
    return(list(results = tibble(smd = res$b[, 1],  # Meta effect size
                                 ci_lb = res$ci.lb, # Lower bound of confidence interval
                                 ci_ub = res$ci.ub, # Upper bound of confidence interval
                                 tau2 = res$tau2,   # Tau-squared (between-study variance)
                                 I2 = res$I2,       # I-squared (percentage of variability due to heterogeneity)
                                 H2 = res$H2), 
                es_corrected = es_corrected))
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
    
    # Get the indices of the N most extreme effect sizes
    extreme_indices <- order(abs(es$yi), decreasing = TRUE)[1:n_extremes]
    
    # Store both original and recalculated SMD values in the reactive value
    extreme_data <- tibble(
      index = extreme_indices,
      Original_SMD = es$yi[extreme_indices]  # Original SMD effect sizes for the extreme studies
    )
    
    reactive_extreme_data(extreme_data)
    
    return(extreme_indices)
  }
  
  # Reactive values to store extreme study indices and their SMDs
  reactive_extreme_data <- reactiveVal()
  
  # Recalculate meta effect size based on the most extreme studies
  smd_corrected_for_extreme_errors <- reactive({
    df <- reactive_data()
    req(input$n_extremes)
    
    # Recalculate SD from SE for all studies
    df <- df %>%
      mutate(sd1_alt = sd1 * sqrt(n1),  
             sd2_alt = sd2 * sqrt(n2))
    
    # Identify the most extreme studies
    extreme_indices <- identify_extreme_studies(df, input$n_extremes)
    
    # Replace the SDs of these studies and recalculate the meta effect size
    corrected_smd <- replace_mislabelled_se_with_sd(df, extreme_indices)
    
    return(corrected_smd)
  })
  
  # Reactive expression to calculate original and recalculated effect sizes
  reactive_results <- reactive({
    df <- reactive_data()
    
    # Calculate original meta-analysis result with CIs
    original_res <- rma_es(dat = df)
    
    # Calculate recalculated meta effect size based on the most extreme errors
    recalculated_res <- smd_corrected_for_extreme_errors()
    
    list(original_res = original_res, recalculated_res = recalculated_res)
  })
  
  # Render the plot with updated data
  output$smdPlot <- renderPlot({
    req(data_available())  # Ensure data is available
    
    results <- reactive_results()
    
    # Create a combined data frame for plotting
    plot_data <- tibble(
      Effect_Type = c("Original", "Recalculated"),
      smd_value = c(results$original_res$smd, results$recalculated_res$results$smd),
      ci_lb = c(results$original_res$ci_lb, results$recalculated_res$results$ci_lb),  
      ci_ub = c(results$original_res$ci_ub, results$recalculated_res$results$ci_ub)
    )
    
    # Output the plot with points and CIs
    ggplot(plot_data, aes(x = smd_value, y = fct_rev(Effect_Type))) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      geom_point(size = 4) +  # Plot points for the effect sizes
      geom_linerangeh(aes(xmin = ci_lb, xmax = ci_ub), linetype = "solid", color = "black", size = 1) +  # Plot CIs
      theme_linedraw() +
      scale_x_continuous(breaks = breaks_pretty(n = 7)) +
      ylab("") +
      xlab("SMD meta effect-size") 
  })
  
  # Render the summary table with updated data
  output$smdTable <- renderTable({
    req(data_available())  # Ensure data is available
    
    results <- reactive_results()
    
    results$original_res |>
      mutate(Effect_Type = "Original") |>
      bind_rows(
        results$recalculated_res$results |>
          mutate(Effect_Type = "Recalculated")
      ) |>
      mutate(across(where(is.numeric), round_half_up, digits = 2)) |>
      select(Type = Effect_Type,
             `SMD meta effect size` = smd, 
             `CI lower` = ci_lb,	
             `CI upper` = ci_ub,	
             `Tau^2` = tau2, 
             `I^2` = I2, 
             `H^2` = H2)
  })
  
  # Render the table for all studies with original and recalculated effect sizes
  output$correctedStudies <- renderTable({
    req(data_available())  # Ensure data is available
    
    results <- smd_corrected_for_extreme_errors()
    
    df <- reactive_data()
    corrected_smds <- results$es_corrected
    
    # Combine original and recalculated effect sizes, with "-" for non-recalculated studies
    df %>%
      mutate(
        `Original SMD effect size` = escalc(measure = "SMD", 
                                            m1i = m1, 
                                            sd1i = sd1, 
                                            n1i = n1,
                                            m2i = m2, 
                                            sd2i = sd2, 
                                            n2i = n2, 
                                            data = df)$yi,
        `Recalculated SMD effect size` = if_else(row_number() %in% reactive_extreme_data()$index,
                                                 corrected_smds$yi[row_number()], 
                                                 NA_real_)
      ) %>%
      #replace_na(list(`Recalculated SMD effect size` = "-")) %>%
      select(study, 
             `Original SMD effect size`, 
             `Recalculated SMD effect size`) 
      #arrange(study) %>%
      #mutate(across(c(`Original SMD effect size`, `Recalculated SMD effect size`), ~ ifelse(. == "-", "-", round(. , 2))))  # Round the SMD effect sizes for display, "-" for non-recalculated ones
  })
  
  ## Download handler for the preformatted CSV file
  output$downloadExample <- downloadHandler(
    filename = function() {
      "example_summary_statistics.csv"
    },
    content = function(file) {
      write.csv(example_summary_statistics, file, row.names = FALSE)
    })
}

# Run the app
shinyApp(ui = ui, server = server)
