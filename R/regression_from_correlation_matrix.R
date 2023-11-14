#' regression_from_correlation_matrix
#'
#' Fit a univeriable or multivariable regression using a covariance matrix as an input rather than the data itself. Note that only continuous variables can be used.  
#'
#' @param model lavaan/Wilkinson notation for the regression model, e.g., Y ~ X1 + X2
#' @param cor A correlation matrix. See examples for how to convert the upper or lower triangle of a correlation matrix reported in an article into the full matrix. 
#' @param n An integer specifying the sample size.
#'
#' @return a data frame containing standardized regression coefficients and their confidence intervals, and p values.
#' 
#' @examples
#' # dependencies 
#' library(dplyr)
#' library(janitor)
#' library(lavaan)
#' library(knitr)
#' library(kableExtra)
#' 
#' ## results extracted from Haller et al. (2022) 'To Help or Not to Help? Prosocial Behavior, Its Association With Well-Being, and Predictors of Prosocial Behavior During the Coronavirus Disease Pandemic'
#' n <- 9496
#' 
#' name <- c("Prosocial behavior", "Well-being", "Social Support", "Perceived Stress", "Psychological Flexibility", "Positive Affect")
#' 
#' name <- janitor::make_clean_names(name)
#' 
#' correlation <- 
#'   lavaan::getCov(x = c(
#'     1.00,  0.32,  0.34, -0.09,  0.21,  0.30, # Prosocial behavior
#'            1.00,  0.45, -0.54,  0.54,  0.64, # Well-being
#'                   1.00, -0.26,  0.32,  0.34, # Social Support
#'                          1.00, -0.52, -0.49, # Perceived Stress
#'                                 1.00,  0.50, # Psychological Flexibility
#'                                        1.00  # Positive Affect
#'   ),
#'   lower = FALSE,
#'   diagonal = TRUE, 
#'   names = name
#'   )
#' 
#' 
#' extracted_results <-
#'   list(
#'     n = n,
#'     name = name,
#'     correlation = correlation
#'   )
#' 
#' # fit regression using correlation matrix
#' fit <- regression_from_correlation_matrix(
#'   model = 'prosocial_behavior ~ perceived_stress + positive_affect + psychological_flexibility + social_support',
#'   cor = extracted_results$correlation,
#'   n = extracted_results$n
#' )
#' 
#' # print results
#' fit |>
#'   # round variables for printing
#'   mutate(across(c(where(is.numeric), -p), janitor::round_half_up, digits = 2)) |>
#'   # print html table
#'   kable() |>
#'   kable_classic(full_width = FALSE)
#' 
#' 
#' @export
regression_from_correlation_matrix <- function(model, cor, n){
  # dependencies
  require(lavaan)
  require(dplyr)
  require(janitor)

  # fit model
  fit <- 
    sem(model       = model, 
        sample.cov  = cor, 
        sample.nobs = n)
  
  # extract results
  res <- 
    parameterEstimates(fit, 
                       remove.nonfree = TRUE) |>
    # drop irrelevant rows
    filter(op != "~~") |>
    # wrangle variable names and round values
    select(predictor = rhs,
           Beta = est,
           Beta_se = se,
           Beta_ci_lower = ci.lower,
           Beta_ci_upper = ci.upper,
           p = pvalue) |>
    mutate(p = ifelse(p < .0001, "< .0001", janitor::round_half_up(p, 4)))
  
  return(res)
}




