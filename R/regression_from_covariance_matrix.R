#' regression_from_covariance_matrix
#'
#' Fit a univeriable or multivariable regression using a covariance matrix as an input rather than the data itself. Note that only continuous variables can be used.  
#'
#' @param model lavaan/Wilkinson notation for the regression model, e.g., Y ~ X1 + X2
#' @param cov A covariance matrix. See examples for how to convert a correlation matrix and standard deviations into a covariance matrix.
#' @param n An integer specifying the sample size.
#'
#' @return a data frame containing unstandardized and standardized regression coefficients and their confidence intervals, and p values.
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
#' mean <- c(               22.80,        48.34,             9.90,              17.10,                       21.80,             28.07)
#' sd   <- c(                4.18,        11.38,             2.10,               7.50,                        4.10,              4.60)
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
#' # convert correlations and SDs to covariances
#' variance_covariance <- 
#'   lavaan::cor2cov(R = correlation, 
#'                   sds = sd)
#' 
#' extracted_results <-
#'   list(
#'     n = n,
#'     name = name,
#'     mean = mean,
#'     sd = sd,
#'     correlation = correlation,
#'     variance_covariance = variance_covariance
#'   )
#' 
#' # fit regression using covariance matrix
#' fit <- regression_from_covariance_matrix(
#'   model = 'prosocial_behavior ~ perceived_stress + positive_affect + psychological_flexibility + social_support',
#'   cov = extracted_results$variance_covariance,
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
regression_from_covariance_matrix <- function(model, cov, n){
  # dependencies
  require(lavaan)
  require(dplyr)
  require(janitor)

  # fit model
  fit <- 
    sem(model       = model, 
        sample.cov  = cov, 
        sample.nobs = n)
  
  # extract results
  res_unstandardized <- 
    parameterEstimates(fit, 
                       remove.nonfree = TRUE) |>
    # drop irrelevant rows
    filter(op != "~~") |>
    # wrangle variable names and round values
    select(predictor = rhs,
           B = est,
           B_se = se,
           B_ci_lower = ci.lower,
           B_ci_upper = ci.upper,
           p = pvalue) |>
    mutate(p = ifelse(p < .0001, "< .0001", janitor::round_half_up(p, 4)))
  
  res_standardized <- 
    standardizedSolution(fit) |>
    # drop irrelevant rows
    filter(op != "~~") |>
    # wrangle variable names and round values
    select(predictor = rhs,
           Beta = est.std,
           Beta_se = se,
           Beta_ci_lower = ci.lower,
           Beta_ci_upper = ci.upper)
  
  res <- full_join(res_unstandardized, res_standardized, by = "predictor")

  return(res)
}




