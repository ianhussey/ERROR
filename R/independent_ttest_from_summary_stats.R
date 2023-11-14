
# Independent t test from summary stats (M, SD, N)
# TODO: like anova_from_summary_stats, this function should accomodate the potential rounding of the reported summary stats
# and return reported, min and max values for t, p, cohen's d, and its CIs.
# TODO: integrate effect_sizes_from_summary_statistics and this function, ie one function that produces t test output (df, p values) 
# and also multiple effect sizes (d, g), and also min max values due to rounding 

independent_ttest_from_summary_stats <- function(m1, sd1, n1, m2, sd2, n2, sig_level = 0.05){
  
  # Calculate the t-test statistic
  t_value <- (m1 - m2) / sqrt((sd1^2/n1) + (sd2^2/n2))
  
  # Calculate the degrees of freedom using the approximation
  df <- ((sd1^2/n1 + sd2^2/n2)^2) / ((sd1^2/n1)^2/(n1-1) + (sd2^2/n2)^2/(n2-1))
  
  # Obtain p-value for two-tailed test
  p_value <- 2 * (1 - pt(abs(t_value), df))
  
  # Calculate Cohen's d
  pooled_sd <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1 + n2 - 2))
  d <- (m1 - m2) / pooled_sd
  
  # Calculate the standard error of d
  se_d <- sqrt(((n1 + n2) / (n1 * n2)) + (d^2 / (2 * (n1 + n2))))
  
  # Calculate the t critical value for 95% CI
  t_critical <- qt(1 - (sig_level/2), df = n1 + n2 - 2)
  
  # Calculate the 95% CI for Cohen's d
  d_ci_lower <- d - t_critical * se_d
  d_ci_upper <- d + t_critical * se_d
  
  res <- 
    data.frame(t = t_value,
               df = df,
               p = p_value,
               cohens_d = d,
               cohens_d_ci_lower =  d_ci_lower,
               cohens_d_ci_upper =  d_ci_upper)
  
  return(res)
}

#' # Example
#' independent_ttest_from_summary_stats(m1 = 10, 
#'                                      sd1 = 2,
#'                                      n1 = 30,
#'                                      m2 = 12,
#'                                      sd2 = 3,
#'                                      n2 = 30)




