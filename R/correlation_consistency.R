#' correlation_consistency
#'
#' Examine the inequality of correlation coefficients. That is, given the correlations between X and Y (r_XY) and X and Z (r_XZ) in the same sample, determine (a) the mathematical bounds of the correlation between Y and Z (r_YX), and (b) whether the reported empirical correlation falls within these bounds. 
#'
#' @param r_XY The reported Pearson's r correlation variables X and Y. 
#' @param r_XZ The reported Pearson's r correlation variables X and Z. 
#' @param r_YZ The reported Pearson's r correlation variables Y and Z. This argument is optional and defaults to NA.
#'
#' @return a data frame with the variables the inputs (r_XY, r_XZ, and r_YZ), r_YZ_lower_bound and r_YZ_upper_bound (calculated from r_XY and r_XZ), and consistent (whether r_YZ is within the lower and upper bounds).
#' 
#' @examples
#' correlation_consistency(r_XY = .70, r_XZ = .80, r_YZ = .10)
#' 
#' correlation_consistency(r_XY = .90, r_XZ = .90, r_YZ = .10)
#' 
#' correlation_consistency(r_XY = .50, r_XZ = .40, r_YZ = .20)
#' 
#' @export
correlation_consistency <- function(r_XY, r_XZ, r_YZ = NA){
  require(janitor)
  require(dplyr)
  
  # Since X, Y, and Z are z-scored vectors, the correlation coefficient between any pair is the cosine similarity.
  lower_bound <- cos(acos(r_XY) + acos(r_XZ))
  upper_bound <- cos(abs(acos(r_XY) - acos(r_XZ)))
  
  # correlations are bounded [-1, +1], so correct impossible results
  lower_bound <- ifelse(lower_bound < -1, -1, 
                        ifelse(lower_bound > +1, +1, lower_bound))
  
  upper_bound <- ifelse(upper_bound < -1, -1, 
                        ifelse(upper_bound > +1, +1, upper_bound))
  
  if(!is.na(r_YZ)){
    res <- data.frame(r_XY = r_XY,
                      r_XZ = r_XZ,
                      r_YZ = r_YZ,
                      r_YZ_lower_bound = round_half_up(lower_bound, digits = 3),
                      r_YZ_upper_bound = round_half_up(upper_bound, digits = 3)) |>
      mutate(consistent = ifelse(r_YZ > r_YZ_lower_bound & r_YZ < r_YZ_upper_bound, TRUE, FALSE))
  } else {
    res <- data.frame(r_XY = r_XY,
                      r_XZ = r_XZ,
                      r_YZ = r_YZ,
                      r_YZ_lower_bound = round_half_up(lower_bound, digits = 3),
                      r_YZ_upper_bound = round_half_up(upper_bound, digits = 3)) |>
      mutate(consistent = NA)
  }
  
  return(res)
}




