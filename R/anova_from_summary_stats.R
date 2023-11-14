
#################
# Compute 1-way or 2-way ANOVA from summary statistics (M, SD, N)
# code adapted from rpsychi R package (v0.8, no longer on CRAN due to lack of maintainance)
# and Nick Brown's f_range() function (https://steamtraen.blogspot.com/2018/01/) which adapts functions from the rpsychi to deal with rounding issues. 
# Nick's code relies on rpsychi, and no longer runs due to changes to R over time.
# This code gathers the necessary functions in one place, fixes issues that arose due to R updates, 
# has the anova_from_summary_stats() function (which was based on f_range) return a data frame instead of printing text, and
# includes more information in that data frame (df1, df2, peta2 and its CIs).
#
# Summary stats are typically rounded for reporting. Recomputed ANOVA results may therefore be inaccurate. 
# We work around this by recomputing F values for the reported summary stats, and also the min and max F values for summary stats that could be rounded 
# to the reported values. P values and partial eta squared ($\eta_p^2$) effect size and its 90% confidence intervals are then computed from those F values. 
# Note that by default 90% CIs are computed rather than 95% CIs because partial eta2 is always positive 
# (see [Laken's blog on this topic](https://daniellakens.blogspot.com/2014/06/calculating-confidence-intervals-for.html)). 


# Compute the variance of a numeric vector
svar <- function(x, na_rm = TRUE) {
  # Remove NA values if na_rm is FALSE
  if (na_rm == FALSE) {
    x <- na.omit(x)
  }
  
  # Compute variance
  variance_value <- 1 / length(x) * sum((x - mean(x))^2)
  
  return(variance_value)
}


# Convert sum of squared deviations to standard deviation
ssd2sd <- function(n, ssd) {
  return(sqrt((n / (n - 1)) * ssd^2))
}


# Find non-centrality parameter for a given F-distribution and probability
F_test_noncentrality <- function(x, df1, df2, prob, interval = c(0, 10000), my_tol = 1e-06) {
  temp <- function(ncp) pf(x, df1, df2, ncp) - prob
  return(uniroot(temp, interval, tol = my_tol)$root)
}


power_f2 <- function(df1, df2, delta, sig.level = 0.05){
  n <- df2/(df1 + 1) + 1
  fc <- qf(p = sig.level, df1 = df1, df2 = (df1 + 1) * (n - 1), lower.tail = FALSE)
  lamda <- (delta^2) * (n * (df1 + 1))
  v <- (df1 + 1) * (n - 1)
  z1b <- (sqrt(2 * (df1 + lamda) - ((df1 + 2 * lamda)/(df1 + lamda))) - sqrt((2 * v - 1) * ((df1 * fc)/v)))/sqrt(((df1 * fc)/v) + ((df1 + 2 * lamda)/(df1 + lamda)))
  return(pnorm(z1b))
}


# Compute effect sizes and their confidence intervals
petasq_confint <- function(petasq, df1, df2, sig_level = 0.10) {
  f2 <- petasq/(1 - petasq)
  f.value <- f2 * (df2/df1)
  iter <- length(petasq)
  delta_lower <- delta_upper <- numeric(iter)
  
  for (i in 1:iter) {
    delta_lower[i] <- try(F_test_noncentrality(f.value[i], df1[i], 
                                              df2, prob = 1 - sig_level/2), silent = TRUE)
    delta_upper[i] <- try(F_test_noncentrality(f.value[i], df1[i], 
                                              df2, prob = sig_level/2), silent = TRUE)
  }
  
  cond1 <- is.character(delta_lower)
  cond2 <- is.character(delta_upper)
  
  if (cond1) {
    delta_lower[grep("Error", delta_lower)] <- 0
    delta_lower <- as.numeric(delta_lower)
  }
  
  if (cond2) {
    delta_upper[grep("Error", delta_upper)] <- 0
    delta_upper <- as.numeric(delta_upper)
  }
  
  lower_petasq <- delta_lower/(delta_lower + df1 + df2 + 1)
  upper_petasq <- delta_upper/(delta_upper + df1 + df2 + 1)
  
  output <- data.frame(partial_etasq = petasq, 
                       partial_etasq_lower = lower_petasq, 
                       partial_etasq_upper = upper_petasq)
  return(output)
}


# Two-way ANOVA for independent measures
ind_twoway_second <- function(m, sd, n, unbiased = TRUE, sig_level = 0.05, digits = 3) {
  
  # Check if n is a vector and convert it to a matrix
  if (is.vector(n)) {
    n <- matrix(n, ncol = ncol(m), nrow = nrow(m))
  }
  
  # Compute Nh based on n
  if (nlevels(as.factor(n)) == 1) {
    Nh <- sum(n)
  } else {
    Nh <- (prod(dim(m))^2) / sum(1 / n)
  }
  
  # Convert SSD to SD if unbiased is FALSE
  if (unbiased == FALSE) {
    sd <- ssd2sd(n, sd)
  }
  
  # Compute various statistics for the ANOVA table
  dfw <- (sum(n) - prod(dim(m)))
  MSw <- sum((n - 1) * sd^2) / dfw
  SSb <- Nh * svar(as.vector(m))
  SSrow <- Nh * svar(rowSums(m) / ncol(m))
  SScol <- Nh * svar(colSums(m) / nrow(m))
  SSint <- SSb - SSrow - SScol
  SSw <- MSw * dfw
  SSt <- SSb + SSw
  
  dfrow <- nrow(m) - 1
  dfcol <- ncol(m) - 1
  dfint <- dfrow * dfcol
  dfb <- dfrow + dfcol + dfint
  
  MSrow <- SSrow / dfrow
  MScol <- SScol / dfcol
  MSint <- SSint / dfint
  
  frow <- MSrow / MSw
  fcol <- MScol / MSw
  fint <- MSint / MSw
  
  MSb <- SSb / dfb
  fb <- MSb / MSw
  
  p_row <- pf(q = frow, df1 = dfrow, df2 = dfw, lower.tail = FALSE)
  p_col <- pf(q = fcol, df1 = dfcol, df2 = dfw, lower.tail = FALSE)
  p_int <- pf(q = fint, df1 = dfint, df2 = dfw, lower.tail = FALSE)
  p_b <- pf(q = fb, df1 = dfb, df2 = dfw, lower.tail = FALSE)
  
  # Construct the ANOVA table
  anova.table <- data.frame(matrix(NA, ncol = 4, nrow = 6))
  rownames(anova.table) <- c("Between", "Between (row)", "Between (col)", "Between (row * col)", "Within", "Total")
  colnames(anova.table) <- c("SS", "df", "MS", "F")
  
  anova.table$SS <- c(SSb, SSrow, SScol, SSint, SSw, SSt)
  anova.table$df <- c(dfb, dfrow, dfcol, dfint, dfw, dfb + dfw)
  anova.table$MS <- c(MSb, MSrow, MScol, MSint, MSw, NA)
  anova.table$F <- c(fb, frow, fcol, fint, NA, NA)
  
  class(anova.table) <- c("anova", "data.frame")
  #anova.table <- round(anova.table, digits)
  
  petasq_row <- SSrow / (SSrow + SSw)
  petasq_col <- SScol / (SScol + SSw)
  petasq_int <- SSint / (SSint + SSw)
  
  omnibus.es <- petasq_confint(petasq = c(petasq_row, petasq_col, petasq_int), 
                               df1 = c(dfrow, dfcol, dfint), 
                               df2 = dfw, 
                               sig_level = sig_level)
  
  rownames(omnibus.es) <- c("Between (row)", "Between (col)", "Between (row * col)")
  #omnibus.es <- round(omnibus.es, digits)
  
  # Compute power criteria
  c_delta <- c(0.1, 0.25, 0.4)
  criterion_power <- rbind(
    power_f2(sig.level = sig_level, df1 = dfrow, df2 = dfw, delta = c_delta), 
    power_f2(sig.level = sig_level, df1 = dfcol, df2 = dfw, delta = c_delta), 
    power_f2(sig.level = sig_level, df1 = dfint, df2 = dfw, delta = c_delta)
  )
  
  colnames(criterion_power) <- c("small", "medium", "large")
  rownames(criterion_power) <- rownames(omnibus.es)
  #criterion_power <- round(criterion_power, digits)
  
  # Aggregate outputs
  output <- list(anova.table = anova.table, 
                 omnibus.es = omnibus.es, 
                 power = criterion_power)
  
  return(output)
}


# Function to display the possible ranges of the F or t statistic from a one- or two-way between subjects ANOVA.
anova_from_summary_stats <- function(m, sd, n, show.t = FALSE, dp.p = -1, labels = c(), peta2_sig_level = 0.10) {
  
  require(janitor)
  require(dplyr)
  
  m.ok <- m
  
  if (inherits(m.ok, "matrix")) {
    func <- ind_twoway_second
    useF <- c(3, 2, 4)
    default_labels <- c("col F", "row F", "inter F")
  } else {
    m.ok <- matrix(m)
    func <- ind_twoway_second
    useF <- 1
    default_labels <- c("F")
    if (show.t) {
      default_labels <- c("t")
    }
  }
  
  # Determine how many DPs to use from input numbers, if not specified
  dp <- dp.p
  
  if (dp.p == -1) {
    dp <- 0
    numbers <- c(m, sd)
    for (i in numbers) {
      if (i != janitor::round_half_up(i, 0)) {
        dp <- max(dp, 1)
        j <- i * 10
        if (j != janitor::round_half_up(j, 0)) {
          dp <- max(dp, 2)
        }
      }
    }
  }
  
  if (length(labels) == 0) {
    labels <- default_labels
  }
  
  # Calculate the nominal test statistic(s) (i.e., assuming no rounding error)
  f.nom <- func(m = m.ok, sd = sd, n = n)$anova.table$F

  # We correct for rounding in reported numbers by allowing for the maximum possible rounding error.
  delta <- (0.1 ^ dp) / 2    #typically 0.005
  tiny.sd <- delta / 100      #minimum sd, to prevent divide-by-zero errors
  
  sd.hi <- pmax(sd - delta, tiny.sd)
  sd.lo <- pmax(sd + delta, tiny.sd)
  
  # Initialise maximum and minimum F statistics to unlikely values.
  f.hi <- rep(-1, length(useF))
  f.lo <- rep(999999, length(useF))
  f.hi <- f.nom
  f.lo <- f.nom
  
  # Generate every possible combination of +/- maximum rounding error to add to each mean.
  l <- length(m.ok)
  rawcomb <- combn(rep(c(-delta, delta), l), l)
  comb <- rawcomb[,!duplicated(t(rawcomb))]
  
  # Generate every possible set of test statistics within the bounds of rounding error,
  #  and retain the largest and smallest.
  for (i in 1:ncol(comb)) {
    m.adj <- m.ok + comb[,i]
    
    if (all((abs(m.adj - m.adj[1])) < 1e-14)) {
      adj.f.hi <- 0
      adj.f.lo <- 0
    } else {
      adj.f.hi <- func(m = m.adj, sd = sd.hi, n = n)$anova.table$F
      adj.f.lo <- func(m = m.adj, sd = sd.lo, n = n)$anova.table$F
    }
    
    f.hi <- pmax(f.hi, adj.f.hi)
    f.lo <- pmin(f.lo, adj.f.lo)
  }
  
  if (show.t) {
    f.nom <- sqrt(f.nom)
    f.hi <-  sqrt(f.hi)
    f.lo <-  sqrt(f.lo)
  }
  
  # Create an empty data frame to store results
  result_df <- data.frame(
    label = character(),
    F_type = character(), # To store min, max, or nominal
    F = numeric(),
    peta2 = numeric(),
    p = numeric(),
    stringsAsFactors = FALSE
  )
  
  df_effect = nrow(m) - 1
  df_error = sum(n) - nrow(m)
  
  fdp <- 3     # number of DPs for F or t statistic
  
  for (i in 1:length(useF)) {
    j <- useF[i]
    
    nominal_p <- 1 - pf(f.nom[j], df_effect, df_error)
    min_p <- 1 - pf(f.lo[j], df_effect, df_error)
    max_p <- 1 - pf(f.hi[j], df_effect, df_error)
    
    # Calculate partial eta squared based on F values
    peta2_nom = (f.nom[j] * df_effect) / (f.nom[j] * df_effect + df_error)
    peta2_min = (f.lo[j] * df_effect) / (f.lo[j] * df_effect + df_error)
    peta2_max = (f.hi[j] * df_effect) / (f.hi[j] * df_effect + df_error)
    
    # Add the results in the desired format to the data frame
    result_df <- rbind(
      result_df,
      data.frame(label = labels[i],
                 F_type = "reported",
                 F = janitor::round_half_up(f.nom[j], fdp),
                 df1 = df_effect,
                 df2 = df_error,
                 p = nominal_p,
                 peta2 = peta2_nom),
      data.frame(label = labels[i],
                 F_type = "min",
                 F = janitor::round_half_up(f.lo[j], fdp),
                 df1 = df_effect,
                 df2 = df_error,
                 p = min_p,
                 peta2 = peta2_min),
      data.frame(label = labels[i],
                 F_type = "max",
                 F = janitor::round_half_up(f.hi[j], fdp),
                 df1 = df_effect,
                 df2 = df_error,
                 p = max_p,
                 peta2 = peta2_max)
    )
  }
  
  # add peta2 CIs
  result_df <- 
    bind_cols(  
      result_df,
      result_df |>
        rowwise() |>
        do(petasq_confint(petasq = .$peta2, 
                          df1 = .$df1, 
                          df2 = .$df2,
                          sig_level = peta2_sig_level)) |>
        ungroup() |>
        select(peta2_ci_lower = partial_etasq_lower,
               peta2_ci_upper = partial_etasq_upper)
    )
  
  return(result_df)
}

round_half_up_to_char <- function(x, digits = 2) {
  # Round the number
  rounded <- round(x, digits)
  # Convert to character while retaining trailing zeros
  formatC(rounded, format = "f", digits = digits)
}



#' # Examples
#' library(dplyr)
#' library(knitr)
#' library(kableExtra)
#'
#' # extracted data
#' labels <- c("groups 1 (A vs B)", "groups 2 (C vs D)", "groups 1 x groups 2 interaction")
#' 
#' m <- matrix(c(5.00, 2.69, 
#'               4.83, 5.54), 
#'             ncol = 2)
#' 
#' sd <- matrix(c(2.99, 2.57, 
#'                2.71, 1.84), 
#'              ncol = 2)
#' 
#' n <- matrix(c(40, 20, 
#'               35, 10), 
#'             ncol = 2)
#' 
#' 
#' # recalculated results
#' res <- anova_from_summary_stats(m      = m, 
#'                                 sd     = sd, 
#'                                 n      = n, 
#'                                 labels = labels) 
#' 
#' res |>
#'   mutate(p = round_half_up_to_char(p, digits = 5)) |>
#'   mutate_if(is.numeric, janitor::round_half_up, digits = 3) |>
#'   knitr::kable() |>
#'   kableExtra::kable_classic(full_width = FALSE)


