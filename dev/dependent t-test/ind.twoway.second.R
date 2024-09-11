ind.twoway.second <- function (m, sd, n, unbiased = TRUE, sig.level = 0.05, digits = 3) 
{
  if (is.vector(n)) {
    n <- matrix(n, ncol = ncol(m), nrow = nrow(m))
  }
  if (nlevels(as.factor(n)) == 1) {
    Nh <- sum(n)
  }
  else {
    Nh <- (prod(dim(m))^2)/sum(1/n)
  }
  if (unbiased == FALSE) {
    sd <- ssd2sd(n, sd)
  }
  dfw <- (sum(n) - prod(dim(m)))
  MSw <- sum((n - 1) * sd^2)/dfw
  SSb <- Nh * svar(as.vector(m))
  SSrow <- Nh * svar(rowSums(m)/ncol(m))
  SScol <- Nh * svar(colSums(m)/nrow(m))
  SSint <- SSb - SSrow - SScol
  SSw <- MSw * dfw
  SSt <- SSb + SSw
  dfrow <- nrow(m) - 1
  dfcol <- ncol(m) - 1
  dfint <- dfrow * dfcol
  dfb <- dfrow + dfcol + dfint
  MSrow <- SSrow/dfrow
  MScol <- SScol/dfcol
  MSint <- SSint/dfint
  frow <- MSrow/MSw
  fcol <- MScol/MSw
  fint <- MSint/MSw
  MSb <- SSb/dfb
  fb <- MSb/MSw
  p.row <- pf(q = frow, df1 = dfrow, df2 = dfw, lower.tail = FALSE)
  p.col <- pf(q = fcol, df1 = dfcol, df2 = dfw, lower.tail = FALSE)
  p.int <- pf(q = fint, df1 = dfint, df2 = dfw, lower.tail = FALSE)
  p.b <- pf(q = fb, df1 = dfb, df2 = dfw, lower.tail = FALSE)
  anova.table <- data.frame(matrix(NA, ncol = 4, nrow = 6))
  rownames(anova.table) <- c("Between", "Between (row)", "Between (col)", 
                             "Between (row * col)", "Within", "Total")
  colnames(anova.table) <- c("SS", "df", "MS", "F")
  anova.table$SS <- c(SSb, SSrow, SScol, SSint, SSw, SSt)
  anova.table$df <- c(dfb, dfrow, dfcol, dfint, dfw, dfb + 
                        dfw)
  anova.table$MS <- c(MSb, MSrow, MScol, MSint, MSw, NA)
  anova.table$F <- c(fb, frow, fcol, fint, NA, NA)
  class(anova.table) <- c("anova", "data.frame")
  anova.table <- round(anova.table, digits)
  tmp.function <- function(etasq, df1, df2, sig.level = sig.level) {
    f2 <- etasq/(1 - etasq)
    f.value <- f2 * (df2/df1)
    iter <- length(etasq)
    delta.lower <- delta.upper <- numeric(iter)
    for (i in 1:iter) {
      delta.lower[i] <- try(FNONCT(f.value[i], df1[i], 
                                   df2, prob = 1 - sig.level/2), silent = TRUE)
      delta.upper[i] <- try(FNONCT(f.value[i], df1[i], 
                                   df2, prob = sig.level/2), silent = TRUE)
    }
    cond1 <- is.character(delta.lower)
    cond2 <- is.character(delta.upper)
    if (cond1) {
      delta.lower[grep("Error", delta.lower)] <- 0
      delta.lower <- as.numeric(delta.lower)
    }
    if (cond2) {
      delta.upper[grep("Error", delta.upper)] <- 0
      delta.upper <- as.numeric(delta.upper)
    }
    lower.etasq <- delta.lower/(delta.lower + df1 + df2 + 
                                  1)
    upper.etasq <- delta.upper/(delta.upper + df1 + df2 + 
                                  1)
    output <- data.frame(partial.etasq = etasq, partial.etasq.lower = lower.etasq, 
                         partial.etasq.upper = upper.etasq)
    return(output)
  }
  etasq.row <- SSrow/(SSrow + SSw)
  etasq.col <- SScol/(SScol + SSw)
  etasq.int <- SSint/(SSint + SSw)
  omnibus.es <- tmp.function(c(etasq.row, etasq.col, etasq.int), 
                             df1 = c(dfrow, dfcol, dfint), df2 = dfw, sig.level = sig.level)
  rownames(omnibus.es) <- c("Between (row)", "Between (col)", 
                            "Between (row * col)")
  omnibus.es <- round(omnibus.es, digits)
  c.delta <- c(0.1, 0.25, 0.4)
  criterion.power <- rbind(power.f2(sig.level = sig.level, 
                                    df1 = dfrow, df2 = dfw, delta = c.delta), power.f2(sig.level = sig.level, 
                                                                                       df1 = dfcol, df2 = dfw, delta = c.delta), power.f2(sig.level = sig.level, 
                                                                                                                                          df1 = dfint, df2 = dfw, delta = c.delta))
  colnames(criterion.power) <- c("small", "medium", "large")
  rownames(criterion.power) <- rownames(omnibus.es)
  criterion.power <- round(criterion.power, digits)
  output <- list(anova.table = anova.table, omnibus.es = omnibus.es, 
                 power = criterion.power)
  return(output)
}