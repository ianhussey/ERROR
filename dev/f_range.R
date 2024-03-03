# nick brown's function to display the possible ranges of the F or t statistic from a one- or two-way ANOVA, 
# while correcting for rounding. Taken from his website; code as taken didn't work. 

library(rpsychi)

# Function to display the possible ranges of the F or t statistic from a one- or two-way ANOVA.
f_range <- function (m, s, n, title=FALSE, show.t=FALSE, dp.p=-1, labels=c()) {
  m.ok <- m
  if (inherits(m.ok, "matrix")) {  # <-- changed this line
    func <- ind.twoway.second
    useF <- c(3, 2, 4)
    default_labels <- c("col F", "row F", "inter F")
  }
  else {
    m.ok <- matrix(m)
    func <- ind.oneway.second
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
    numbers <- c(m, s)
    for (i in numbers) {
      if (i != round(i, 0)) {
        dp <- max(dp, 1)
        j <- i * 10
        if (j != round(j, 0)) {
          dp <- max(dp, 2)
        }
      }
    }
  }
  
  if (length(labels) == 0) {
    labels <- default_labels
  }
  
  # Calculate the nominal test statistic(s) (i.e., assuming no rounding error)
  f.nom <- func(m=m.ok, sd=s, n=n)$anova.table$F
  
  # We correct for rounding in reported numbers by allowing for the maximum possible rounding error.
  # For the maximum F estimate, we subtract .005 from all SDs; for minimum F estimate, we add .005.
  # We then add or subtract .005 to every mean, in all possible permutations.
  # (".005" is an example, based on 2 decimal places of precision.)
  delta <- (0.1 ^ dp) / 2    #typically 0.005
  tiny.s <- delta / 100      #minimum SD, to prevent divide-by-zero errors
  
  s.hi <- pmax(s - delta, tiny.s)
  s.lo <- pmax(s + delta, tiny.s)
  
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
    }
    else {
      adj.f.hi <- func(m=m.adj, sd=s.hi, n=n)$anova.table$F
      adj.f.lo <- func(m=m.adj, sd=s.lo, n=n)$anova.table$F
    }
    
    f.hi <- pmax(f.hi, adj.f.hi)
    f.lo <- pmin(f.lo, adj.f.lo)
  }
  
  if (show.t) {
    f.nom <- sqrt(f.nom)
    f.hi <-  sqrt(f.hi)
    f.lo <-  sqrt(f.lo)
  }
  
  if (title != FALSE) {
    cat(title)
  }
  
  sp <- " "
  fdp <- 3     # number of DPs for F or t statistic
  dpf <- paste("%.", fdp, "f", sep="")
  for (i in 1:length(useF)) {
    j <- useF[i]
    cat(sp, labels[i], ": ", sprintf(dpf, f.nom[j]),
        " (min=", sprintf(dpf, f.lo[j]),
        ", max=", sprintf(dpf, f.hi[j]), ")",
        sep="")
    sp <- "  "
  }
  
  if ((dp.p == -1)  && (dp < 2)) {
    cat(" <<< dp set to", dp, "automatically")
  }
  
  cat("\n", sep="")
}

lab <- c("gender", "group", "gender x group")
n <- matrix(c(40, 20, 35, 10), ncol=2)
m <- matrix(c(5.00, 2.69, 4.83, 5.54), ncol=2)
sd <- matrix(c(2.99, 2.57, 2.71, 1.84), ncol=2)
f_range(m = m, s = sd, n = n, title = "Line 1", labels = lab)

# error message
# > Error in if (class(m.ok) == "matrix") { : the condition has length > 1
