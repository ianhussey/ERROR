# ERROR

This R package contains a collection of tools for scrutinising the results of articles and preprints to detect errors. References and web apps are provided for each below. Working examples are provided for each in the vignettes.

[TOC]



## Usage

Install the package directly from GitHub, then view the vignettes in RStudio.

1. Run the following code:

```R
devtools::install_github("ianhussey/ERROR", build_vignettes = TRUE)
library(ERROR)
?ERROR
```

2. Click "Index" and then "User guides, package vignettes and other documentation." to view a list of all vignettes.

## Included tools

### Statcheck

For assessing whether reported test statistics, degrees of freedom, and *p* values are mathematically congruent with one another, and therefore useful for detecting reporting errors.  

#### Webapp

[statcheck.io](https://statcheck.io)

#### R package

[statcheck](https://cran.r-project.org/web/packages/statcheck/)

#### Documentation/Tutorial

Difficulty: easy
Details: comprehensive

[statcheck](https://rpubs.com/michelenuijten/statcheckmanual)

#### References

Nuijten, M. B., & Polanin, J. R. (2020) “statcheck”: Automatically detect statistical reporting inconsistencies to increase reproducibility of meta-analyses. *Research Synthesis Methods.* https://doi.org/10.1002/jrsm.1408 [pdf on ilias]

Nuijten, M. B., Hartgerink, C. H., Van Assen, M. A., Epskamp, S., & Wicherts, J. M. (2016). The prevalence of statistical reporting errors in psychology (1985–2013). *Behavior Research Methods, 48,* 1205-1226. https://doi.org/10.3758/s13428-015-0664-2 



### GRIM, GRIMMER & DEBIT

For assessing whether reported means and standard deviations (and indeed standard errors and variances) are mathematically possible for granular/discrete or binary data, and therefore useful for detecting reporting errors.  

#### Webapp

GRIM (means) http://www.prepubmed.org/general_grim/

GRIMMER (SDs, SEs, or variance) http://www.prepubmed.org/grimmer/ 

DEBIT (binary data) ?

#### R package

[scrutiny](https://cran.r-project.org/web/packages/scrutiny)

#### Documentation/Tutorial

Difficulty: easy to medium
Details: comprehensive

[GRIM](https://lhdjung.github.io/scrutiny/articles/grim.html)

[GRIMMER](https://lhdjung.github.io/scrutiny/articles/grimmer.html)

[DEBIT](https://lhdjung.github.io/scrutiny/articles/debit.html)

#### References

Brown, Nicholas J. L., Heathers, James A. J. (2016). The GRIM Test: A Simple Technique Detects Numerous Anomalies in the Reporting of Results in Psychology. *Social Psychological and Personality Science. 8*(4), pp 363–369. https://doi.org/10.1177/1948550616673876 

Heathers, J. (2016) The GRIM test — a method for evaluating published research. https://jamesheathers.medium.com/the-grim-test-a-method-for-evaluating-published-research-9a4e5f05e870 

Anaya J. (2016) The GRIMMER test: A method for testing the validity of reported measures of variability. https://doi.org/10.7287/peerj.preprints.2400v1 



### SPRITE

For reconstructing datasets that have the same summary statistics (M, SD, N) and constraints (e.g., range constraints) as those reported in articles, in order to assess the plausibility of those original datasets and summary statistics.

#### **Webapp** 

https://steamtraen.shinyapps.io/rsprite/ 

#### R package

[rsprite2](https://cran.r-project.org/web/packages/rsprite2)

possibly also [scrutiny](https://cran.r-project.org/web/packages/scrutiny)?

#### Documentation/Tutorial

Difficulty: easy
Details: scarce

[rsprite2](https://lukaswallrich.github.io/rsprite2/)

#### References

Heathers, J. A., Anaya, J., van der Zee, T., Brown, N. J. L. (2018). Recovering data from summary statistics: Sample Parameter Reconstruction via Iterative TEchniques (SPRITE). PeerJ. https://doi.org/10.7287/peerj.preprints.26968v1

Heathers, J. A. (2018). Introducing SPRITE (and the Case of the Carthorse Child). https://medium.com/hackernoon/introducing-sprite-and-the-case-of-the-carthorse-child-58683c2bfeb 



### Recalculate effect sizes and p values for between subjects designs from summary stats

Recalculate Cohen's d or Hedges' g effect sizes, independent t-tests, 1-way or 2-way between subjects ANOVAs from summary statistics (M, SD, and N per cell). 

#### **Webapp** 

NA

#### R package

[under construction, see vignettes for working examples]

#### References

[under construction]

#### Documentation/Tutorial

[under construction]



## Tools to be added



### *p*-curve

For assessing the credibility of p values within a given literature.

#### Webapp

https://www.p-curve.com/app4/ 

#### R package

[dmetar](https://dmetar.protectlab.org/) (see Harrer chapter below)

#### Documentation/Tutorial

Difficulty: medium
Details: comprehensive
[dmetar](https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/pub-bias.html#addressing-pubbias)

#### References

Simonsohn, Nelson, & Simmons (2014) p-Curve: A Key to the File-Drawer. *Journal of Experimental Psychology: General.* https://doi.org/10.1037/a0033242 

Simonsohn, Nelson, & Simmons (2015) Better p-curves: Making p-curve analysis more robust to errors, fraud, and ambitious p-hacking, a Reply to Ulrich and Miller (2015). *Journal of Experimental Psychology: General.* https://doi.org/10.1037/xge0000104 

Harrer, M., Cuijpers, P., Furukawa, T.A., & Ebert, D.D. (2021). Doing Meta-Analysis with R: A Hands-On Guide. Boca Raton, FL and London: Chapman & Hall/CRC Press. ISBN 978-0-367-61007-4. https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/pub-bias.html#p-curve 

Erdfelder & Heck (2019) Detecting Evidential Value and p-Hacking With the p-Curve Tool: A Word of Caution. https://doi.org/10.1027/2151-2604/a000383 



### *Z*-curve

For assessing the credibility of p values within a given literature. Argued to have advantages over *p*-curve, but is also more complex (both in its inner workings and to interpret the output of). 

#### Webapp

https://shinyapps.org/apps/p-checker/ (see the Excess Significance tab, although note this only produces some of Z-curve's metrics and not its plot)

#### R package

[zcurve](https://cran.r-project.org/web/packages/zcurve/index.html)

#### Documentation/Tutorial

Difficulty: medium
Details: average (explain how to use the package, not how to interpret results)

[zcurve](https://fbartos.github.io/zcurve/)

#### References

Bartoš, F., & Schimmack, U. (2022) Z-curve 2.0: Estimating Replication Rates and Discovery Rates. *Meta-psychology*, 6. https://doi.org/10.15626/MP.2021.2720 



### WebPlotDigitiser

For extracting data from plots.

#### Webapp

https://automeris.io/WebPlotDigitizer/ (also has local apps for Windows, Mac OS and Linux)

#### R package

[digitizeR](https://github.com/ankitrohatgi/digitizeR) (note that this package is only available on GitHub, not CRAN. It provides an R interface for the underlying original WebPlotDigitiser, which is written in JavaScript I think)

#### References

#### Documentation/Tutorials

Difficulty: ?
Details: comprehensive

[manual](https://automeris.io/WebPlotDigitizer/userManual.pdf)
[videos](https://automeris.io/WebPlotDigitizer/tutorial.html)



### RIVETS

Rounded Input Variables, Exact Test Statistics: A Technique For Detecting Hand-Calculated Results in Published Research.

#### Webapp

[to be added]

#### R package

[to be added]

#### Documentation/Tutorials

[to be added]

#### References

Brown & Heathers (2019) Rounded Input Variables, Exact Test Statistics (RIVETS): A Technique For Detecting Hand-Calculated Results in Published Research. https://doi.org/10.31234/osf.io/ctu9z 



### RECOVAR (REuse COrrelations for Validation And Regression)

Check correlation tables for consistency and (re)create regression analyses using correlation tables as input rather than the participant level data 

#### Webapp

[under construction]

#### R package

[under construction]

#### References

[under construction]

#### Documentation/Tutorials

[under construction]
