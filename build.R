
library(roxygen2)
# setwd("~/git")
# devtools::create("ERROR")

setwd("~/git/ERROR")
devtools::document()

devtools::build_vignettes()

devtools::check()

#devtools::install()

# or from github, after push
library(devtools)
install_github("ianhussey/ERROR")

library(ERROR)

?ERROR

detach("package:ERROR", unload=TRUE)
