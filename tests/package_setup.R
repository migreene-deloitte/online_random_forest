## package test

#install.packages("roxygen2")
#install.packages("devtools")
library(devtools)
library(roxygen2)
library(dplyr)

#create package directory
#create("corf") # uses devtools - trying instead with RcppEigen template
# library(RcppEigen)
# RcppEigen.package.skeleton("corf")
# 
### set up the RcppExports code
#setwd("..")
#Rcpp::compileAttributes("corf")

### documentation creation
setwd("./corf")
document()

#not working with Rcpp - using R CMD check and R CMD INSTALL instead
#setwd("..")
#install("corf")

#setwd("corf_package/corf")
setwd("./corf")
devtools::check()
  
