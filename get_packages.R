######################################
# Function to install basic packages #
# Daniel Grimaldi ####################
######################################

get_packages <- function(install=TRUE){
  package.list <- c("devtools",
                    "base64enc",
                    "bit64",
                    "car",
                    "clubSandwich",
                    "data.table",
                    "DBI",
                    "dplyr",
                    "describedata",
                    "doParallel",
                    "dbplyr",
                    "evd",
                    "foreign",
                    "Formula",
                    "fuzzyjoin",
                    "ggcorrplot",
                    "ggplot2",
                    "ggpubr",
                    "Hmisc",
                    "htmltools",
                    "kableExtra",
                    "knitr",
                    "lmtest",
                    "lubridate",
                    "MASS",
                    "MatchIt",
                    "openxlsx",
                    "odbc",
                    "patchwork",
                    "pdftools",
                    "plm",
                    "rdd",
                    "rdrobust",
                    "rddtools",
                    "readstata13",
                    "rgenoud",
                    "rjson",
                    "rmarkdown",
                    "RODBC",
                    "RSQLite",
                    "stargazer",
                    "summarytools",
                    "tidyr",
                    "tinytex",
                    "tidyverse")
  
  status.vector <- sapply(package.list, require, character.only=T, quietly=TRUE)
  print(paste("The following packages need to be installed:", paste(names(status.vector[status.vector==FALSE]), collapse = ", ")))
  
  if(install){
    for (i in names(status.vector[status.vector==FALSE])){
      print(paste("Package ", i, "will be installed."))
      if (i=="clubSandwich"){
        devtools::install_url("https://cran.r-project.org/src/contrib/Archive/clubSandwich/clubSandwich_0.4.2.tar.gz", dependencies=TRUE)}else{
      install.packages(i)}}
  }
}



