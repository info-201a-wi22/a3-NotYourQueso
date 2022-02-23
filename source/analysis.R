#Installing necessary packages
  library("dplyr")
  library("stringr")
  library("tidyverse")
  library("ggplot2")  

# Loading the dataset
incarceration <- read.csv("../source/incarceration_trends.csv",
                          stringsAsFactors = FALSE)


