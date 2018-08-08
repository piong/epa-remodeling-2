#######################
### RUN THIS SCRIPT ###
#######################
###      FIRST      ###
#######################

### This R Script serves the function of cleaning and combining datasets from the US EPA's National Rivers and Streams Assessment Survey, for the
### purposes of eventual data analysis in a D3 dashboard.

### Updated 02/28/2018

# This formats and stores the current date as a variable, and uses this in the output function writecsv.
date <- format(Sys.Date(), format="%Y_%m_%d")

###########################
#   LOADING THE PACKAGES  #
###########################

library(tidyverse)
library(plyr)
library(stringr)
library(data.table)
library(readxl)

# install.packages("tidyverse")
# install.packages("stringr")
# install.packages("data.table")
# install.packages("readxl")

######################################
#   SETTING DIR, GETTING FILE NAMES  #
######################################

# This establishes the root working directory where this R Project sits. Storing the directory as a variable aids in writing the output to folders relative to the working directory (see writecsv function)
root <- getwd()

# Changing the directory to the Data folder
setwd(paste(root, "/Data", sep=''))

# Looking for data files within the directory that match the name "NRSA......csv". So ensure that all data files that should be read in have this naming pattern.
files <- list.files(pattern=c("NRSA","*.csv"))


#######################
#   READING IN DATA   #
#######################

# This loop reads in the data by its file name, which should comprise the files list from the previous line of code. It also names the data exactly as its raw filename.
for (i in 1:length(files)) {
  assign(paste(files[i]), read.csv(files[i], check.names=FALSE))
  print(paste(files[i]))
  print(names(get(files[i])))
}

setwd(root)



