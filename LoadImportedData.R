## Load the data

## Set the working directory and load packages
setwd("/Users/pbyrd/Git/casestudy2")

## Need to install the following packages: fpp
# install.packages("fpp")
library(fpp)


## Read CSV input file
ImportedAsIsData <- read.csv("Data/ImportedAsIsDataChulwalar.csv", header=FALSE, sep=";", fill=T)
ImportedPlanData <- read.csv("Data/ImportedPlanDataChulwalar.csv", header=FALSE, sep=";", fill=T)
ImportedIndicators <- read.csv("Data/ImportedIndicatorsChulwalar.csv", header=FALSE, sep=";", fill=T)
