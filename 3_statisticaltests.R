### Main script for statistical tests ###

# This script contains references to and results of statistical tests                   #
# A linear regression on roadtype, a linear regression on distance to nearest trashcan, #
# the inter operator variability, and validation through means of a confusion matrix    #

# date: june 2019


# load libraries ----
library(gsheet)
library(sp)
library(raster)
library(optpart)

# load scripts ----
source('scripts/regression_trashcansdistance.R')
source('scripts/regression_roadtype.R')
source('scripts/interoperatorvariability.R')
source('scripts/validation.R')
source('scripts/getdataframe.R')

# linear regression (x = distance to nearest trashcan, y = total amount of litter) ----
trashCanRegressionResults <- trashcanRegression()
print(trashCanRegressionResults)

# linear regression (x = road type, y = total amount of litter) ----
roadTypeRegressionResults <- roadTypeRegression()
print(roadTypeRegressionResults)

# interoperator variablity ----
IOPBoxplot <- interOperatorVariability()
IOPBoxplot

# Validation of indicator kriging using independent validation points
confusionMatrix <- doValidation()
confusionMatrix


