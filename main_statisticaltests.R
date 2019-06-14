# load scripts ----
source('scripts/regression_trashcansdistance.R')
source('scripts/regression_roadtype.R')
source('scripts/interoperatorvariability.R')

# linear regression (x = distance to nearest trashcan, y = total amount of litter) ----
trashCanRegressionResults <- trashcanRegression()
print(trashCanRegressionResults)

# linear regression (x = road type, y = total amount of litter) ----
roadTypeRegressionResults <- roadTypeRegression()
print(roadTypeRegressionResults)

# interoperator variablity ----
IOPBoxplot <- interOperatorVariability()
IOPBoxplot
