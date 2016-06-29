library(e1071)

source("core.r")

# Data frame
myData <- openData("Theoph.arff")

val <- localSearch(list(1,1,1,5, 0), myData)

print(val)