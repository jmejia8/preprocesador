library(e1071)

source("core.r")

# Data frame
myData <- openData("Theoph.arff")

val <- localSearch(c(1,1,1,5), myData)

print(val)