#install.packages("C50")
#install.packages("gmodels")
library(C50)
library(gmodels)

occ_detect <- read.csv("data/classification/OccupancyDetection.csv", stringsAsFactors = FALSE)
#add data to a dataframe

set.seed(2)
occ_detectRand <- occ_detect[order(runif(150)), ]

occ_detectTrain <- occ_detectRand[1:100, ]
occ_detectTest <- occ_detectRand[101:150, ]

prop.table(table(occ_detectTrain$Occupancy))
prop.table(table(occ_detectTest$Occupancy))

#occ_detectTreeModel <- C5.0(occ_detectTrain[-5], occ_detectTrain$Occupancy)
occ_detectTreeModel <- C5.0(as.factor(Occupancy) ~ ., data= occ_detectTrain)

summary(occ_detectTreeModel)
plot(occ_detectTreeModel)

occ_detectPredict <- predict(occ_detectTreeModel, occ_detectTest)

CrossTable(occ_detectPredict, occ_detectTest$Occupancy, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

