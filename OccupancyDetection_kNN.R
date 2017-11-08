#install.packages('class')
#install.packages('gmodels')

occ_detect <- read.csv("data/classification/OccupancyDetection.csv", stringsAsFactors = FALSE)
str(occ_detect)
# drop the id feature
#occ_detect <- occ_detect[-1]
# table of diagnosis
table(occ_detect$X)
# recode diagnosis as a factor
colnames(occ_detect)[colnames(occ_detect)=="X"] <- "Status"
occ_detect$Status <- factor(occ_detect$Status,
                         levels = c("1", "0"),
                         labels = c("Occupied", "Vacant"))
str(occ_detect)

occ_detect <- occ_detect[lapply(occ_detect,length)>0]

# table or proportions with more informative labels
round(prop.table(table(occ_detect$Occupancy)) * 100, digits = 1)
# summarize three numeric features
summary(occ_detect[c("CO2", "Humidity",
               "Light")])


# create normalization function
normalize <- function(x) {
 return ((x - min(x)) / (max(x) - min(x)))
}
# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))


# normalize the occupancy data
occ_detect_n <- as.data.frame(lapply(occ_detect[3:6], normalize))

# confirm that normalization worked
summary(occ_detect_n)
# create training and test data
occ_detect_train <- occ_detect_n[1:469, 1]
occ_detect_test <- occ_detect_n[470:569, 1]
# create labels for training and test data
occ_detect_train_labels <- occ_detect[1:469, 1]
occ_detect_test_labels <- occ_detect[470:569, 1]
## Step 3: Training a model on the data ----
library(class)
occ_detect_test_pred <- knn(train = data.frame(occ_detect_train), test = data.frame(occ_detect_test), cl = occ_detect_train_labels, k=4)
## Step 4: Evaluating model performance ----
# load the "gmodels" library
#install.packages("gmodels")
library(gmodels)
# Create the cross tabulation of predicted vs. actual
CrossTable(x = occ_detect_test, y = occ_detect_test_pred,
           prop.chisq=FALSE)

