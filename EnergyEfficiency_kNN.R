energy_efficiency <- read.csv("data/Regression/EnergyEfficiency.csv", stringsAsFactors = FALSE)
str(energy_efficiency)

#reset NA to 0
#adult_income[is.na(adult_income)] <- 0
# table of Wage
table(energy_efficiency$HeatingLoad)
table(energy_efficiency$CoolingLoad)

# table or proportions with more informative labels
round(prop.table(table(energy_efficiency$HeatingLoad)) * 100, digits = 1)

# summarize three numeric features
summary(energy_efficiency[c("SurfaceArea", "RoofArea",
                       "WallArea")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# normalize the adult_income data
energy_efficiency_n <- as.data.frame(lapply(energy_efficiency[1:8], normalize))

# confirm that normalization worked
summary(energy_efficiency_n$SurfaceArea)

# create training and test data
energy_efficiency_train <- energy_efficiency_n[1:500, ]
energy_efficiency_test <- energy_efficiency_n[501:768, ]

# create labels for training and test data
energy_efficiency_train_labels <- energy_efficiency[1:500, 1]
energy_efficiency_test_labels <- energy_efficiency[501:768, 1]
## Step 3: Training a model on the data ----
library(class)
energy_efficiency_test_pred <- knn(train = energy_efficiency_train, test =
                                     energy_efficiency_test, cl = energy_efficiency_train_labels, k=1)

## Step 4: Evaluating model performance ----
# load the "gmodels" library
#install.packages("gmodels")
library(gmodels)
# Create the cross tabulation of predicted vs. actual
CrossTable(x = energy_efficiency_test_labels, y = energy_efficiency_test_pred,
           prop.chisq=FALSE, dnn = c('predicted', 'actual'))

## Making some mistakes more costly than others
# create a cost matrix
error_cost <- matrix(c(0, 1, 4, 0), 396, 396, byrow=TRUE)
error_cost
library(C50)
# apply the cost matrix to the tree
energy_efficiency_cost <- C5.0(as.factor(HeatingLoad) ~ ., data = energy_efficiency_train,
                    costs = error_cost)

#credit_cost <- C5.0(credit_train[-17], credit_train$default,
#                    costs = error_cost)

energy_efficiency_predictions <- predict(energy_efficiency_cost, energy_efficiency_test)

CrossTable(energy_efficiency_predictions, energy_efficiency_test$HeatingLoad,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

