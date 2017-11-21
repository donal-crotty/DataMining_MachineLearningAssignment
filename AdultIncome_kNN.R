adult_income <- read.csv("data/Classification/Adult_Income.csv", stringsAsFactors = FALSE)
str(adult_income)

#reset NA to 0
adult_income[is.na(adult_income)] <- 0
# table of Wage
table(adult_income$Wage)
# recode Wage as a factor >50K recoded to Yes, <=50k recoded to No
#There if user makes more than 50k, it is Yes, else No
#adult_income$Wage <- factor(adult_income$Wage,
 #                        levels = c(">50K", "<=50K"),
  #                       labels = c("Yes", "No"))

# table or proportions with more informative labels
round(prop.table(table(adult_income$Wage)) * 100, digits = 1)

# summarize three numeric features
summary(adult_income[c("Capital_Gain", "Capital_Loss",
               "Hours_Per_Week")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# test normalization function prior to using it - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the adult_income data
adult_income_n <- as.data.frame(lapply(adult_income[11:13], normalize))

# confirm that normalization worked
summary(adult_income_n$Capital_Gain)

# create training and test data
adult_income_train <- adult_income_n[1:8000, ]
adult_income_test <- adult_income_n[8001:10000, ]

# create labels for training and test data
adult_income_train_labels <- adult_income[1:8000, 1]
adult_income_test_labels <- adult_income[8001:10000, 1]
## Step 3: Training a model on the data ----
library(class)
adult_income_test_pred <- knn(train = adult_income_train, test =
                                adult_income_test, cl = adult_income_train_labels, k=5)
## Step 4: Evaluating model performance ----
# load the "gmodels" library
#install.packages("gmodels")
library(gmodels)
# Create the cross tabulation of predicted vs. actual
CrossTable(x = adult_income_test_labels, y = adult_income_test_pred,
           prop.chisq=FALSE)

