adult_income <- read.csv("data/Classification/Adult_Income.csv", stringsAsFactors = FALSE)
str(adult_income)

#reset NA to 0
adult_income[is.na(adult_income)] <- 0
# table of Wage
table(adult_income$Wage)

# table or proportions with more informative labels
round(prop.table(table(adult_income$Wage)) * 100, digits = 1)

# summarize three numeric features
summary(adult_income[c("Capital_Gain", "Capital_Loss",
               "Hours_Per_Week")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# normalize the adult_income data
adult_income_n <- as.data.frame(lapply(adult_income[11:13], normalize))

# confirm that normalization worked
summary(adult_income_n$Hours_Per_Week)

# create training and test data
adult_income_train <- adult_income_n[1:3000, ]
adult_income_test <- adult_income_n[3001:3900, ]

# create labels for training and test data
adult_income_train_labels <- adult_income[1:3000, 1]
adult_income_test_labels <- adult_income[3001:3900, 1]
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

