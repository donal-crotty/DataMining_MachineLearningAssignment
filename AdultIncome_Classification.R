
#install.packages("C50")
#install.packages("gmodels")
library(C50)
library(gmodels)

adult_income <- read.csv("data/classification/Adult_Income.csv", stringsAsFactors = FALSE)
head(adult_income)




#create a randomly shuffled sample for training and test data
set.seed(1)

adult_income_rand <- adult_income[order(runif(10000)), ]
adult_income_train <- adult_income_rand[1:8000, ]
adult_income_test <- adult_income_rand[8001:10000, ]

#Train
adult_income_model <- C5.0(as.factor(Wage) ~ ., data = adult_income_train)

summary(adult_income_model)
plot(adult_income_model)

adult_income_train$Wage <- as.factor(adult_income_train$Wage)
adult_income_test$Wage <- as.factor(adult_income_test$Wage)

#Evaluation
adult_income_predictions <- predict(adult_income_model, adult_income_test)
adult_income_predictions
?CrossTable
CrossTable(adult_income_predictions, adult_income_test$Wage,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn= c('predicted', 'actual'))


