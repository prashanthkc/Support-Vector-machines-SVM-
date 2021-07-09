#####Support Vector Machines 

# Partition Data into train and test data
salary_train <- read.csv(file.choose(), stringsAsFactors = TRUE)
salary_test  <- read.csv(file.choose(), stringsAsFactors = TRUE)

summary(salary_train)
summary(salary_test)

# Training a model on the data ----
# Begin by training a simple linear SVM
install.packages("kernlab")
library(kernlab)

salary_classifier <- ksvm(Salary ~ ., data = salary_train, kernel = "vanilladot")
?ksvm

## Evaluating model performance ----
# predictions on testing dataset
salary_predictions <- predict(salary_classifier, salary_test)

table(salary_predictions, salary_test$Salary)
agreement <- salary_predictions == salary_test$Salary
table(agreement)
prop.table(table(agreement))

## Improving model performance ----
salary_classifier_rbf <- ksvm(Salary ~ ., data = salary_train, kernel = "rbfdot")
salary_predictions_rbf <- predict(salary_classifier_rbf, salary_test)
agreement_rbf <- salary_predictions_rbf == salary_test$Salary
table(agreement_rbf)
prop.table(table(agreement_rbf))

############################### PROBLERM  2 #################################

#####Support Vector Machines 

# Load the Dataset
forest<- read.csv(file.choose(), stringsAsFactors = TRUE)

summary(forest)

# Partition Data into train and test data
forest_train <- forest[1:300, ]
forest_test  <- forest[301:517, ]

# Training a model on the data ----
# Begin by training a simple linear SVM
install.packages("kernlab")
library(kernlab)

forest_classifier <- ksvm(size_category ~ ., data = forest_train, kernel = "vanilladot")
?ksvm

## Evaluating model performance ----
# predictions on testing dataset
forest_predictions <- predict(forest_classifier, forest_test)

table(forest_predictions, forest_test$size_category)
agreement <- forest_predictions == forest_test$size_category
table(agreement)
prop.table(table(agreement))

## Improving model performance ----
forest_classifier_rbf <- ksvm(size_category ~ ., data = forest_train, kernel = "rbfdot")
forest_predictions_rbf <- predict(forest_classifier_rbf, forest_test)
agreement_rbf <- forest_predictions_rbf == forest_test$size_category
table(agreement_rbf)
prop.table(table(agreement_rbf))
