library(e1071)
library(caret)
library(caTools)

# Read the dataset
ds <- read.csv("F:\\Year 2 Sem 1\\R programming\\Datasets\\student_portuguese_clean.csv")
ds
# Perform data splitting
split_ratio <- sample.split(ds, SplitRatio = 0.75)
training_dataset <- subset(ds, split_ratio == TRUE)
testing_dataset <- subset(ds, split_ratio == FALSE)

# Ensure 'final_grade' is a factor in both datasets with the same levels
training_dataset$final_grade <- as.factor(training_dataset$final_grade)
testing_dataset$final_grade <- as.factor(testing_dataset$final_grade)

# Create training and testing data frames with the selected columns
training_ds <- training_dataset[, 1:33]
testing_ds <- testing_dataset[, 1:33]

# Train the Naive Bayes model
set.seed(400)
model <- naiveBayes(final_grade ~ ., data = training_dataset)

# Make predictions on the testing dataset
predicted_results <- predict(model, newdata = testing_ds)

# Ensure predicted results are factors with the same levels as testing_dataset$final_grade
predicted_results <- factor(predicted_results, levels = levels(testing_dataset$final_grade))

# Create confusion matrix
confusionMatrix(predicted_results, testing_dataset$final_grade)