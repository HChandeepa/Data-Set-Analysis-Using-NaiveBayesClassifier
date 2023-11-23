library(e1071)
library(caret)
library(caTools)

# Read the dataset
ds <- read.csv("F:\\Year 2 Sem 1\\R programming\\Datasets\\student_portuguese_clean.csv")
ds



#convert strings to  numeric-encoding
str(ds$school)
ds$school<-as.numeric(factor(ds$school))
ds$school
str(ds$sex)

ds$sex<-as.numeric(factor(ds$sex))
ds$sex



ds$address_type <- factor(ds$address_type, levels = c("Urban", "Suburban", "Rural")) 


ds$address_type <- as.numeric(ds$address_type)

str(ds$address_type)
ds$address_type<-as.numeric(ds$address_type)
ds$address_type
str(ds$address_type)
str(ds$family_size)
ds$family_size<-factor(ds$family_size,levels = c("Greater than 3","Less than or equal to 3"))
ds$family_size<-as.numeric(ds$family_size)
ds$family_size
str(ds$parent_status)
ds$parent_status<-factor(ds$parent_status,levels = c("Apart","Living together"))
ds$parent_status<-as.numeric(ds$parent_status)
ds$parent_status
str(ds$mother_education)

ds$mother_education <- factor(ds$mother_education, levels = c("higher education", "primary education (4th grade)", "secondary education", "5th to 9th grade"))
ds$mother_education <- as.numeric(ds$mother_education)
ds$mother_education

ds$father_education<-factor(ds$father_education,levels = c("higher education", "primary education (4th grade)", "secondary education", "5th to 9th grade","none"))
ds$father_education<-as.numeric(ds$father_education)
ds$father_education

unique(ds$school_choice_reason)
ds$school_choice_reason<-factor(ds$school_choice_reason,levels = c("course","other","home","reputation"))
ds$school_choice_reason<-as.numeric(ds$school_choice_reason)

unique(ds$study_time)
ds$study_time<-factor(ds$study_time,levels = c("2 to 5 hours","5 to 10 hours","<2 hours",">10 hours"))
ds$study_time<-as.numeric(ds$study_time)
ds$study_time


ds$school_support<-as.numeric(ds$school_support=="yes")
ds$school_support

unique(ds$extra_paid_classes)
ds$extra_paid_classes<-as.numeric(ds$extra_paid_classes=="yes")
ds$extra_paid_classes


ds$activities<-as.numeric(ds$activities=="yes")
ds$activities

ds$nursery_school<-as.numeric(ds$nursery_school=="yes")
ds$nursery_school

ds$higher_ed<-as.numeric(ds$higher_ed=="yes")
ds$higher_ed
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
ds$internet_access<-as.numeric(ds$internet_access=="yes")
ds$internet_access


unique(ds$guardian)
ds$guardian<-factor(ds$guardian,levels = c("mother", "father", "other" ))
ds$guardian<-as.numeric(ds$guardian)
ds$guardian

unique(ds$travel_time)
ds$travel_time<-factor(ds$travel_time,levels = c("15 to 30 min.","<15 min.","30 min. to 1 hour",">1 hour"))
ds$travel_time<-as.numeric(ds$travel_time)
ds$travel_time


#handling missing values
#sum(is.na(ds))

missing_cols <- colnames(ds)[colSums(is.na(ds)) >0]
missing_cols

# Impute missing values with column means
for (col in missing_cols) {
  ds[[col]][is.na(ds[[col]])] <- mean(ds[[col]], na.rm = TRUE)
}

#remove rows that with missing values
#na.omit(ds)

# Perform data splitting
split_ratio <- sample.split(ds, SplitRatio = 0.75)
training_dataset <- subset(ds, split_ratio == TRUE)
testing_dataset <- subset(ds, split_ratio == FALSE)


# Ensure 'final_grade' is a factor in both datasets with the same levels
training_dataset$final_grade <- as.factor(training_dataset$final_grade)
training_dataset$
testing_dataset$final_grade <- as.factor(testing_dataset$final_grade)

# Create training and testing data frames with the selected columns
training_ds <- training_dataset[, 1:34]
testing_ds <- testing_dataset[, 1:34]

# Train the Naive Bayes model
set.seed(400)
#model <- naiveBayes(final_grade ~ ., data = training_dataset)
# Train the Naive Bayes model using the correct data frame
# Train the Naive Bayes model using the correct data frame
model <- naiveBayes(final_grade ~ ., data = training_ds)

# Make predictions on the testing dataset
predicted_results <- predict(model, newdata = testing_ds)

# Convert predicted_results to a factor with the same levels as training_ds$final_grade
predicted_results <- factor(predicted_results, levels = levels(training_ds$final_grade))

# Convert testing_dataset$final_grade to a factor with the same levels as predicted_results
testing_dataset$final_grade <- factor(testing_dataset$final_grade, levels = levels(predicted_results))

# Create confusion matrix
confusionMatrix(predicted_results, testing_dataset$final_grade)


