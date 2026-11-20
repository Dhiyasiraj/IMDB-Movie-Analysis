# ALGORITHM 5: SUPPORT VECTOR MACHINE (SVM)

print("ALGORITHM 5: SUPPORT VECTOR MACHINE")

# Step 1: Load packages
library(e1071)
library(caret)

# Step 2: Prepare dataset
svm_data <- ml_data[, c(
  "Gross",
  "IMDB_Rating",
  "Meta_score",
  "Runtime",
  "No_of_Votes",
  "Released_Year"
)]

# Step 3: Convert variables into numeric format
svm_data$Gross <- as.numeric(gsub(",", "", as.character(svm_data$Gross)))
svm_data$IMDB_Rating <- as.numeric(as.character(svm_data$IMDB_Rating))
svm_data$Meta_score <- as.numeric(as.character(svm_data$Meta_score))
svm_data$Runtime <- as.numeric(as.character(svm_data$Runtime))
svm_data$No_of_Votes <- as.numeric(gsub(",", "", as.character(svm_data$No_of_Votes)))
svm_data$Released_Year <- as.numeric(as.character(svm_data$Released_Year))

# Step 4: Remove missing values
svm_data <- na.omit(svm_data)

print("SVM dataset dimensions:")
print(dim(svm_data))

# Step 5: Split dataset into training and testing
set.seed(123)

svm_index <- createDataPartition(
  svm_data$Gross,
  p = 0.8,
  list = FALSE
)

svm_train <- svm_data[svm_index, ]
svm_test <- svm_data[-svm_index, ]

print("Training records:")
print(nrow(svm_train))

print("Testing records:")
print(nrow(svm_test))

# Step 6: Create box-office categories
gross_median <- median(svm_train$Gross)

svm_train$Gross_Category <- factor(
  ifelse(svm_train$Gross >= gross_median, "High", "Low"),
  levels = c("Low", "High")
)

svm_test$Gross_Category <- factor(
  ifelse(svm_test$Gross >= gross_median, "High", "Low"),
  levels = c("Low", "High")
)

# Remove Gross to prevent data leakage
svm_train$Gross <- NULL
svm_test$Gross <- NULL

print("Box-office category distribution:")
print(table(svm_train$Gross_Category))

# Step 7: Build SVM model
svm_model <- svm(
  Gross_Category ~ .,
  data = svm_train,
  kernel = "radial",
  scale = TRUE
)

print("SUPPORT VECTOR MACHINE MODEL:")
print(svm_model)

# Step 8: Predict testing records
svm_pred <- predict(
  svm_model,
  newdata = svm_test
)

print("Actual box-office categories:")
print(head(svm_test$Gross_Category, 10))

print("Predicted box-office categories:")
print(head(svm_pred, 10))

# Step 9: Evaluate model
print("SVM CONFUSION MATRIX:")

svm_cm <- confusionMatrix(
  svm_pred,
  svm_test$Gross_Category
)

print(svm_cm)

print("SVM ACCURACY:")
print(round(svm_cm$overall["Accuracy"] * 100, 2))