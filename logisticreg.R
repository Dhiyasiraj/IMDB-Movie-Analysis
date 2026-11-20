# ALGORITHM 2: LOGISTIC REGRESSION

print("ALGORITHM 2: LOGISTIC REGRESSION")

# Step 1: Prepare dataset
log_data <- ml_data[, c(
  "Rating_Category",
  "Runtime",
  "Meta_score",
  "No_of_Votes",
  "Released_Year"
)]

# Step 2: Convert variables into correct formats
log_data$Runtime <- as.numeric(as.character(log_data$Runtime))
log_data$Meta_score <- as.numeric(as.character(log_data$Meta_score))
log_data$No_of_Votes <- as.numeric(as.character(log_data$No_of_Votes))
log_data$Released_Year <- as.numeric(as.character(log_data$Released_Year))

# Remove missing values
log_data <- na.omit(log_data)

# Make sure target has two categories
log_data$Rating_Category <- factor(log_data$Rating_Category)

print("Logistic Regression dataset:")
print(dim(log_data))

# Step 3: Split dataset
set.seed(123)

library(caret)

log_index <- createDataPartition(
  log_data$Rating_Category,
  p = 0.8,
  list = FALSE
)

log_train <- log_data[log_index, ]
log_test <- log_data[-log_index, ]

print("Training records:")
print(nrow(log_train))

print("Testing records:")
print(nrow(log_test))

# Step 4: Build Logistic Regression model
log_model <- glm(
  Rating_Category ~ Runtime + Meta_score +
    No_of_Votes + Released_Year,
  data = log_train,
  family = binomial
)

print("LOGISTIC REGRESSION MODEL:")
print(summary(log_model))

# Step 5: Predict testing data
log_prob <- predict(
  log_model,
  newdata = log_test,
  type = "response"
)

log_pred <- ifelse(log_prob >= 0.5,
                   levels(log_train$Rating_Category)[2],
                   levels(log_train$Rating_Category)[1])

log_pred <- factor(
  log_pred,
  levels = levels(log_test$Rating_Category)
)

# Step 6: Evaluate model
print("LOGISTIC REGRESSION CONFUSION MATRIX:")

log_cm <- confusionMatrix(
  log_pred,
  log_test$Rating_Category
)

print(log_cm)

print("LOGISTIC REGRESSION ACCURACY:")
print(log_cm$overall["Accuracy"])

# Step 7: Save results
save.image("IMDb_Project_Backup.RData")