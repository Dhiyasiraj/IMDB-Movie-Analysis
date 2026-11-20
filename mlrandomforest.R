# ALGORITHM 1: RANDOM FOREST REGRESSION

print("ALGORITHM 1: RANDOM FOREST REGRESSION")

library(randomForest)

# Prepare data
rf_data <- na.omit(ml_data[, c(
  "IMDB_Rating",
  "Runtime",
  "Meta_score",
  "No_of_Votes",
  "Released_Year"
)])

print("Random Forest dataset dimensions:")
print(dim(rf_data))

# Split dataset
set.seed(123)

rf_index <- sample(
  seq_len(nrow(rf_data)),
  size = floor(0.80 * nrow(rf_data))
)

rf_train <- rf_data[rf_index, ]
rf_test <- rf_data[-rf_index, ]

print("Training records:")
print(nrow(rf_train))

print("Testing records:")
print(nrow(rf_test))

# Build Random Forest model
set.seed(123)

rf_model <- randomForest(
  IMDB_Rating ~ Runtime +
    Meta_score + No_of_Votes + Released_Year,
  data = rf_train,
  ntree = 300,
  importance = TRUE
)

print("RANDOM FOREST MODEL:")
print(rf_model)

# Predict IMDb ratings
rf_pred <- predict(
  rf_model,
  newdata = rf_test
)

print("Actual IMDb ratings:")
print(head(rf_test$IMDB_Rating))

print("Predicted IMDb ratings:")
print(head(rf_pred))

# Calculate RMSE
rf_rmse <- sqrt(mean(
  (rf_test$IMDB_Rating - rf_pred)^2
))

print("RANDOM FOREST RMSE:")
print(round(rf_rmse, 4))

# Calculate MAE
rf_mae <- mean(
  abs(rf_test$IMDB_Rating - rf_pred)
)

print("RANDOM FOREST MAE:")
print(round(rf_mae, 4))

# Variable importance
print("VARIABLE IMPORTANCE:")
print(importance(rf_model))

varImpPlot(
  rf_model,
  main = "Random Forest Variable Importance"
)