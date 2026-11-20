
# ==========================================
# CART CLASSIFICATION - IMDb MOVIE ANALYSIS
# ==========================================

library(rpart)
library(rpart.plot)
library(caret)

# Step 2: Prepare the dataset
cart_data <- movies_clean[, c(
  "IMDB_Rating",
  "Runtime",
  "Meta_score",
  "No_of_Votes",
  "Released_Year"
)]

# Convert columns into numeric format
cart_data$IMDB_Rating <- as.numeric(cart_data$IMDB_Rating)
cart_data$Runtime <- as.numeric(cart_data$Runtime)
cart_data$Meta_score <- as.numeric(cart_data$Meta_score)
cart_data$No_of_Votes <- as.numeric(cart_data$No_of_Votes)
cart_data$Released_Year <- as.numeric(cart_data$Released_Year)

# Create rating categories
cart_data$Rating_Category <- factor(
  ifelse(cart_data$IMDB_Rating >= 8, "High", "Below_8"),
  levels = c("Below_8", "High")
)

# Remove original IMDb rating
cart_data$IMDB_Rating <- NULL

# Remove missing values
cart_data <- na.omit(cart_data)

print("CART Dataset Dimensions:")
print(dim(cart_data))

print("Rating Category Distribution:")
print(table(cart_data$Rating_Category))

# Step 3: Split dataset into training and testing
set.seed(123)

cart_index <- createDataPartition(
  cart_data$Rating_Category,
  p = 0.80,
  list = FALSE
)

cart_train <- cart_data[cart_index, ]
cart_test <- cart_data[-cart_index, ]

print("Training Dataset:")
print(dim(cart_train))

print("Testing Dataset:")
print(dim(cart_test))

# Step 4: Build initial CART model
cart_model <- rpart(
  Rating_Category ~ .,
  data = cart_train,
  method = "class",
  control = rpart.control(
    maxdepth = 3,
    minsplit = 30,
    minbucket = 10,
    cp = 0.01
  )
)

print("INITIAL CART MODEL:")
print(cart_model)

# Step 5: Build optimized CART model using cross-validation
set.seed(123)

cart_cv <- rpart(
  Rating_Category ~ .,
  data = cart_train,
  method = "class",
  control = rpart.control(
    maxdepth = 5,
    minsplit = 20,
    minbucket = 7,
    cp = 0.001,
    xval = 10
  )
)

print("CART CROSS-VALIDATION RESULTS:")
printcp(cart_cv)

# Step 6: Prune the optimized tree
cart_final <- prune(
  cart_cv,
  cp = 0.001
)

print("FINAL OPTIMIZED CART MODEL:")
print(cart_final)

# Step 7: Visualize optimized decision tree
rpart.plot(
  cart_final,
  type = 3,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Optimized CART Decision Tree"
)

# Step 8: Make predictions
cart_final_predictions <- predict(
  cart_final,
  cart_test,
  type = "class"
)

print("Actual Rating Categories:")
print(head(cart_test$Rating_Category, 10))

print("Predicted Rating Categories:")
print(head(cart_final_predictions, 10))

# Step 9: Evaluate optimized CART model
cart_final_cm <- confusionMatrix(
  cart_final_predictions,
  cart_test$Rating_Category
)

print("OPTIMIZED CART CONFUSION MATRIX:")
print(cart_final_cm)

# Step 10: Calculate model accuracy
cart_final_accuracy <- cart_final_cm$overall["Accuracy"]

print("OPTIMIZED CART ACCURACY:")
print(cart_final_accuracy)

# Step 11: Display variable importance
print("CART VARIABLE IMPORTANCE:")
print(cart_final$variable.importance)

# Step 12: Compare original and optimized accuracy
cart_original_predictions <- predict(
  cart_model,
  cart_test,
  type = "class"
)

cart_original_cm <- confusionMatrix(
  cart_original_predictions,
  cart_test$Rating_Category
)

cart_original_accuracy <- cart_original_cm$overall["Accuracy"]

print("ORIGINAL CART ACCURACY:")
print(cart_original_accuracy)

print("OPTIMIZED CART ACCURACY:")
print(cart_final_accuracy)

# ==========================================
# END OF CART IMPLEMENTATION
# ==========================================
