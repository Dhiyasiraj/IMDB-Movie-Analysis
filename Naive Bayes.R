# ALGORITHM 4: IMPROVED NAIVE BAYES CLASSIFICATION

print("IMPROVED NAIVE BAYES CLASSIFICATION")

library(e1071)
library(caret)

# Step 1: Prepare dataset
nb_data <- ml_data[, c(
  "Genre",
  "Runtime",
  "IMDB_Rating",
  "Meta_score",
  "No_of_Votes",
  "Released_Year"
)]

# Step 2: Extract primary genre
nb_data$Primary_Genre <- trimws(
  sub(",.*", "", nb_data$Genre)
)

# Step 3: Convert numerical variables
nb_data$Runtime <- as.numeric(as.character(nb_data$Runtime))
nb_data$IMDB_Rating <- as.numeric(as.character(nb_data$IMDB_Rating))
nb_data$Meta_score <- as.numeric(as.character(nb_data$Meta_score))
nb_data$No_of_Votes <- as.numeric(as.character(nb_data$No_of_Votes))
nb_data$Released_Year <- as.numeric(as.character(nb_data$Released_Year))

# Step 4: Select five most frequent genres
genre_count <- sort(
  table(nb_data$Primary_Genre),
  decreasing = TRUE
)

top_genres <- names(genre_count)[1:5]

print("Selected genres:")
print(top_genres)

nb_data <- nb_data[
  nb_data$Primary_Genre %in% top_genres,
]

# Step 5: Remove missing values
nb_data <- na.omit(nb_data)

# Step 6: Transform number of votes
nb_data$No_of_Votes <- log1p(nb_data$No_of_Votes)

# Remove original Genre column
nb_data$Genre <- NULL

# Convert target into factor
nb_data$Primary_Genre <- factor(nb_data$Primary_Genre)

print("Improved Naive Bayes dataset:")
print(dim(nb_data))

print("Genre distribution:")
print(table(nb_data$Primary_Genre))

# Step 7: Split dataset
set.seed(123)

nb_index <- createDataPartition(
  nb_data$Primary_Genre,
  p = 0.8,
  list = FALSE
)

nb_train <- nb_data[nb_index, ]
nb_test <- nb_data[-nb_index, ]

print("Training records:")
print(nrow(nb_train))

print("Testing records:")
print(nrow(nb_test))

# Step 8: Build improved model
nb_model <- naiveBayes(
  Primary_Genre ~ .,
  data = nb_train,
  laplace = 1
)

print("IMPROVED NAIVE BAYES MODEL:")
print(nb_model)

# Step 9: Predict genres
nb_pred <- predict(
  nb_model,
  newdata = nb_test
)

# Step 10: Evaluate model
print("IMPROVED NAIVE BAYES CONFUSION MATRIX:")

nb_cm <- confusionMatrix(
  nb_pred,
  nb_test$Primary_Genre
)

print(nb_cm)

print("IMPROVED NAIVE BAYES ACCURACY:")
print(round(nb_cm$overall["Accuracy"] * 100, 2))

# Step 11: Save results
save.image("IMDb_Project_Backup.RData")