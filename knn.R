# ALGORITHM 3: K-NEAREST NEIGHBOURS

print("ALGORITHM 3: KNN SIMILAR MOVIE DISCOVERY")

# Prepare movie similarity data
knn_data <- na.omit(ml_data[, c(
  "Series_Title",
  "Runtime",
  "IMDB_Rating",
  "Meta_score",
  "No_of_Votes"
)])

print("KNN dataset dimensions:")
print(dim(knn_data))

# Standardize numerical variables
knn_scaled <- scale(knn_data[, c(
  "Runtime",
  "IMDB_Rating",
  "Meta_score",
  "No_of_Votes"
)])

print("Numerical attributes standardized!")

# Select a movie
query_title <- "Inception"

print("Selected movie:")
print(query_title)

# Find selected movie
query_index <- match(
  query_title,
  knn_data$Series_Title
)

if (!is.na(query_index)) {
  
  # Calculate distances
  distances <- sqrt(rowSums(
    (knn_scaled -
       matrix(
         knn_scaled[query_index, ],
         nrow = nrow(knn_scaled),
         ncol = ncol(knn_scaled),
         byrow = TRUE
       ))^2
  ))
  
  # Exclude selected movie
  distances[query_index] <- Inf
  
  # Find five nearest movies
  nearest_movies <- order(distances)[
    seq_len(min(5, length(distances) - 1))
  ]
  
  # Display similar movies
  similar_movies <- data.frame(
    Movie_Name = knn_data$Series_Title[nearest_movies],
    IMDb_Rating = knn_data$IMDB_Rating[nearest_movies],
    Distance = round(distances[nearest_movies], 3)
  )
  
  print("FIVE SIMILAR MOVIES:")
  print(similar_movies)
  
} else {
  
  print("Selected movie not found in the dataset.")
  
}