# ALGORITHM 6: K-MEANS CLUSTERING

print("ALGORITHM 6: K-MEANS MOVIE POPULARITY SEGMENTATION")

# Step 1: Load package
library(ggplot2)

# Step 2: Prepare dataset
kmeans_data <- ml_data[, c(
  "Series_Title",
  "IMDB_Rating",
  "No_of_Votes",
  "Gross",
  "Meta_score"
)]

# Step 3: Convert numerical variables
kmeans_data$IMDB_Rating <- as.numeric(
  as.character(kmeans_data$IMDB_Rating)
)

kmeans_data$No_of_Votes <- as.numeric(
  gsub(",", "", as.character(kmeans_data$No_of_Votes))
)

kmeans_data$Gross <- as.numeric(
  gsub(",", "", as.character(kmeans_data$Gross))
)

kmeans_data$Meta_score <- as.numeric(
  as.character(kmeans_data$Meta_score)
)

# Step 4: Remove missing values
kmeans_data <- na.omit(kmeans_data)

print("K-Means dataset dimensions:")
print(dim(kmeans_data))

# Step 5: Select numerical attributes
kmeans_features <- kmeans_data[, c(
  "IMDB_Rating",
  "No_of_Votes",
  "Gross",
  "Meta_score"
)]

# Step 6: Apply logarithmic transformation
kmeans_features$No_of_Votes <- log1p(
  kmeans_features$No_of_Votes
)

kmeans_features$Gross <- log1p(
  kmeans_features$Gross
)

# Step 7: Standardize numerical data
kmeans_scaled <- scale(kmeans_features)

print("Numerical attributes standardized!")

# Step 8: Apply K-Means clustering
set.seed(123)

kmeans_model <- kmeans(
  kmeans_scaled,
  centers = 3,
  nstart = 25
)

print("K-MEANS MODEL:")
print(kmeans_model)

# Step 9: Add cluster labels
kmeans_data$Cluster <- factor(
  kmeans_model$cluster
)

print("MOVIE CLUSTER DISTRIBUTION:")
print(table(kmeans_data$Cluster))

# Step 10: Calculate cluster averages
cluster_summary <- aggregate(
  kmeans_features,
  by = list(Cluster = kmeans_data$Cluster),
  FUN = mean
)

print("CLUSTER CHARACTERISTICS:")
print(cluster_summary)

# Step 11: Display movies in each cluster
print("SAMPLE MOVIES FROM EACH CLUSTER:")

for (i in 1:3) {
  
  print(paste("CLUSTER", i))
  
  print(
    head(
      kmeans_data[
        kmeans_data$Cluster == i,
        c("Series_Title", "IMDB_Rating", "Cluster")
      ],
      10
    )
  )
}

# Step 12: Visualize clusters
cluster_plot_data <- data.frame(
  kmeans_scaled,
  Cluster = kmeans_data$Cluster
)

print("K-MEANS CLUSTER VISUALIZATION:")

ggplot(
  cluster_plot_data,
  aes(
    x = IMDB_Rating,
    y = No_of_Votes,
    color = Cluster
  )
) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "IMDb Movie Popularity Segmentation Using K-Means",
    x = "IMDb Rating",
    y = "Log Transformed Number of Votes",
    color = "Movie Cluster"
  ) +
  theme_minimal()