# hier_data contains the four selected numeric columns.
hier_data$No_of_Votes <- log1p(hier_data$No_of_Votes)
hier_data$Gross <- log1p(hier_data$Gross)

hier_scaled <- scale(hier_data)

movie_dist <- dist(
  hier_scaled,
  method = "euclidean"
)
hier_model <- hclust(
  movie_dist,
  method = "ward.D2"
)
# Hierarchical clustering dendrogram
plot(
  hier_model,
  main = "Hierarchical Clustering of IMDb Movies",
  xlab = "Movies",
  sub = "",
  cex = 0.5
)
hier_clusters <- cutree(
  hier_model,
  k = 3
)
print(table(hier_clusters))
hier_summary <- aggregate(
  hier_data,
  by = list(Cluster = hier_clusters),
  FUN = mean
)
print(hier_summary)
