
library(ggplot2)
library(dplyr)
# Load data and clean columns so R reads them as numbers
movies <- read.csv("imdb_top_1000.csv.xls")
movies$Runtime <- as.numeric(gsub(" min", "", movies$Runtime))
movies$Gross <- as.numeric(gsub(",", "", movies$Gross))
# Histogram: Distribution of Movie Runtimes
ggplot(movies, aes(x = Runtime)) +
  geom_histogram(fill = "yellow", color = "black", bins = 30) +
  labs(
    title = "Distribution of Movie Runtimes",
    x = "Runtime (Minutes)",
    y = "Count"
  ) +
  theme_minimal()
#Scatter Plot: Runtime vs. IMDb Rating with Trendline
ggplot(movies, aes(x = Runtime, y = IMDB_Rating)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "lm", color = "yellow", se = FALSE) +
  labs(
    title = "Impact of Movie Runtime on IMDb Ratings",
    x = "Runtime (Minutes)",
    y = "IMDb Rating"
  ) +
  theme_minimal()
# Column chart IMDb Ratings Across Age Certificates
library(ggplot2)
library(dplyr)
# Calculate average rating per certificate
movies_cert_bar <- movies %>%
  filter(!is.na(Certificate) & Certificate != "") %>%
  group_by(Certificate) %>%
  summarise(avg_rating = mean(IMDB_Rating, na.rm = TRUE))
# Simple Column Chart
ggplot(movies_cert_bar, aes(x = Certificate, y = avg_rating)) +
  geom_col(fill = "yellow", color = "black") +
  labs(
    title = "Average IMDb Rating by Age Certificate",
    x = "Age Certificate",
    y = "Average IMDb Rating"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )#  Bar Chart: Top 10 Movie Genres by Frequency
top_genres <- movies %>%
  count(Genre, sort = TRUE) %>%
  head(10)
ggplot(top_genres, aes(x = reorder(Genre, n), y = n)) +
  geom_col(fill = "yellow", color = "black") +
  coord_flip() +
  labs(
    title = "Top 10 Movie Genres by Frequency",
    x = "Genre",
    y = "Number of Movies"
  ) +
  theme_minimal()

