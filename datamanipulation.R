library(dplyr)
# Filter: Find movies with an IMDb rating greater than or equal to 8.5
print(filter(movies_clean, IMDB_Rating >= 8.5))
# Arrange: Sort movies in ascending order by their Runtime
print(arrange(movies_clean, Runtime))
#Mutate: Create a new categorical column classifying movies based on Metascore
movies_clean <- mutate(
  movies_clean,
  score_category = ifelse(
    Meta_score >= 75,
    "Highly_Acclaimed",
    "Moderately_Acclaimed"))
print(head(movies_clean, 10))
#Summarise: Calculate overall summary statistics for movie runtime
print(summarise(
  movies_clean,
  average = mean(Runtime, na.rm = TRUE),
  minimum = min(Runtime, na.rm = TRUE),
  maximum = max(Runtime, na.rm = TRUE),
  median = median(Runtime, na.rm = TRUE)
))
# Calculate standard deviation for movie runtime
print(sd(movies_clean$Runtime, na.rm = TRUE))
#Group By & Summarise: Analyse average ratings and gross earnings grouped by Certificate type
certificate_analysis <- movies_clean %>%
  group_by(Certificate) %>%
  summarise(
    average_rating = mean(IMDB_Rating, na.rm = TRUE),
    average_gross = mean(Gross, na.rm = TRUE))
print(certificate_analysis)
