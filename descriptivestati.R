# Print average IMDb rating
print(mean(movies_clean$IMDB_Rating, na.rm = TRUE))
# Print median IMDb rating
print(median(movies_clean$IMDB_Rating, na.rm = TRUE))
# Print standard deviation
print(sd(movies_clean$IMDB_Rating, na.rm = TRUE))
# Print minimum rating
print(min(movies_clean$IMDB_Rating, na.rm = TRUE))
# Print maximum rating
print(max(movies_clean$IMDB_Rating, na.rm = TRUE))
# Print average runtime
print(mean(movies_clean$Runtime, na.rm = TRUE))
# Print median runtime
print(median(movies_clean$Runtime, na.rm = TRUE))
# Print average Metascore
print(mean(movies_clean$Meta_score, na.rm = TRUE))
# Print average gross revenue
print(mean(movies_clean$Gross, na.rm = TRUE))

