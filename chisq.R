# Load the dataset
movies <- read.csv("imdb_top_1000.csv.xls")
# Create a table of genre and certificate 
genre_certificate <- table( movies_clean$Genre, movies_clean$Certificate )
# Print the contingency table 
print(genre_certificate) 
# Perform Chi-Square test 
chi_test <- chisq.test(genre_certificate) 
# Print the test result 
print(chi_test)
