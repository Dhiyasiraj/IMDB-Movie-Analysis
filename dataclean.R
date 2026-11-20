# Load the dataset
movies <- read.csv("imdb_top_1000.csv.xls")
# Inspect initial data and missing values
print(head(movies))
print(str(movies))
print(colSums(is.na(movies)))
# Remove unwanted columns (Poster, Overview, and Star cast columns)
movies <- movies[, !(names(movies) %in% c("Poster_Link", "Overview", "Star1", "Star2", "Star3", "Star4"))]
# Clean Runtime and Gross columns (remove text and commas, convert to numeric)
movies$Runtime <- as.numeric(gsub(" min", "", movies$Runtime))
movies$Gross <- as.numeric(gsub(",", "", movies$Gross))
# Handle missing values by omitting rows with NA in critical columns
movies_clean <- na.omit(movies)
# Check and remove duplicate rows
print(duplicated(movies_clean))
movies_clean <- movies_clean[!duplicated(movies_clean), ]
# Convert categorical variables to factors
movies_clean$Certificate <- as.factor(movies_clean$Certificate)
movies_clean$Genre <- as.factor(movies_clean$Genre)
movies_clean$Director <- as.factor(movies_clean$Director)
# Print final summary of the cleaned dataset
print(summary(movies_clean))
movies_clean_unique <- unique(movies_clean)
print(movies_clean_unique)
