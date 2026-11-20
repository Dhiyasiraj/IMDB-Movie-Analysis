# Check total dimensions (rows and columns)
dim(movies)
# Check column names
names(movies)
# Check structure of the dataset (data types)
str(movies)
# Generate summary statistics for all columns
summary(movies)
# Check the total number of missing (NA) values per column
colSums(is.na(movies))
