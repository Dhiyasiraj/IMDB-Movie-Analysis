# Load the dataset 
movies <- read.csv("imdb_top_1000.csv.xls")
# Remove records with missing values 
regression_data <- na.omit( movies_clean[, c("Meta_score", "IMDB_Rating")] )
# Create the linear regression model
model <- lm( IMDB_Rating ~ Meta_score, data = regression_data ) 
# Print the model
print(model)
# Print the model summary 
print(summary(model))
