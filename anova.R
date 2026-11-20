# Load the dataset
movies <- read.csv("imdb_top_1000.csv.xls")
# Remove records with missing genre or IMDb rating 
anova_data <- na.omit( movies_clean[, c("Genre", "IMDB_Rating")] )
# Convert genre into a categorical variable 
anova_data$Genre <- as.factor(anova_data$Genre) 
# Perform One-Way ANOVA 
anova_test <- aov( IMDB_Rating ~ Genre, data = anova_data ) 
# Print ANOVA results 
print(summary(anova_test))
