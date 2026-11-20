movies <- read.csv("imdb_top_1000.csv.xls") 
# Select movies with valid runtime and IMDb ratings 
runtime_data <- subset( movies_clean, !is.na(Runtime) & !is.na(IMDB_Rating) ) 
# Create runtime groups 
runtime_data$Runtime_Group <- ifelse( runtime_data$Runtime < 120, "Shorter", "Longer" )
# Perform independent samples t-test 
hypothesis_test <- t.test( IMDB_Rating ~ Runtime_Group, data = runtime_data ) 
# Print runtime groups 
print(runtime_data$Runtime_Group) 
# Print hypothesis test result 
print(hypothesis_test)
