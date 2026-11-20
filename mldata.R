

print("STEP 4: VERIFYING ML DATASET")

ml_data <- movies_clean[, required_columns]

print("Dataset dimensions:")
print(dim(ml_data))

print("Dataset columns:")
print(names(ml_data))

print("No_of_Votes summary:")
print(summary(ml_data$No_of_Votes))

print("Missing values:")
print(colSums(is.na(ml_data)))

print("Dataset structure:")
str(ml_data)