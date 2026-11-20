# STEP 3: CREATING ANALYTICAL CATEGORIES

print("STEP 3: CREATING ANALYTICAL CATEGORIES")

# Rating category
ml_data$Rating_Category <- factor(
  ifelse(
    ml_data$IMDB_Rating >= 8.0,
    "High",
    "Below_8"
  ),
  levels = c("Below_8", "High")
)

# Runtime category
ml_data$Runtime_Group <- cut(
  ml_data$Runtime,
  breaks = c(-Inf, 100, 150, Inf),
  labels = c("Short", "Medium", "Long")
)

# Extract primary genre
ml_data$Primary_Genre <- trimws(
  sub(",.*", "", as.character(ml_data$Genre))
)

print("Rating category distribution:")
print(table(ml_data$Rating_Category))

print("Runtime group distribution:")
print(table(ml_data$Runtime_Group))

print("Primary genre distribution:")
print(sort(
  table(ml_data$Primary_Genre),
  decreasing = TRUE
))

print("New categories created successfully!")