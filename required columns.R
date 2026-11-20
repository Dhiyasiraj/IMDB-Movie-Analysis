print("STEP 3: CHECKING REQUIRED COLUMNS")

print("Available columns:")
print(names(movies_clean))

required_columns <- c(
  "Series_Title",
  "Released_Year",
  "Certificate",
  "Runtime",
  "Genre",
  "IMDB_Rating",
  "Meta_score",
  "No_of_Votes",
  "Gross"
)

missing_columns <- setdiff(
  required_columns,
  names(movies_clean)
)

print("Checking missing required columns:")
print(missing_columns)

if (length(missing_columns) == 0) {
  print("All required columns are available!")
} else {
  stop("Some required columns are missing. Check your dataset.")
}