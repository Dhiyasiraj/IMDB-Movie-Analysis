# STEP 4: SPLITTING THE DATASET

print("STEP 4: SPLITTING THE DATASET")

library(caret)

set.seed(123)

split_data <- ml_data[
  complete.cases(ml_data[, c(
    "Rating_Category",
    "Runtime",
    "Meta_score",
    "No_of_Votes",
    "Released_Year"
  )]),
]

train_index <- createDataPartition(
  split_data$Rating_Category,
  p = 0.80,
  list = FALSE
)

train_data <- split_data[train_index, ]
test_data <- split_data[-train_index, ]

print("Training dataset dimensions:")
print(dim(train_data))

print("Testing dataset dimensions:")
print(dim(test_data))

print("Training and testing split completed!")