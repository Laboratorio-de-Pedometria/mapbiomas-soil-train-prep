# title: Cross-Validation of SOC Stock Model
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2025
# To estimate the generalization error of the predictive model, a repeated 10-fold cross-validation
#  strategy is employed. This entire process is repeated 100 times to yield a more robust estimate
# of model performance. To prevent data leakage and avoid overly optimistic results, the following
# constraints are applied during the partitioning of data into validation folds:
# - For the SOC model, which includes augmented data, a leave-replicas-out approach is used. This
#   method guarantees that when an original data point is assigned to a validation fold, all of its
#   associated spatial or temporal replicas are also excluded from the training set for that fold.
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("ranger")) {
  install.packages("ranger")
}
if (!require("caret")) {
  install.packages("caret")
}

# Source helper functions
source("src/00_helper_functions.r")

# Build cross validation folds

# Set variables
res_tab_path <- "res/tab/"
random_seed <- 1984

# Read data from disk
matrix_path <- "~/Insync/Earth Engine Exports/matriz-col2v2.csv"
soildata <- data.table::fread(matrix_path)
# Check the data
dim(soildata)
# Rows: 23817
# Columns: 106

# Process data #####################################################################################
# Drop unwanted columns
soildata <- soildata[, c("system:index", ".geo") := NULL]

# Set cross-validation parameters
control <- train::trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 100,
  savePredictions = TRUE,
  allowParallel = TRUE,
  index = createDataPartition(soildata$SOC, p = 0.8, list = FALSE)
)