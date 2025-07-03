# title: Modeling Soil Organic Carbon Stock - Validation Statistics
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2025
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Source helper functions
source("src/00_helper_functions.r")

# Set variables
res_fig_path <- "res/fig/"
res_tab_path <- "res/tab/"
random_seed <- 1984

# Read validation data
# "res/tab/soc_model01_predictions.txt", sep = "\t"
# "res/tab/soc_model02_predictions.txt", sep = "\t"
# "res/tab/soc_model03_predictions.txt", sep = "\t"
soc_model01 <- data.table::fread("res/tab/soc_model01_predictions.txt", sep = "\t", header = TRUE)
soc_model02 <- data.table::fread("res/tab/soc_model02_predictions.txt", sep = "\t", header = TRUE)
soc_model03 <- data.table::fread("res/tab/soc_model03_predictions.txt", sep = "\t", header = TRUE)
soc_model <- rbind(soc_model01, soc_model02, soc_model03)
dim(soc_model)
# 15659     4

# General validation statistics (t/ha)
error_statistics(observed = soc_model$estoque / 100, predicted = soc_model$predicted / 100)

# Average observed and predicted values over groups
# Create groups
soc_model[, group_id := sub("-REP.*", "", dataset_id)]
# Calculate average observed and predicted values
soc_model_avg <- soc_model[, .(
  avg_observed = mean(estoque, na.rm = TRUE),
  avg_predicted = mean(predicted, na.rm = TRUE)
), by = group_id]
# Calculate error statistics for average values (t/ha)
error_statistics(
  observed = soc_model_avg$avg_observed / 100,
  predicted = soc_model_avg$avg_predicted / 100
)
