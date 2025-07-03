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
soc_model01 <- data.table::fread("res/tab/soc_model01_predictions.txt", sep = "\t", header = TRUE)
soc_model02 <- data.table::fread("res/tab/soc_model02_predictions.txt", sep = "\t", header = TRUE)
soc_model03 <- data.table::fread("res/tab/soc_model03_predictions.txt", sep = "\t", header = TRUE)
soc_model <- rbind(soc_model01, soc_model02, soc_model03, fill = TRUE)
soc_model[, estoque := estoque / 100] # Convert to t/ha
soc_model[, predicted := predicted / 100] # Convert to t/ha
dim(soc_model)
# 15659     10

# Create groups
soc_model[, group_id := sub("-REP.*", "", dataset_id)]
# Calculate average over groups
soc_model_avg <- soc_model[, .(
  estoque = mean(estoque, na.rm = TRUE),
  predicted = mean(predicted, na.rm = TRUE),
  Amazonia = max(Amazonia, na.rm = TRUE),
  Pampa = max(Pampa, na.rm = TRUE),
  Cerrado = max(Cerrado, na.rm = TRUE),
  Pantanal = max(Pantanal, na.rm = TRUE),
  Caatinga = max(Caatinga, na.rm = TRUE),
  Mata_Atlantica = max(Mata_Atlantica, na.rm = TRUE)
  ),
  by = group_id]
dim(soc_model_avg)
# 12196     3

# Validation statistics - Brazil ###################################################################
rbind(
  # General validation statistics (t/ha)
  soc_model[, error_statistics(observed = estoque, predicted = predicted)],
  # Calculate error statistics for average values (t/ha)
  soc_model_avg[, error_statistics(observed = estoque, predicted = predicted)]
)

# Validation statistics - Amazon ###################################################################
rbind(
  # General validation statistics (t/ha)
  soc_model[Amazonia == 1, error_statistics(observed = estoque, predicted = predicted)],
  # Calculate error statistics for average values (t/ha)
  soc_model_avg[Amazonia == 1, error_statistics(observed = estoque, predicted = predicted)]
)

# Validation statistics - Pampa ###################################################################
rbind(
  # General validation statistics (t/ha)
  soc_model[Pampa == 1, error_statistics(observed = estoque, predicted = predicted)],
  # Calculate error statistics for average values (t/ha)
  soc_model_avg[Pampa == 1, error_statistics(observed = estoque, predicted = predicted)]
)

# Validation statistics - Cerrado ###################################################################
rbind(
  # General validation statistics (t/ha)
  soc_model[Cerrado == 1, error_statistics(observed = estoque, predicted = predicted)],
  # Calculate error statistics for average values (t/ha)
  soc_model_avg[Cerrado == 1, error_statistics(observed = estoque, predicted = predicted)]
)

# Validation statistics - Pantanal #################################################################
rbind(
  # General validation statistics (t/ha)
  soc_model[Pantanal == 1, error_statistics(observed = estoque, predicted = predicted)],
  # Calculate error statistics for average values (t/ha)
  soc_model_avg[Pantanal == 1, error_statistics(observed = estoque, predicted = predicted)]
)

# Validation statistics - Caatinga #################################################################
rbind(
  # General validation statistics (t/ha)
  soc_model[Caatinga == 1, error_statistics(observed = estoque, predicted = predicted)],
  # Calculate error statistics for average values (t/ha)
  soc_model_avg[Caatinga == 1, error_statistics(observed = estoque, predicted = predicted)]
)

# Validation statistics - Mata Atlantica ###########################################################
rbind(
  # General validation statistics (t/ha)
  soc_model[Mata_Atlantica == 1, error_statistics(observed = estoque, predicted = predicted)],
  # Calculate error statistics for average values (t/ha)
  soc_model_avg[Mata_Atlantica == 1, error_statistics(observed = estoque, predicted = predicted)]
)
