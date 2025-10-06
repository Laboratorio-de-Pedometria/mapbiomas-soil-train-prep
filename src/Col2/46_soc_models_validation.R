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

# Validation statistics
# Brazil
brazil <- soc_model[, error_statistics(observed = estoque, predicted = predicted)]

# Amazon
amazon <- soc_model[Amazonia == 1, error_statistics(observed = estoque, predicted = predicted)]

# Pampa
pampa <- soc_model[Pampa == 1, error_statistics(observed = estoque, predicted = predicted)]

# Cerrado
cerrado <- soc_model[Cerrado == 1, error_statistics(observed = estoque, predicted = predicted)]

# Pantanal
pantanal <- soc_model[Pantanal == 1, error_statistics(observed = estoque, predicted = predicted)]

# Caatinga
caatinga <- soc_model[Caatinga == 1, error_statistics(observed = estoque, predicted = predicted)]

# Mata Atlantica
mata_atlantica <- soc_model[Mata_Atlantica == 1, error_statistics(observed = estoque, predicted = predicted)]

# rbind all validation statistics
validation_stats <- rbind(
  brazil = brazil,
  amazon = amazon,
  pampa = pampa,
  cerrado = cerrado,
  pantanal = pantanal,
  caatinga = caatinga,
  mata_atlantica = mata_atlantica
)
validation_stats <- round(validation_stats, 2)
print(validation_stats)

# Save validation statistics to a file
write.table(validation_stats, file = paste0(res_tab_path, "soc_validation_stats.txt"), sep = "\t")
