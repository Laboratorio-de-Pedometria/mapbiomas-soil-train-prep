# title: MapBiomas Soil
# subtitle: 14. Estimate coarse fragments
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025 CC-BY
rm(list = ls())

# Set MapBiomas Soil Collection
collection <- "c3"

# Install and load required packages
if (!requireNamespace("ranger")) {
  install.packages("ranger")
}

# Source helper functions and packages
source("src/00_helper_functions.r")

# SOILDATA

# Read data from previous processing script
file_path <- "data/13_soildata.txt"
soildata <- data.table::fread(file_path, sep = "\t", na.strings = c("", "NA", "NaN"))
summary_soildata(soildata)
# Layers: 54555
# Events: 18870
# Georeferenced events: 16360
# Datasets: 265
soildata[, geomorphon := as.character(geomorphon)]

# DESIGN MATRIX

# Stoniness
soildata[, .N, by = pedregosidade]
# - -> NA_character_
soildata[grepl("^-$", pedregosidade), pedregosidade := NA_character_]
# Não pedregoso -> "Ausente"
soildata[grepl("Não pedregos", pedregosidade, ignore.case = TRUE), pedregosidade := "Ausente"]
# ausente -> "Ausente"
soildata[grepl("ausente", pedregosidade), pedregosidade := "Ausente"]
# Sem Pedregosidade -> "Ausente"
soildata[grepl("Sem Pedregosidade", pedregosidade), pedregosidade := "Ausente"]
# Pedregoso -> "Pedregosa"
soildata[grepl("Pedregos", pedregosidade, ignore.case = TRUE), pedregosidade := "Pedregosa"]
# Moderadamente pedregosa -> "Moderada"
soildata[grepl("Moderadamente pedregos", pedregosidade, ignore.case = TRUE), pedregosidade := "Moderada"]
# Extremamente pedregoso -> "Extrema"
soildata[grepl("Extremamente pedregos", pedregosidade, ignore.case = TRUE), pedregosidade := "Extrema"]
# Extremamente -> "Extrema"
soildata[grepl("Extremamente", pedregosidade, ignore.case = TRUE), pedregosidade := "Extrema"]
# Ligeiramente pedregoso -> "Ligeira"
soildata[grepl("Ligeiramente pedregos", pedregosidade, ignore.case = TRUE), pedregosidade := "Ligeira"]
# Ligeiramente -> "Ligeira"
soildata[grepl("Ligeiramente", pedregosidade, ignore.case = TRUE), pedregosidade := "Ligeira"]
# Forte -> "Muita"
soildata[grepl("Forte", pedregosidade, ignore.case = TRUE), pedregosidade := "Muita"]
# Muito -> "Muita"
soildata[grepl("Muito", pedregosidade, ignore.case = TRUE), pedregosidade := "Muita"]
# Pouca -> "Ligeira"
soildata[grepl("Pouca", pedregosidade, ignore.case = TRUE), pedregosidade := "Ligeira"]
soildata[, .N, by = pedregosidade]
# Stoniness levels: Ausente, Extrema, Ligeira, Moderada, Muita, Pedregosa
# New column 'stoniness' will copy only the relevant levels from 'pedregosidade'
soildata[, stoniness := ifelse(
  pedregosidade %in% c("Ausente", "Extrema", "Ligeira", "Moderada", "Muita", "Pedregosa"),
  pedregosidade, "unknown"
)]
soildata[, .N, by = stoniness]
# has_stoniness
# NA = "unknown"
# Ausente = FALSE
# All other levels = TRUE
soildata[, has_stoniness := ifelse(
  is.na(pedregosidade) | pedregosidade == "unknown", "unknown",
  ifelse(pedregosidade == "Ausente", FALSE, TRUE)
)]
soildata[, .N, by = has_stoniness]

# Rockiness
soildata[, .N, by = rochosidade]
# - -> NA_character_
soildata[grepl("^-$", rochosidade), rochosidade := NA_character_]
# Sem Rochosidade -> "Ausente"
soildata[grepl("Sem Rochosidade", rochosidade), rochosidade := "Ausente"]
# Não rochosa -> "Ausente"
soildata[grepl("Não rochos", rochosidade, ignore.case = TRUE), rochosidade := "Ausente"]
# Moderadamente rochosa -> "Moderada"
soildata[grepl("Moderadamente rochos", rochosidade, ignore.case = TRUE), rochosidade := "Moderada"]
# Ligeiramente rochosa -> "Ligeira"
soildata[grepl("Ligeiramente rochos", rochosidade, ignore.case = TRUE), rochosidade := "Ligeira"]
# Não rochoso -> "Ausente"
soildata[grepl("Não rochoso", rochosidade, ignore.case = TRUE), rochosidade := "Ausente"]
# Extremamente rochosa -> "Extrema"
soildata[grepl("Extremamente rochos", rochosidade, ignore.case = TRUE), rochosidade := "Extrema"]
# Extremamente -> "Extrema"
soildata[grepl("Extremamente", rochosidade, ignore.case = TRUE), rochosidade := "Extrema"]
# Muito rochosa -> "Muita"
soildata[grepl("Muito rochos", rochosidade, ignore.case = TRUE), rochosidade := "Muita"]
# Muito -> "Muita"
soildata[grepl("Muito", rochosidade, ignore.case = TRUE), rochosidade := "Muita"]
# ausente -> "Ausente"
soildata[grepl("ausente", rochosidade), rochosidade := "Ausente"]
# Rochoso -> "Rochosa"
soildata[grepl("Rochos", rochosidade, ignore.case = TRUE), rochosidade := "Rochosa"]
# Pouco -> "Ligeira"
soildata[grepl("Pouco", rochosidade, ignore.case = TRUE), rochosidade := "Ligeira"]
soildata[, .N, by = rochosidade]
# Rockiness levels: Ausente, Extrema, Ligeira, Moderada, Muita, Rochosa
# New column 'rockiness' will copy only the relevant levels from 'rochosidade'
soildata[, rockiness := ifelse(
  rochosidade %in% c("Ausente", "Extrema", "Ligeira", "Moderada", "Muita", "Rochosa"),
  rochosidade, "unknown"
)]
soildata[, .N, by = rockiness]
# has_rockiness
# NA = "unknown"
# Ausente = FALSE
# All other levels = TRUE
soildata[, has_rockiness := ifelse(
  is.na(rochosidade) | rochosidade == "unknown", "unknown",
  ifelse(rochosidade == "Ausente", FALSE, TRUE)
)]
soildata[, .N, by = has_rockiness]

# State (estado_id)
soildata[, .N, by = estado_id]
# Rio de Janeiro -> RJ
soildata[grepl("Rio de Janeiro", estado_id), estado_id := "RJ"]
soildata[, .N, by = estado_id]

# Target variable: Proportion of coarse fragments (esqueleto)
# Identify soil layers missing the proportion of coarse fragments
is_na_skeleton <- is.na(soildata[["esqueleto"]])
sum(is_na_skeleton) # 7894 layers out of 54555

# # Compute thickness of each soil layer
# soildata[, espessura := profund_inf - profund_sup]
# rockdata <- soildata[esqueleto == 1000, ]
# print(rockdata[, .(id, camada_nome, profund_sup, profund_inf, espessura)])
# # If a rock layer has thickness (espessura) greater than 10 cm, slice it into multiple layers of
# # about 10 cm that fit into the the original layer thickness (espessura). For example, if a rock
# # layer has thickness of 25 cm, slice it into three layers of thickness 8.33 cm each.
# rockdata_expanded <- data.table::data.table()
# for (i in 1:nrow(rockdata)) {
#   layer_thickness <- rockdata[i, espessura]
#   if (layer_thickness > 10) {
#     num_slices <- ceiling(layer_thickness / 10)
#     slice_thickness <- layer_thickness / num_slices
#     for (j in 0:(num_slices - 1)) {
#       new_layer <- data.table::copy(rockdata[i, ])
#       new_layer[, profund_sup := profund_sup + j * slice_thickness]
#       new_layer[, profund_inf := profund_sup + slice_thickness]
#       new_layer[, espessura := slice_thickness]
#       rockdata_expanded <- rbind(rockdata_expanded, new_layer)
#     }
#   } else {
#     rockdata_expanded <- rbind(rockdata_expanded, rockdata[i, ])
#   }
# }
# rockdata_expanded[, profund_sup := round(profund_sup, 1)]
# rockdata_expanded[, profund_inf := round(profund_inf, 1)]
# rockdata_expanded[, espessura := round(profund_inf - profund_sup, 1)]
# # Update layer order (camada_id) by soil profile (id)
# rockdata_expanded[, camada_id := seq_len(.N), by = id]
# nrow(rockdata_expanded) # 3735 layers
# print(rockdata_expanded[, .(id, camada_nome, profund_sup, profund_inf, espessura)])
# # Replace original rock layers with expanded rock layers.
# soildata <- soildata[is.na(esqueleto) | esqueleto < 1000, ]
# soildata <- rbind(soildata, rockdata_expanded)
# summary_soildata(soildata)
# # Layers: 57452
# # Events: 18870
# # Georeferenced events: 16360
# # Datasets: 265

# Identify soil layers with proportion of coarse fragments equal to 100%
is_rock <- soildata[!is_na_skeleton, esqueleto == 1000]
sum(is_rock) # 838 layers out of 54555

# Covariates

# Set covariates
sort(colnames(soildata))
covars2drop <- c(
  # Target variable
  "esqueleto", "terrafina",
  # Identifiers and metadata
  "dataset_titulo", "organizacao_nome", "dataset_licenca", "sisb_id", "ibge_id", "id", 
  "coord_precisao", "coord_fonte", "coord_datum", "pais_id", "municipio_id", "amostra_quanti",
  "amostra_area", "amostra_tipo", "camada_nome", "camada_id", "amostra_id", 
  # Redundant covariates
  "coord_x", "coord_y",  "data_ano", "taxon_sibcs", "taxon_st", "taxon_wrb", "profund_sup",
  "profund_inf", "ano_fonte", "pedregosidade", "rochosidade",
  # Near zero variance covariates
  "GurupiProv", "SaoLuisProv", "Stagnosols",
  # Non-predictive covariates
  "Massad_aguaProv", "DENSIC", "bdod_100_200cm"
)
# Check remaining covariates
colnames(soildata[, !..covars2drop])

# Check structure of the data
print(soildata[, !..covars2drop])

# 1. Feature selection: remove zero-variance and near-zero-variance predictors
covars_names <- colnames(soildata[, !..covars2drop])
near_zero_variance_covars <- caret::nearZeroVar(
  soildata[, ..covars_names],
  freqCut = 1000 / 1,
  uniqueCut = 10,
  saveMetrics = FALSE
)
near_zero_variance_covars <- colnames(soildata[, ..covars_names])[near_zero_variance_covars]
if (length(near_zero_variance_covars) == 0) {
  print("No covariates with near-zero variance found.")
} else {
  print("Covariates with near-zero variance:")
  print(near_zero_variance_covars)
}
# 'GurupiProv', 'SaoLuisProv', and 'Stagnosols' was moved to list above; "dist2sand" was kept
# covars_names <- setdiff(covars_names, near_zero_variance_covars)
# print(covars_names)

# 2. Feature selection: remove covariates with high correlation
# Compute Spearman correlation matrix between quantitative covariates
is_numeric <- sapply(soildata[, ..covars_names], is.numeric)
correlation_matrix <- cor(
  soildata[, ..covars_names][, is_numeric, with = FALSE],
  method = "spearman",
  use = "pairwise.complete.obs"
)
# Identify highly correlated covariates
cor_limit <- 0.95
high_correlation <- which(abs(correlation_matrix) >= cor_limit, arr.ind = TRUE)
high_correlation <- high_correlation[high_correlation[, 1] != high_correlation[, 2], ]
high_correlation <- high_correlation[order(high_correlation[, 1]), ]
if (nrow(high_correlation) == 0) {
  print("No covariates with high correlation found.")
} else {
  print("Covariates with high correlation:")
  print(high_correlation)
}

# Remove one of each pair of highly correlated covariates
# Programmatically identify which covariates to drop based on correlation count
if (nrow(high_correlation) > 0) {
  # Get the column names from indices
  high_cor_names <- data.frame(
    row_name = colnames(correlation_matrix)[high_correlation[, 1]],
    col_name = colnames(correlation_matrix)[high_correlation[, 2]],
    stringsAsFactors = FALSE
  )
  print("Highly correlated covariate pairs:")
  print(unique(high_cor_names))

  # Identify all variables involved in high correlations
  vars_in_cor <- unique(c(high_cor_names$row_name, high_cor_names$col_name))

  # For each correlated variable, count how many correlations it has
  cor_count <- table(c(high_cor_names$row_name, high_cor_names$col_name))
  cor_count <- sort(cor_count, decreasing = TRUE)
  print("\nNumber of high correlations per variable:")
  print(cor_count)

  # Programmatically decide which variables to drop
  # Strategy: For each unique pair, drop the variable with more total correlations
  # If tied, keep the one that appears first alphabetically (arbitrary but consistent)
  covars_to_drop <- character()
  processed_pairs <- character()

  for (i in 1:nrow(high_cor_names)) {
    var1 <- high_cor_names$row_name[i]
    var2 <- high_cor_names$col_name[i]
    pair_id <- paste(sort(c(var1, var2)), collapse = "_")

    # Skip if we've already processed this pair
    if (pair_id %in% processed_pairs) {
      next
    }
    processed_pairs <- c(processed_pairs, pair_id)

    # Count correlations for each variable
    count1 <- cor_count[var1]
    count2 <- cor_count[var2]

    # Drop the one with more correlations; if tied, drop the one that comes later alphabetically
    if (count1 > count2) {
      var_to_drop <- var1
    } else if (count2 > count1) {
      var_to_drop <- var2
    } else {
      # Tied: drop the one that comes later alphabetically
      var_to_drop <- ifelse(var1 > var2, var1, var2)
    }

    # Only add if not already in the drop list
    if (!var_to_drop %in% covars_to_drop) {
      covars_to_drop <- c(covars_to_drop, var_to_drop)
    }
  }

  cat("\nDropping", length(covars_to_drop), "covariates due to high correlation:\n")
  print(sort(covars_to_drop))

  # Update covars_names
  covars_names <- setdiff(covars_names, covars_to_drop)
  cat("\nTotal covariates remaining:", length(covars_names), "\n")
}
print(covars_names)

# Missing value imputation
# Use the missingness-in-attributes (MIA) approach with +/- Inf, with the indicator for missingness
# (mask) to impute missing values in the covariates
covariates <- data.table::as.data.table(
  imputation(soildata[, ..covars_names],
    method = "mia", na.replacement = list(cont = Inf, cat = "unknown"), na.indicator = TRUE
  )
)
print(covariates)

# MODELING

# Prepare grid of hyperparameters
# mtry. Previous tests have demonstrated that mtry is overwhelmingly the most influential
#   hyperparameter. As mtry increases, model performance improves. Its absolute correlation with ME,
#   MAE, RMSE, and NSE is consistently larger than 0.8. An mtry = 16 was used in 2024 (c2) and was
#   suggested in initial tests in 2025 (c3). Here we try to increase it further. mtry controls the
#   bias-variance trade-off of the model.
#   mtry <- c(2, 4, 8, 16) # 2024 and 2025
mtry <- c(16, 24, 32)
# max_depth. Previous tests have shown that increasing max_depth improves model more than
#   increasing the number of trees. This parameter defines how much each individual tree is allowed
#   to learn. The best results obtained in previous/inicial tests consistently were with max_depth
#   set 20 or 30. Here we try to fine tune it further.
# max_depth <- c(10, 20, 30, 40)
max_depth <- c(20, 25, 30)
# num_trees. This parameter is about stabilizing the model. Previous/initial tests produced only
#   small improvements when increasing num_trees when compared to increasing max_depth or mtry.
#   In order to be computationally efficient, we fix num_trees to 400 here.
# num_trees <- c(100, 200, 400, 800)
num_trees <- 400
# min_node_size. Previous/initial tests have using min_node_size = 1 causes slight overfitting,
# while increasing to 8 is too restrictive and causes underfitting. So values of 2 and 4 where
# consistently produced the best results in previous/initial tests. Here we try intermediate values.
# min_node_size <- c(1, 2, 4, 8)
min_node_size <- c(2, 3, 4)

hyperparameters <- expand.grid(num_trees, mtry, min_node_size, max_depth)
colnames(hyperparameters) <- c("num_trees", "mtry", "min_node_size", "max_depth")
print(hyperparameters)

# Fit ranger model testing different hyperparameters
t0 <- Sys.time()
hyper_results <- data.table::data.table()
for (i in 1:nrow(hyperparameters)) {
  print(hyperparameters[i, ])
  set.seed(1984)
  model <- ranger::ranger(
    y = soildata[!is_na_skeleton, esqueleto],
    x = covariates[!is_na_skeleton, ],
    num.trees = hyperparameters$num_trees[i],
    mtry = hyperparameters$mtry[i],
    min.node.size = hyperparameters$min_node_size[i],
    max.depth = hyperparameters$max_depth[i],
    always.split.variables = c(),
    replace = TRUE,
    verbose = TRUE,
    num.threads = parallel::detectCores() - 1
  )
  observed <- soildata[!is_na_skeleton, esqueleto]
  predicted <- model$predictions
  error <- observed - predicted
  residual <- mean(observed) - observed
  me <- mean(error)
  mae <- mean(abs(error))
  mse <- mean(error^2)
  rmse <- sqrt(mse)
  nse <- 1 - mse / mean(residual^2)
  slope <- coef(lm(observed ~ predicted))[2]
  hyper_results <- rbind(hyper_results, data.table::data.table(
    num_trees = hyperparameters$num_trees[i],
    mtry = hyperparameters$mtry[i],
    min_node_size = hyperparameters$min_node_size[i],
    max_depth = hyperparameters$max_depth[i],
    me = me,
    mae = mae,
    rmse = rmse,
    nse = nse,
    slope = slope
  ))
}
Sys.time() - t0

# Export the results to a TXT file
file_path <- paste0("res/tab/", collection, "_skeleton_ranger_hyperparameter_tunning.txt")
data.table::fwrite(hyper_results, file_path, sep = "\t")
if (FALSE) {
  # Read the results from disk
  hyper_results <- data.table::fread(file_path, sep = "\t")
}

# Assess results
# What is the Spearman correlation between hyperparameters and model performance metrics?
correlation <- round(cor(hyper_results, method = "spearman"), 2)
file_path <- paste0("res/tab/", collection, "_skeleton_ranger_hyperparameter_correlation.txt")
data.table::fwrite(correlation, file_path, sep = "\t")
print(correlation[1:4, 5:9])

# Sort the results by RMSE
hyper_results <- hyper_results[order(rmse)]

# Select the best hyperparameters
# Among smallest `rmse`, select the hyperparameters with the smallest `num_trees`.
# Then select the hyperparameters with the largest `nse`.
# Then select the hyperparameters with the smallest `max_depth`.
# Then select the hyperparameters with the smallest `mtry`.
# Then select the hyperparameters with the largest `min_node_size`.
digits <- 2
hyper_best <- round(hyper_results, digits)
hyper_best <- hyper_best[rmse == min(rmse), ]
hyper_best <- hyper_best[nse == max(nse), ]
hyper_best <- hyper_best[num_trees == min(num_trees), ]
hyper_best <- hyper_best[max_depth == min(max_depth), ]
hyper_best <- hyper_best[mtry == min(mtry), ]
hyper_best <- hyper_best[min_node_size == max(min_node_size), ]
print(hyper_best)

# Hard code the best hyperparameters for the model
hyper_best <- data.frame(num_trees = 400, mtry = 50, min_node_size = 2, max_depth = 25)

# Fit the best model
t0 <- Sys.time()
set.seed(2001)
skeleton_model <- ranger::ranger(
  y = soildata[!is_na_skeleton, esqueleto],
  x = covariates[!is_na_skeleton, ],
  num.trees = hyper_best$num_trees,  
  mtry = hyper_best$mtry,
  min.node.size = hyper_best$min_node_size,
  max.depth = hyper_best$max_depth,
  importance = "impurity",
  replace = TRUE,
  verbose = TRUE,
  num.threads = parallel::detectCores() - 1
)
Sys.time() - t0
print(skeleton_model)
# OOB prediction error (MSE): 4142.32
# R squared (OOB): 0.8568503

# Proportion of correctly classified rock layers (esqueleto == 1000)
# Tolerance of 0, 5, and 10%
round(sum(round(skeleton_model$predictions[is_rock] / 10) == 100) / sum(is_rock) * 100) # 90%
round(sum(round(skeleton_model$predictions[is_rock] / 10) >= 95) / sum(is_rock) * 100) # 99%
round(sum(round(skeleton_model$predictions[is_rock] / 10) >= 90) / sum(is_rock) * 100) # 100%

# Compute regression model statistics and write to disk
skeleton_model_stats <- error_statistics(
  soildata[!is_na_skeleton, esqueleto][!is_rock],
  skeleton_model$predictions[!is_rock]
)
file_path <- paste0("res/tab/", collection, "_skeleton_ranger_model_statistics.txt")
data.table::fwrite(skeleton_model_stats, file_path, sep = "\t")
print(round(skeleton_model_stats, 2))

# Write model parameters to disk
file_path <- paste0("res/tab/", collection, "_skeleton_ranger_model_parameters.txt")
write.table(capture.output(print(skeleton_model))[6:15],
  file = file_path, sep = "\t", row.names = FALSE
)
if (FALSE) {
  # Read the model parameters from disk
  skeleton_model <- data.table::fread(file_path, sep = "\t")
  print(skeleton_model)
}

# Check absolute error
abs_error_tolerance <- 100
soildata[
  !is_na_skeleton,
  abs_error := abs(soildata[!is_na_skeleton, esqueleto] - skeleton_model$predictions)
]
if (any(soildata[!is_na_skeleton, abs_error] >= abs_error_tolerance)) {
  print(soildata[
    abs_error >= abs_error_tolerance,
    .(id, camada_id, camada_nome, esqueleto, abs_error)
  ])
} else {
  print(paste0("All absolute errors are below ", abs_error_tolerance, " %."))
}
# 2900 layers with absolute error >= 100 %

# Figure: Variable importance
variable_importance_threshold <- 0.02
skeleton_model_variable <- sort(skeleton_model$variable.importance)
skeleton_model_variable <- round(skeleton_model_variable / max(skeleton_model_variable), 4)
file_path <- paste0("res/fig/", collection, "_skeleton_variable_importance.png")
png(file_path, width = 480 * 3, height = 480 * 4, res = 72 * 3)
par(mar = c(4, 6, 1, 1) + 0.1)
barplot(skeleton_model_variable[skeleton_model_variable >= variable_importance_threshold],
  horiz = TRUE, las = 1,
  col = "gray", border = "gray",
  xlab = paste("Relative importance >=", variable_importance_threshold), cex.names = 0.5
)
grid(nx = NULL, ny = FALSE, col = "gray")
dev.off()
# Which variables have importance below the threshold?
names(skeleton_model_variable[skeleton_model_variable < variable_importance_threshold])

# Figure: Plot fitted versus observed values
color_breaks <- seq(0, abs_error_tolerance, length.out = 5)
color_class <- cut(soildata[!is_na_skeleton, abs_error], breaks = color_breaks, include.lowest = TRUE)
color_palette <- RColorBrewer::brewer.pal(length(color_breaks) - 1, "Purples")
file_path <- paste0("res/fig/", collection, "_skeleton_observed_versus_oob.png")
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(mar = c(4, 4.5, 2, 2) + 0.1)
plot(
  y = soildata[!is_na_skeleton, esqueleto], x = skeleton_model$predictions,
  panel.first = grid(),
  pch = 21, bg = color_palette[as.numeric(color_class)],
  ylab = "Observed proportion of coarse fragments (%)",
  xlab = "Fitted proportion of coarse fragments (%)"
)
abline(0, 1)
legend("topleft",
  title = "Absolute error (%)",
  legend = levels(color_class),
  pt.bg = color_palette, border = "white", box.lwd = 0, pch = 21
)
dev.off()

# Predict soil skeleton
skeleton_digits <- 0
tmp <- predict(skeleton_model, data = covariates[is_na_skeleton, ])
soildata[is_na_skeleton, esqueleto := round(tmp$predictions, skeleton_digits)]
nrow(unique(soildata[, "id"])) # Result: 18870
nrow(soildata) # Result: 54555

# Figure. Distribution of soil skeleton data
file_path <- paste0("res/fig/", collection, "_skeleton_histogram.png")
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(mar = c(5, 4, 2, 2) + 0.1)
hist(soildata[, esqueleto],
  xlab = "Proportion of coarse fragments (%)",
  ylab = paste0("Absolute frequency (n = ", length(soildata[, esqueleto]), ")"),
  main = "", col = "gray", border = "gray"
)
grid(nx = FALSE, ny = NULL, col = "gray")
rug(soildata[!is_na_skeleton, esqueleto])
legend("topright",
  legend = c("All data (columns)", "Training data (rug)"),
  fill = c("gray", "black"),
  border = "white",
  box.lwd = 0
)
dev.off()

# Dataset for PSD modelling
tmp <- soildata[
  !is.na(esqueleto) &
    !is.na(argila) & !is.na(areia) & !is.na(silte) &
    !is.na(profund_sup) & !is.na(profund_inf)
]
summary_soildata(tmp)
# Layers: 47182
# Events: 16179
# Georeferenced events: 13966
# Datasets: 242

# Write data to disk ###############################################################################
soildata[, abs_error := NULL]
summary_soildata(soildata)
# Layers: 54555
# Events: 18870
# Georeferenced events: 16360
# Datasets: 265
data.table::fwrite(soildata, "data/14_soildata.txt", sep = "\t")
