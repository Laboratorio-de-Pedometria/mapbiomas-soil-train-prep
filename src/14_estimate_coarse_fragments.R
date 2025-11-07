# title: MapBiomas Soil
# subtitle: 14. Estimate coarse fragments
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025 CC-BY
rm(list = ls())

# Set MapBiomas Soil Collection
collection <- "c3"

# Define maximum depth for soil layers to be used in modeling
max_depth <- 100

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
# Layers: 54105
# Events: 18870
# Georeferenced events: 16360
# Datasets: 265

# DESIGN MATRIX

# Convert categorical variables to character
soildata[, geomorphon := as.character(geomorphon)]

# Depth at the middle of the soil layer, then filter layers deeper than max_depth
soildata[, profundidade := (profund_inf + profund_sup) / 2]
soildata <- soildata[profundidade <= max_depth, ]
summary(soildata[, profundidade])
summary_soildata(soildata)
# Layers: 50118
# Events: 18845
# Georeferenced events: 16343
# Datasets: 265

# MANNUAL CORRECTION OF ERRORS IN THE TARGET VARIABLE ##############################################
# ctb0751
# ctb0751-53 is classified as "LATOSSOLO VERMELHO ESCURO ÁLICO cascalhento A moderado textura muito
# argilosa". The document reports values of coarse fragments as (%): 44 (A1), 22 (A3), 15 (B1),
# 95 (B21), and 9 (B22). We think that the value of 95% is a typo, and should be 9%. We found
# no evidence in the document of presence of such a high value of coarse fragments in this soil
# profile. So we correct this value here. If esqueleto is 950 g/kg, then it should be 90 g/kg and
# terrafina should be 910 g/kg.
soildata[id == "ctb0751-53" & esqueleto == 950, `:=`(terrafina = 910, esqueleto = 90)]

# EXPLORATORY DATA ANALYSIS #######################################################################
if (FALSE) {
  soildata_sf <- sf::st_as_sf(
    soildata[profundidade >= 0 &
    profundidade <= 10 &
    !is.na(coord_datum) & esqueleto > 0 & esqueleto < 1000, .(id, coord_x, coord_y, esqueleto)],
    coords = c("coord_x", "coord_y"),
    crs = 4674,
    remove = FALSE
  )
  nrow(soildata_sf) # 13296
  mapview::mapview(soildata_sf, zcol = "esqueleto", layer.name = "Soil skeleton (dag/kg)")
  # 00-10 cm: 3515 (11885)
  # 10-20 cm: 2096 (4729)
  # 20-30 cm: 1254 (3985)
  # 30-40 cm: 2286 (4797)
  # 40-50 cm: 1176 (3239)
  # 50-60 cm: 1105 (2768)
  # 60-70 cm: 1495 (3387)
  # 70-80 cm: 1450 (2932)
  # 80-90 cm: 784 (2110)
  # 90-100 cm: 682 (1703)
}

# Target variable: Proportion of coarse fragments (esqueleto)
# Identify soil layers missing the proportion of coarse fragments
is_na_skeleton <- is.na(soildata[["esqueleto"]])
sum(is_na_skeleton) # 7827 layers out of 50118

# Identify soil layers with proportion of coarse fragments equal to 100%
is_rock <- soildata[!is_na_skeleton, esqueleto == 1000]
sum(is_rock) # 756 layers out of 50118

# Covariates

# Set covariates
sort(colnames(soildata))
covars2drop <- c(
  # Target variable
  "esqueleto", "terrafina",
  # Identifiers and metadata
  "dataset_id", "observacao_id",
  "dataset_titulo", "organizacao_nome", "dataset_licenca", "sisb_id", "ibge_id", "id", 
  "coord_precisao", "coord_fonte", "coord_datum", "pais_id", "municipio_id", "amostra_quanti",
  "amostra_area", "amostra_tipo", "camada_nome", "camada_id", "amostra_id", 
  # Redundant covariates
  "coord_x", "coord_y",  "data_ano", "taxon_sibcs", "taxon_st", "taxon_wrb", "profund_sup",
  "profund_inf", "ano_fonte", "pedregosidade", "rochosidade",
  # Near zero variance covariates
  "GurupiProv", "SaoLuisProv", "Stagnosols", "dist2sand",
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
# 'GurupiProv', 'SaoLuisProv', 'Stagnosols', and 'dist2sand' were added to list above
covars_names <- setdiff(covars_names, near_zero_variance_covars)
print(sort(covars_names))

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

# Missing value imputation
# Use the missingness-in-attributes (MIA) approach with +/- Inf, with the indicator for missingness
# (mask) to impute missing values in the covariates
covariates <- data.table::as.data.table(
  imputation(soildata[, ..covars_names],
    method = "mia", na.replacement = list(cont = Inf, cat = "unknown"), na.indicator = TRUE
  )
)
print(covariates)

# MODELING #########################################################################################

# Prepare grid of hyperparameters
# mtry. Previous tests have demonstrated that mtry is overwhelmingly the most influential
#   hyperparameter. As mtry increases, model performance improves. Its absolute correlation with ME,
#   MAE, RMSE, and NSE is consistently larger than 0.8. An mtry = 16 was used in 2024 (c2) and was
#   suggested in initial tests in 2025 (c3). Here we try to increase it further. mtry controls the
#   bias-variance trade-off of the model.
#   mtry <- c(2, 4, 8, 16) # 2024 and 2025
# mtry <- c(16, 24, 32)
mtry <- c(24, 32, 48, 54)
# max_depth. Previous tests have shown that increasing max_depth improves model more than
#   increasing the number of trees. This parameter defines how much each individual tree is allowed
#   to learn. The best results obtained in previous/inicial tests consistently were with max_depth
#   set 20 or 30. Here we try to fine tune it further.
# max_depth <- c(10, 20, 30, 40)
# max_depth <- c(20, 25, 30)
max_depth <- c(25)
# num_trees. This parameter is about stabilizing the model. Previous/initial tests produced only
#   small improvements when increasing num_trees when compared to increasing max_depth or mtry.
#   In order to be computationally efficient, we fix num_trees to 400 here.
# num_trees <- c(100, 200, 400, 800)
num_trees <- 400
# min_node_size. Previous/initial tests have using min_node_size = 1 causes slight overfitting,
# while increasing to 8 is too restrictive and causes underfitting. So values of 2 and 4 where
# consistently produced the best results in previous/initial tests. Here we try intermediate values.
# min_node_size <- c(1, 2, 4, 8)
# min_node_size <- c(2, 3, 4)
min_node_size <- c(2, 3)
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
hyper_best <- data.frame(num_trees = 400, mtry = 54, min_node_size = 3, max_depth = 25)

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
# OOB prediction error (MSE): 4198.282
# R squared (OOB): 0.8512358

# Proportion of correctly classified rock layers (esqueleto == 1000)
# Tolerance of 0, 5, and 10%
round(sum(round(skeleton_model$predictions[is_rock] / 10) == 100) / sum(is_rock) * 100) # 92%
round(sum(round(skeleton_model$predictions[is_rock] / 10) >= 95) / sum(is_rock) * 100) # 98%
round(sum(round(skeleton_model$predictions[is_rock] / 10) >= 90) / sum(is_rock) * 100) # 100%

# Compute regression model statistics and write to disk
skeleton_model_stats <- error_statistics(
  soildata[!is_na_skeleton, esqueleto][!is_rock],
  skeleton_model$predictions[!is_rock]
)
file_path <- paste0("res/tab/", collection, "_skeleton_ranger_model_statistics.txt")
data.table::fwrite(skeleton_model_stats, file_path, sep = "\t")
print(round(skeleton_model_stats, 2))
#             me   mae     mse  rmse  mec slope
# predicted 1.59 24.14 4271.46 65.36 0.65  1.03

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
# 2691 layers with absolute error >= 100 %

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
  ylab = "Observed proportion of coarse fragments (dag/kg)",
  xlab = "Fitted proportion of coarse fragments (dag/kg)"
)
abline(0, 1)
legend("topleft",
  title = "Absolute error (dag/kg)",
  legend = levels(color_class),
  pt.bg = color_palette, border = "white", box.lwd = 0, pch = 21
)
dev.off()

# Predict soil skeleton
skeleton_digits <- 0
tmp <- predict(skeleton_model, data = covariates[is_na_skeleton, ])
soildata[is_na_skeleton, esqueleto := round(tmp$predictions, skeleton_digits)]
nrow(unique(soildata[, "id"])) # Result: 18845
nrow(soildata) # Result: 50118

# Figure. Distribution of soil skeleton data
file_path <- paste0("res/fig/", collection, "_skeleton_histogram.png")
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(mar = c(5, 4, 2, 2) + 0.1)
hist(soildata[, esqueleto],
  xlab = "Proportion of coarse fragments (dag/kg)",
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
# Layers: 43060
# Events: 16146
# Georeferenced events: 13943
# Datasets: 241

# Write data to disk ###############################################################################
soildata[, abs_error := NULL]
summary_soildata(soildata)
# Layers: 50118
# Events: 18845
# Georeferenced events: 16343
# Datasets: 265
data.table::fwrite(soildata, "data/14_soildata.txt", sep = "\t")
