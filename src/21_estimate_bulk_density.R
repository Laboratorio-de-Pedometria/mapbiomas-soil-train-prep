# title: MapBiomas Soil
# subtitle: 21. Estimate Soil Bulk Density
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2025
rm(list = ls())

# Set MapBiomas Soil Collection
collection <- "c3"

# Source helper functions and packages
source("src/00_helper_functions.r")

# LOAD SOILDATA ####################################################################################
# Read SoilData data processed in the previous script. We go back to the file generated in step 14.
file_path <- "data/14_soildata.txt"
soildata <- data.table::fread(file_path, sep = "\t", na.strings = c("", "NA", "NaN"))
summary_soildata(soildata)
# Layers: 62468
# Events: 18882
# Georeferenced events: 16366
# Datasets: 265

# Check possible inconsistencies in soil bulk density values
soildata[dsi > 2.4, .(id, camada_nome, profund_sup, profund_inf, dsi)]
# Correct outliers (THIS HAS ALSO BEEN DONE IN THE SOURCE SPREADSHEET)
soildata[id == "ctb0793-PERFIL-01", dsi := ifelse(dsi == 2.44, 1.07, dsi)]
soildata[id == "ctb0811-50" & profund_sup == 40, dsi := ifelse(dsi == 2.5, 1.69, dsi)]
soildata[id == "ctb0811-50" & profund_sup == 80, dsi := ifelse(dsi == 2.5, 1.63, dsi)]
# Check again
soildata[dsi > 2.4, .(id, camada_nome, profund_sup, profund_inf, dsi)]

# Correct other outliers (after modelling) (THIS HAS ALSO BEEN DONE IN THE SOURCE SPREADSHEET)
soildata[id == "ctb0811-2", dsi := ifelse(dsi == 0.34, 1.64, dsi)]
soildata[id == "ctb0572-Perfil-063", dsi := ifelse(dsi == 0.34, 0.84, dsi)]
soildata[id == "ctb0572-Perfil-063", dsi := ifelse(dsi == 1.84, 0.84, dsi)]

# Correct other outliers (after modelling) (NOT CORRECTED IN THE SOURCE SPREADSHEET AS WE DO NOT HAVE
# ACCESS TO THE SOURCE TEXT DOCUMENT)
soildata[id == "ctb0829-P46", dsi := ifelse(dsi == 1.09, 2.09, dsi)]

# Update soil covariates: bulk density of upper and lower layer
# First, sort the data by soil event (id) and soil layer (camada_id).
# For each soil layer (camada_id) in a soil event (id), identify the bulk density (dsi) of the
# immediately upper and lower layers. If the layer is the first or last in a given soil event (id),
# the bulk density of the upper or lower layer is set to NA, respectively.
soildata <- soildata[order(id, camada_id)]
soildata[, dsi_upper := shift(dsi, type = "lag"), by = id]
soildata[, dsi_lower := shift(dsi, type = "lead"), by = id]
summary(soildata[, dsi_upper])
summary(soildata[, dsi_lower])

# DESIGN MATRIX FOR BULK DENSITY ESTIMATION ########################################################
# Check data type
print(soildata)

# Convert categorical variables to character
soildata[, geomorphon := as.character(geomorphon)]

# Target variable: soil bulk density (dsi)
# Identify soil layers missing soil bulk density data
is_na_dsi <- is.na(soildata[["dsi"]])
nrow(soildata[is.na(dsi), ]) # Result: 53095 layers
nrow(unique(soildata[is.na(dsi), "id"])) # Result: 15281 events

# Plot distribution of the target variable before inputation
file_path <- paste0("res/fig/", collection, "_bulk_density_histogram_before_imputation.png")
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(mar = c(5, 4, 2, 2) + 0.1)
hist(soildata[, dsi],
  xlab = "Soil Bulk Density (g/cm³)",
  ylab = paste0("Absolute frequency (n = ", length(na.exclude(soildata[, dsi])), ")"),
  main = "", col = "gray", border = "gray",
  breaks = seq(0, 3, by = 0.1)
)
grid(nx = FALSE, ny = NULL, col = "gray")
rug(soildata[!is_na_dsi, dsi])
dev.off()

# Covariates

# Set covariates for bulk density estimation
# We use all covariates available except those listed below
sort(colnames(soildata))
covars2drop <- c(
  # Target variable
  "dsi",
  # Identifiers and metadata
  "dataset_id", "observacao_id",
  "dataset_titulo", "organizacao_nome", "dataset_licenca", "sisb_id", "ibge_id", "id", 
  "coord_precisao", "coord_fonte", "coord_datum",
  "pais_id", "municipio_id",
  "amostra_quanti",
  "amostra_area", "amostra_tipo", "camada_nome", "camada_id", "amostra_id",
  # Redundant covariates
  "coord_x", "coord_y",  "data_ano", "taxon_sibcs", "taxon_st", "taxon_wrb", "profund_sup",
  "profund_inf", "ano_fonte", "pedregosidade", "rochosidade", "is_rock", "esqueleto",
  "Massad_aguaProv"
)
# Check remaining covariates
colnames(soildata[, !..covars2drop])

# Check structure of the data
print(soildata[, !..covars2drop])

# 1. Feature selection: remove zero-variance and near-zero-variance predictors
covars_names <- colnames(soildata[, !..covars2drop])
near_zero_variance_covars <- caret::nearZeroVar(
  soildata[!is_na_dsi, ..covars_names],
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
# GurupiProv, ReconcavoTucano_JatobaProv, SaoLuisProv, Stagnosols
covars_names <- setdiff(covars_names, near_zero_variance_covars)
print(sort(covars_names))

# 2. Feature selection: remove covariates with high correlation
# Compute Spearman correlation matrix between quantitative covariates
is_numeric <- sapply(soildata[, ..covars_names], is.numeric)
correlation_matrix <- cor(
  soildata[!is_na_dsi, ..covars_names][, is_numeric, with = FALSE],
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
# Dropping 17 covariates due to high correlation:
#  [1] "bdod_05_15cm"  "bdod_30_60cm"  "bdod_60_100cm" "cfvo_05_15cm" 
#  [5] "cfvo_15_30cm"  "cfvo_30_60cm"  "cfvo_60_100cm" "clay_05_15cm" 
#  [9] "clay_30_60cm"  "clay_60_100cm" "roughness"     "sand_05_15cm" 
# [13] "sand_15_30cm"  "sand_30_60cm"  "sand_60_100cm" "slope"        
# [17] "soc_60_100cm" 

# Missing value imputation
# Use the missingness-in-attributes (MIA) approach with +/- Inf, with the indicator for missingness
# (mask) to impute missing values in the covariates
covariates <- data.table::as.data.table(
  imputation(soildata[, ..covars_names],
    method = "mia", na.replacement = list(cont = Inf, cat = "unknown"), na.indicator = TRUE
  )
)
print(covariates)
ncol(covariates) # 273 covariates after feature selection

# MODELING #########################################################################################

# Prepare grid of hyperparameters
# mtry. This is the number of variables to possibly split at in each node. The parameter controls
# the trade-off between variance and bias. A lower mtry increases the variance and reduces the bias,
# while a higher mtry reduces the variance and increases the bias. The default value in ranger is
# the square root of the number of predictors for regression tasks. In Collection 2, we tested the
# following values: 2, 4, 8, and 16, with 16 being the overall best choice. In Collection 3, the
# default ranger value would be sqrt(273) ≈ 16.5. Therefore, we will drop the two lowermost values
# and add 24 and 32 to better cover the range of possible values.
# mtry <- c(2, 4, 8, 16) # Collection 2, with 16 being the best
mtry <- c(8, 16, 24, 32)  # Collection 3
# max.depth. This parameter limits the maximum depth of the trees. Deeper trees can capture more
# complex patterns but may lead to overfitting. Shallower trees are less likely to overfit but may
# underfit the data. In Collection 2, we tested the following values: 10, 20, 30, and 40, with 20
# being the overall best choice. We will keep the same values for Collection 3.
# max_depth <- c(10, 20, 30, 40) # Collection 2, with 20 being the best
max_depth <- c(10, 20, 30, 40) # Collection 3
# num.trees. This parameter defines the number of trees to grow in the forest. A higher number of
# trees can improve model performance but also increases computational cost. In Collection 2, we
# tested the following values: 100, 200, 400, and 800, with 400 being the overall best choice. We
# will drop the lowest value and add 600 to better cover the range of possible values.
# num_trees <- c(100, 200, 400, 800) # Collection 2, with 400 being the best
num_trees <- c(200, 400, 600, 800) # Collection 3
# min.node.size. This parameter sets the minimum size of nodes to be considered for splitting. It
# controls the complexity of the model by determining how many samples must be present in a node
# for it to be split further. A smaller min.node.size allows for more splits, leading to a more
# complex model that may capture intricate patterns in the data but also risks overfitting. A larger
# min.node.size results in fewer splits, producing a simpler model that may generalize better but
# could underfit the data. In Collection 2, we tested the following values: 1, 2, 4, and 8, with 1
# being the overall best choice. This is the default value for classification tasks in ranger, while
# for regression tasks the default is 5. We will drop "1", adding "6" to better cover the range of
# possible values.
# min_node_size <- c(1, 2, 4, 8) # Collection 2, with 1 being the best
min_node_size <- c(2, 4, 6, 8) # Collection 3
# min.bucket. This parameter sets the minimum size of terminal nodes (leaves) in the trees. It
# determines how many samples must be present in a leaf node. A smaller min.bucket allows for more
# detailed splits, leading to a more complex model that may capture intricate patterns in the data
# but also risks overfitting. A larger min.bucket results in fewer splits, producing a simpler model
# that may generalize better but could underfit the data. In Collection 2, we used the default value
# of 1. We will keep the same value for Collection 3.
# min_bucket <- c(1) # Collection 2, with 1 being the default
min_bucket <- c(1) # Collection 3
hyperparameters <- expand.grid(num_trees, mtry, min_node_size, max_depth, min_bucket)
colnames(hyperparameters) <- c("num_trees", "mtry", "min_node_size", "max_depth", "min_bucket")

# Fit ranger model testing different hyperparameters
t0 <- Sys.time()
hyper_results <- data.table::data.table()
for (i in 1:nrow(hyperparameters)) {
  print(hyperparameters[i, ])
  set.seed(1984)
  model <- ranger::ranger(
    y = soildata[!is_na_dsi, dsi],
    x = covariates[!is_na_dsi, ],
    num.trees = hyperparameters$num_trees[i],
    mtry = hyperparameters$mtry[i],
    min.node.size = hyperparameters$min_node_size[i],
    max.depth = hyperparameters$max_depth[i],
    min.bucket = hyperparameters$min_bucket[i],
    replace = TRUE,
    verbose = TRUE,
    num.threads = parallel::detectCores() - 1
  )
  observed <- soildata[!is_na_dsi, dsi]
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
    min_bucket = hyperparameters$min_bucket[i],
    me = me,
    mae = mae,
    rmse = rmse,
    nse = nse,
    slope = slope
  ))
}
Sys.time() - t0
# Time difference of 58.98823 mins

# Export the results to a TXT file
file_path <- paste0("res/tab/", collection, "_bulk_density_hyperparameter_tunning.txt")
data.table::fwrite(hyper_results, file_path, sep = "\t")
if (FALSE) {
  # Read the results from disk.
  hyper_results <- data.table::fread(file_path, sep = "\t")
}

# Assess results
# What is the Spearman correlation between hyperparameters and model performance metrics?
correlation <- round(cor(hyper_results, method = "spearman"), 2)
file_path <- paste0("res/tab/", collection, "_bulk_density_hyperparameter_correlation.txt")
data.table::fwrite(correlation, file_path, sep = "\t")
print(correlation[1:4, 6:10])
#                  me   mae  rmse   nse slope
# num_trees      0.11 -0.09 -0.11  0.11  0.09
# mtry           0.38 -0.61 -0.66  0.66 -0.61
# min_node_size -0.01  0.11  0.13 -0.13  0.10
# max_depth      0.62 -0.61 -0.56  0.56 -0.61

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
print(hyper_best[, -"min_bucket"])
#    num_trees  mtry min_node_size max_depth    me   mae  rmse   nse slope
#        <num> <num>         <num>     <num> <num> <num> <num> <num> <num>
# 1:       200    24             8        20     0  0.07  0.11  0.84  1.07

# Hard code the best hyperparameters for the model
hyper_best <- data.frame(
  num_trees = 200, mtry = 24, min_node_size = 8, max_depth = 20, min_bucket = 1
)

# Fit the best model
t0 <- Sys.time()
set.seed(2001)
dsi_model <- ranger::ranger(
  y = soildata[!is_na_dsi, dsi],
  x = covariates[!is_na_dsi, ],
  num.trees = hyper_best$num_trees,  
  mtry = hyper_best$mtry,
  min.node.size = hyper_best$min_node_size,
  max.depth = hyper_best$max_depth,
  min.bucket = hyper_best$min_bucket,
  importance = "impurity",
  replace = TRUE,
  verbose = TRUE,
  num.threads = parallel::detectCores() - 1
)
Sys.time() - t0
print(dsi_model)
# OOB prediction error (MSE): 0.01189643 
# R squared (OOB): 0.8372426

# Compute regression model statistics and write to disk
dsi_model_stats <- error_statistics(soildata[!is_na_dsi, dsi], dsi_model$predictions)
data.table::fwrite(
  dsi_model_stats, paste0("res/tab/", collection, "_bulk_density_model_statistics.txt"),
  sep = "\t"
)
print(round(dsi_model_stats, 2))
#           me  mae  mse rmse  mec slope
# predicted  0 0.07 0.01 0.11 0.84  1.07

# Write model parameters to disk
file_path <- paste0("res/tab/", collection, "_bulk_density_model_parameters.txt")
write.table(capture.output(print(dsi_model))[6:15], file = file_path, sep = "\t", row.names = FALSE)
if (FALSE) {
  # Read the model parameters from disk
  file_path <- paste0("res/tab/", collection, "_bulk_density_model_parameters.txt")
  dsi_model <- data.table::fread(file_path, sep = "\t")
  print(dsi_model)
}

# Check absolute error
# abs_error_tolerance <- 1.0 # Collection 2
abs_error_tolerance <- 0.9 # Collection 3
soildata[!is_na_dsi, abs_error := abs(soildata[!is_na_dsi, dsi] - dsi_model$predictions)]
if (any(soildata[!is_na_dsi, abs_error] >= abs_error_tolerance)) {
  print(soildata[
    abs_error >= abs_error_tolerance,
    .(id, camada_id, camada_nome, dsi, dsi_upper, dsi_lower, abs_error)
  ])
} else {
  print(paste0("All absolute errors are below ", abs_error_tolerance, " g/dm^3."))
}

# Figure: Variable importance
# Plot only those with relative importance >= 0.02
variable_importance_threshold <- 0.02
dsi_model_variable <- sort(dsi_model$variable.importance)
dsi_model_variable <- round(dsi_model_variable / max(dsi_model_variable), 4)
dev.off()
file_path <- paste0("res/fig/", collection, "_bulk_density_variable_importance.png")
png(file_path, width = 480 * 3, height = 480 * 4, res = 72 * 3)
par(mar = c(4, 6, 1, 1) + 0.1)
barplot(dsi_model_variable[dsi_model_variable >= variable_importance_threshold],
  horiz = TRUE, las = 1,
  col = "gray", border = "gray",
  xlab = paste("Relative importance >=", variable_importance_threshold), cex.names = 0.5
)
grid(nx = NULL, ny = FALSE, col = "gray")
dev.off()
# Which variables have importance below the threshold?
names(dsi_model_variable[dsi_model_variable < variable_importance_threshold])

# Figure: Plot fitted versus observed values
# Set color of points as a function of the absolute error, that is, abs(y - x)
color_breaks <- c(seq(0, abs_error_tolerance, length.out = 6), Inf)
color_class <- cut(soildata[!is_na_dsi, abs_error], breaks = color_breaks, include.lowest = TRUE)
color_palette <- c(RColorBrewer::brewer.pal(length(color_breaks) - 2, "Purples"), "red4")
dev.off()
file_path <- paste0("res/fig/", collection, "_bulk_density_observed_versus_oob.png")
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(mar = c(4, 4.5, 2, 2) + 0.1)
plot(
  y = soildata[!is_na_dsi, dsi], x = dsi_model$predictions,
  xlim = c(0, 2.5), ylim = c(0, 2.5),
  panel.first = grid(),
  pch = 21, bg = color_palette[as.numeric(color_class)],
  ylab = expression("Observed soil bulk density, g cm"^-3),
  xlab = expression("Fitted bulk soil density (OOB), g cm"^-3)
)
abline(0, 1)
legend("topleft", title = expression("Absolute error, g cm"^-3),
  legend = levels(color_class),
  pt.bg = color_palette, border = "white", box.lwd = 0, pch = 21
)
dev.off()
