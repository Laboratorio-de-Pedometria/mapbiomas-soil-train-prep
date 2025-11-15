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
# mtry
mtry <- c(2, 4, 8, 16)
# max.depth
max_depth <- c(10, 20, 30, 40)
# num.trees
num_trees <- c(100, 200, 400, 800)
# min.node.size
min_node_size <- c(1, 2, 4, 8)
hyperparameters <- expand.grid(num_trees, mtry, min_node_size, max_depth)
colnames(hyperparameters) <- c("num_trees", "mtry", "min_node_size", "max_depth")
