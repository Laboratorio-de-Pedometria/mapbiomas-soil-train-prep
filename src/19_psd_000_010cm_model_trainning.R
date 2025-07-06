# title: Modeling Soil Particle Size Distribution
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2025
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("ranger")) {
  install.packages("ranger")
}
if (!require("sf")) {
  install.packages("sf")
}
if (!require("mapview")) {
  install.packages("mapview")
}
if (!require("caret")) {
  install.packages("caret")
}

# Source helper functions
source("src/00_helper_functions.r")

# Set variables
res_fig_path <- "res/fig/"
res_tab_path <- "res/tab/"
random_seed <- 1984
n_cores <- parallel::detectCores() - 1

# Read data from disk
soildata <- read_insync("psd_c02beta_000_010cm_v2.csv")
# Check the data
dim(soildata)
# Rows: 19944
# Columns: 79

# Process data #####################################################################################
# Drop unwanted covariates
colnames(soildata)
soildata <- soildata[
  ,
  c("system:index", "depth_5", "latitude", "longitude", "ndvi", ".geo") := NULL
]
# Check the data again
dim(soildata)
# Rows: 19944
# Columns: 73

# Check data #######################################################################################
# Compare to the original training data 
original_path <- "~/Insync/MapBiomas Solo/Trainning samples/"
original_name <- "-clay-silt-sand-log-ratio.csv"
# List existing files in the folder_path and get the last one. Then read it.
existing_files <- list.files(path = original_path, pattern = original_name)
last_file <- existing_files[length(existing_files)]
original <- data.table::fread(paste0(original_path, last_file))
# If the number of rows is different, check which rows are missing in the matrix. Then create a
# spatial data and plot using mapview.
print(paste0("Number of rows in original: ", nrow(original)))
print(paste0("Number of rows in soildata: ", nrow(soildata)))
# Check which rows are missing in the matrix
missing_rows <- setdiff(original$id, soildata$id)
# Get the missing rows, create spatial data and plot using mapview
missing_original <- original[id %in% missing_rows]
# Create spatial data
missing_original_sf <- sf::st_as_sf(missing_original, coords = c("coord_x", "coord_y"), crs = 4326)
# Plot using mapview
mapview::mapview(missing_original_sf, zcol = "id", layer.name = "Missing samples")
# Print the number of missing rows
print(paste0("Number of missing rows: ", length(missing_rows)))
# Print the missing rows
print(missing_rows)

# log_clay_sand ####################################################################################
# Fit ranger model to predict log_clay_sand using all columns as predictors,
# except for log_silt_sand.
log_clay_sand_model <- ranger::ranger(
  formula = log_clay_sand ~ .,
  data = soildata[, !c("log_silt_sand", "id")],
  num.trees = 100,
  mtry = 16,
  min.node.size = 2,
  max.depth = 30,
  replace = TRUE,
  verbose = TRUE,
  importance = "impurity",
  seed = random_seed
)
# Print model summary
print(log_clay_sand_model)
# Compute error metrics
observed <- soildata$log_clay_sand
predicted <- log_clay_sand_model$predictions
error <- observed - predicted
residual <- mean(observed) - observed
me <- mean(error)
mae <- mean(abs(error))
mse <- mean(error^2)
rmse <- sqrt(mse)
mec <- 1 - mse / mean(residual^2)
slope <- coef(lm(observed ~ predicted))[2]
# Print error metrics
error_metrics <- data.frame(
  target = "log_clay_sand_000_010cm",
  ME = me,
  MAE = mae,
  MSE = mse,
  RMSE = rmse,
  MEC = mec,
  Slope = slope
)
print(error_metrics)

# Compute variable importance
importance <- log_clay_sand_model$variable.importance
importance <- data.table(
  variable = names(importance),
  importance = importance
)
importance <- importance[order(importance, decreasing = TRUE)]
# Divide importance by the maximum value
importance$importance <- importance$importance / max(importance$importance)

# Plot variable importance using base R, splitting into multiple plots if needed
num_vars <- nrow(importance)
vars_per_plot <- ceiling(num_vars/2)
num_plots <- ceiling(num_vars / vars_per_plot)
png(paste0(res_fig_path, "psd_varimp_log_clay_sand_000_010cm.png"),
  width = 480 * 3, height = 480 * 2, res = 72 * 2
)
par(mar = c(5, 10, 2, 2), mfrow = c(1, num_plots))
xlim <- c(0, max(importance$importance))
for (i in 1:num_plots) {
  start_idx <- (i - 1) * vars_per_plot + 1
  end_idx <- min(i * vars_per_plot, num_vars)
  barplot(
    rev(importance$importance[start_idx:end_idx]),
    names.arg = rev(importance$variable[start_idx:end_idx]),
    las = 1,
    main = "",
    xlab = "Relative importance",
    ylab = "",
    col = "lightgray",
    border = "darkgray",
    cex.names = 0.7,
    cex.axis = 0.7,
    cex.lab = 0.7,
    horiz = TRUE,
    xlim = xlim
  )
}
mtext("Variable Importance: log_clay_sand Model (0-10cm)",
  side = 3, line = -2, outer = TRUE, cex = 1, font = 1
)
dev.off()

# log_silt_sand ####################################################################################
# Fit ranger model to predict log_silt_sand using all columns as predictors,
# except for log_clay_sand.
log_silt_sand_model <- ranger::ranger(
  formula = log_silt_sand ~ .,
  data = soildata[, !c("log_clay_sand", "id")],
  num.trees = 100,
  mtry = 16,
  min.node.size = 2,
  max.depth = 30,
  replace = TRUE,
  verbose = TRUE,
  importance = "impurity",
  seed = random_seed
)
# Print model summary
print(log_silt_sand_model)
# Compute error metrics
observed <- soildata$log_silt_sand
predicted <- log_silt_sand_model$predictions
error <- observed - predicted
residual <- mean(observed) - observed
me <- mean(error)
mae <- mean(abs(error))
mse <- mean(error^2)
rmse <- sqrt(mse)
mec <- 1 - mse / mean(residual^2)
slope <- coef(lm(observed ~ predicted))[2]
# Print error metrics
error_metrics <- rbind(
  error_metrics,
  data.frame(
    target = "log_silt_sand_000_010cm",
    ME = me,
    MAE = mae,
    MSE = mse,
    RMSE = rmse,
    MEC = mec,
    Slope = slope
  )
)
# Print error metrics
print(error_metrics)

# Compute variable importance
importance <- log_silt_sand_model$variable.importance
importance <- data.table(
  variable = names(importance),
  importance = importance
)
importance <- importance[order(importance, decreasing = TRUE)]
# Divide importance by the maximum value
importance$importance <- importance$importance / max(importance$importance)
# Plot variable importance using base R, splitting into multiple plots if needed
num_vars <- nrow(importance)
vars_per_plot <- ceiling(num_vars / 2)
num_plots <- ceiling(num_vars / vars_per_plot)
png(paste0(res_fig_path, "psd_varimp_log_silt_sand_000_010cm.png"),
  width = 480 * 3, height = 480 * 2, res = 72 * 2
)
par(mar = c(5, 10, 2, 2), mfrow = c(1, num_plots))
xlim <- c(0, max(importance$importance))
for (i in 1:num_plots) {
  start_idx <- (i - 1) * vars_per_plot + 1
  end_idx <- min(i * vars_per_plot, num_vars)
  barplot(
    rev(importance$importance[start_idx:end_idx]),
    names.arg = rev(importance$variable[start_idx:end_idx]),
    las = 1,
    main = "",
    xlab = "Relative importance",
    ylab = "",
    col = "lightgray",
    border = "darkgray",
    cex.names = 0.7,
    cex.axis = 0.7,
    cex.lab = 0.7,
    horiz = TRUE,
    xlim = xlim
  )
}
mtext("Variable Importance: log_silt_sand Model (0-10cm)",
  side = 3, line = -2, outer = TRUE, cex = 1, font = 1
)
dev.off()

# Write error metrics to disk
write.csv(
  error_metrics,
  file = paste0(res_tab_path, "psd_error_metrics_000_010cm.csv"),
  row.names = FALSE
)

# Cross-validation #################################################################################
# Leave-group-out cross-validation. Set groups of samples based on the 'id' column. Each soil
# profile is a group, consisting of multiple samples at different depths.
# Check the number of unique groups
num_groups <- length(unique(soildata$id))
print(num_groups)
# 11623

# Add a column to store the cross-validation predictions
soildata[, pred_log_clay_sand := as.numeric(NA)]
soildata[, pred_log_silt_sand := as.numeric(NA)]

# Perform cross-validation: log_clay_sand
# This task takes about 16 hours to run on a machine with 3 cores and 16GB of RAM.
t0 <- Sys.time()
for (group in unique(soildata$id)) {
  # Print the current group
  print(group)
  # Check if the validation group has any samples with depth 0-10 cm
  if (nrow(soildata[id == group & depth <= 10]) == 0) {
    print("No samples with depth 0-10 cm in group")
  } else {
    print("Samples with depth 0-10 cm in group")
    # Fit the model on the training set
    set.seed(random_seed)
    model <- ranger::ranger(
      formula = log_clay_sand ~ .,
      data = soildata[id != group,
        !c("id", "log_silt_sand", "pred_log_clay_sand", "pred_log_silt_sand")],
      num.trees = 100,
      mtry = 16,
      min.node.size = 2,
      max.depth = 30,
      verbose = FALSE,
      num.threads = n_cores
    )
    # Predict on the validation group, for layers with depth 0-10 cm
    soildata[
      id == group & depth <= 10,
      pred_log_clay_sand :=
      predict(model, data = soildata[id == group & depth <= 10,
        !c("id", "log_silt_sand", "pred_log_clay_sand", "pred_log_silt_sand")])$predictions
    ]
  }
}
print(Sys.time() - t0)

# Save preditions to disk
data.table::fwrite(soildata[, .(id, depth, pred_log_clay_sand)],
  file = paste0(res_tab_path, "log_clay_sand_000_010cm_cross_validation.txt"), sep = "\t",
  row.names = FALSE
)

# Perform cross-validation: log_silt_sand
# This task takes about 16 hours to run on 3 cores and about 5.5 hours on 7 cores.
t0 <- Sys.time()
for (group in unique(soildata$id)) {
  # Print the current group
  print(group)
  # Check if the validation group has any samples with depth 0-10 cm
  if (nrow(soildata[id == group & depth <= 10]) == 0) {
    print("No samples with depth 0-10 cm in group")
  } else {
    print("Samples with depth 0-10 cm in group")
    # Fit the model on the training set
    set.seed(random_seed)
    model <- ranger::ranger(
      formula = log_silt_sand ~ .,
      data = soildata[id != group,
        !c("id", "log_clay_sand", "pred_log_clay_sand", "pred_log_silt_sand")],
      num.trees = 100,
      mtry = 16,
      min.node.size = 2,
      max.depth = 30,
      verbose = FALSE,
      num.threads = n_cores
    )
    # Predict on the validation group, for layers with depth 0-10 cm
    soildata[
      id == group & depth <= 10,
      pred_log_silt_sand :=
      predict(model, data = soildata[id == group & depth <= 10,
        !c("id", "log_clay_sand", "pred_log_clay_sand", "pred_log_silt_sand")])$predictions
    ]
  }
}
print(Sys.time() - t0)

# Save preditions to disk
data.table::fwrite(soildata[, .(id, depth, pred_log_silt_sand)],
  file = paste0(res_tab_path, "log_silt_sand_000_010cm_cross_validation.txt"), sep = "\t",
  row.names = FALSE
)
