# title: Modeling Soil Particle Size Distribution
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2025
rm(list = ls())

####################################################################################################
# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("ranger")) {
  install.packages("ranger")
}

# Source helper functions
source("src/00_helper_functions.r")

# Read data from disk
folder_path <- "~/Insync/Earth Engine Exports/"
file_name <- "psd_c02beta_00_10cm_v2.csv"
soildata_00_10cm <- data.table::fread(paste0(folder_path, file_name))
# Check the data
dim(soildata_00_10cm)
# Rows: 19944
# Columns: 79

# Drop unwanted covariates
colnames(soildata_00_10cm)
soildata_00_10cm <- soildata_00_10cm[
  ,
  c("id", "system:index", "depth_5", "latitude", "longitude", "ndvi", ".geo") := NULL
]
# Check the data again
dim(soildata_00_10cm)
# Rows: 19944
# Columns: 72

# log_clay_sand ####################################################################################
# Fit ranger model to predict log_clay_sand using all columns as predictors,
# except for log_silt_sand.
log_clay_sand_00_10cm_model <- ranger::ranger(
  formula = log_clay_sand ~ .,
  data = soildata_00_10cm[, !c("log_silt_sand")],
  num.trees = 100,
  mtry = 16,
  min.node.size = 2,
  max.depth = 30,
  replace = TRUE,
  verbose = TRUE,
  importance = "impurity",
  seed = 1984
)
# Print model summary
print(log_clay_sand_00_10cm_model)
# Compute error metrics
observed <- soildata_00_10cm$log_clay_sand
predicted <- log_clay_sand_00_10cm_model$predictions
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
  target = "log_clay_sand_00_10cm",
  ME = me,
  MAE = mae,
  MSE = mse,
  RMSE = rmse,
  MEC = mec,
  Slope = slope,
  row.names = NULL
)
print(error_metrics)

# Compute variable importance
importance <- log_clay_sand_00_10cm_model$variable.importance
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
if (num_plots > 0) {
  png("res/fig/psd_varimp_log_clay_sand_00_10cm.png", width = 480 * 3, height = 480 * 2, res = 72 * 2)
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
  mtext("Variable Importance: log_clay_sand Model (0-10cm)", side = 3, line = -2, outer = TRUE, cex = 1, font = 1)
  dev.off()
}

# log_silt_sand ######
# Fit ranger model to predict log_silt_sand using all columns as predictors,
# except for log_clay_sand.
log_silt_sand_00_10cm_model <- ranger::ranger(
  formula = log_silt_sand ~ .,
  data = soildata_00_10cm[, !c("log_clay_sand")],
  num.trees = 100,
  mtry = 16,
  min.node.size = 2,
  max.depth = 30,
  replace = TRUE,
  verbose = TRUE,
  importance = "impurity",
  seed = 1984
)
# Print model summary
print(log_silt_sand_00_10cm_model)
# Compute error metrics
observed <- soildata_00_10cm$log_silt_sand
predicted <- log_silt_sand_00_10cm_model$predictions
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
    target = "log_silt_sand_00_10cm",
    ME = me,
    MAE = mae,
    MSE = mse,
    RMSE = rmse,
    MEC = mec,
    Slope = slope,
    row.names = NULL
  )
)
# Print error metrics
print(error_metrics)

# Compute variable importance
importance <- log_silt_sand_00_10cm_model$variable.importance
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
if (num_plots > 0) {
  png("res/fig/psd_varimp_log_silt_sand_00_10cm.png", width = 480 * 3, height = 480 * 2, res = 72 * 2)
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
  mtext("Variable Importance: log_silt_sand Model (0-10cm)", side = 3, line = -2, outer = TRUE, cex = 1, font = 1)
  dev.off()
}

