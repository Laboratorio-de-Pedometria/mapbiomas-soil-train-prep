# title: Modeling Soil Organic Carbon Stock - Model 2 (Cerrado, Caatinga, Atlantic Forest, Pantanal)
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

# Source helper functions
source("src/00_helper_functions.r")

# Set variables
res_fig_path <- "res/fig/"
res_tab_path <- "res/tab/"
random_seed <- 1984

# Read data from disk
matrix_path <- "~/Insync/Earth Engine Exports/matriz-collection2_MODEL2_v2.csv"
soildata <- data.table::fread(matrix_path)
# Check the data
dim(soildata)
# 8936  104

# Process the data #################################################################################
sort(colnames(soildata))

# Check which columns are constant
constant_columns <- sapply(soildata, function(x) length(unique(x)) == 1)
# Print the names of the constant columns
constant_colnames <- names(soildata)[constant_columns]
print(paste0("Constant columns: ", paste(constant_colnames, collapse = ", ")))
# Remove constant columns
soildata <- soildata[, !constant_columns, with = FALSE]
# Check the data again
dim(soildata)
# 8936   94

# Check data #######################################################################################
# Compare to the original training data 
original_path <- "data/40_soildata_soc.txt"
original <- data.table::fread(original_path)
# If the number of rows is different, check which rows are missing in the matrix. Then create a
# spatial data and plot using mapview.
print(paste0("Number of rows in original: ", nrow(original)))
print(paste0("Number of rows in soildata: ", nrow(soildata)))
# Check which rows are missing in the matrix
missing_rows <- setdiff(original$id, soildata$dataset_id)
# Get the missing rows, create spatial data and plot using mapview
missing_original <- original[id %in% missing_rows]
# Create spatial data
missing_original_sf <- sf::st_as_sf(missing_original, coords = c("coord_x", "coord_y"), crs = 4326)
# Plot using mapview
mapview::mapview(missing_original_sf, layer.name = "Missing samples")
# Print the number of missing rows
print(paste0("Number of missing rows: ", length(missing_rows)))
# Print the missing rows
print(missing_rows)
rm(missing_rows, missing_original, missing_original_sf)

# Process data #####################################################################################
# Drop unwanted columns
soildata <- soildata[, c("system:index", ".geo") := NULL]
# Check the data again
dim(soildata)
# 8936   92

# Select model hyperparameters #####################################################################

# Prepare grid of hyperparameters
# num.trees, mtry, min.node.size and max.depth
num_trees <- c(100, 200, 400, 800)
mtry <- c(2, 4, 8, 16)
min_node_size <- c(1, 2, 4, 8)
max_depth <- c(10, 20, 30, 40)
hyperparameters <- expand.grid(num_trees, mtry, min_node_size, max_depth)
colnames(hyperparameters) <- c("num_trees", "mtry", "min_node_size", "max_depth")

# Fit ranger model testing different hyperparameters
# (this takes about 15 minutes to run)
t0 <- Sys.time()
hyper_results <- data.table()
for (i in 1:nrow(hyperparameters)) {
  print(hyperparameters[i, ])
  set.seed(1984)
  model <- ranger::ranger(
    formula = estoque ~ .,
    data = soildata[, !c("dataset_id", "year")],
    num.trees = hyperparameters$num_trees[i],
    mtry = hyperparameters$mtry[i],
    min.node.size = hyperparameters$min_node_size[i],
    max.depth = hyperparameters$max_depth[i],
    replace = TRUE,
    verbose = TRUE
  )
  observed <- soildata[, estoque]
  predicted <- model$predictions
  error <- observed - predicted
  residual <- mean(observed) - observed
  me <- mean(error)
  mae <- mean(abs(error))
  mse <- mean(error^2)
  rmse <- sqrt(mse)
  nse <- 1 - mse / mean(residual^2)
  slope <- coef(lm(observed ~ predicted))[2]
  hyper_results <- rbind(hyper_results, data.table(
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
file_path <- "res/tab/soc_model02_hyperparameter_tunning.txt"
data.table::fwrite(hyper_results, file_path, sep = "\t")
if (FALSE) {
  # Read the results from disk
  hyper_results <- data.table::fread(file_path, sep = "\t")
}

# Assess results
# What is the Spearman correlation between hyperparameters and model performance metrics?
correlation <- round(cor(hyper_results, method = "spearman"), 2)
data.table::fwrite(correlation, "res/tab/soc_model02_hyperparameter_correlation.txt", sep = "\t")
print(correlation[1:4, 5:9])

# Sort the results by RMSE
# Keep only those with ntree < 800
hyper_results <- hyper_results[order(rmse)]
hyper_results <- hyper_results[num_trees < 800]

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
hyper_best <- data.frame(num_trees = 400, mtry = 16, min_node_size = 1, max_depth = 40)

# Fit the best model
t0 <- Sys.time()
set.seed(2001)
soc_model <- ranger::ranger(
  formula = estoque ~ .,
  data = soildata[, !c("dataset_id", "year")],
  # Use the best hyperparameters
  num.trees = hyper_best$num_trees,
  mtry = hyper_best$mtry,
  min.node.size = hyper_best$min_node_size,
  max.depth = hyper_best$max_depth,
  importance = "impurity",
  replace = TRUE,
  verbose = TRUE
)
Sys.time() - t0
print(soc_model)

# Compute regression model statistics and write to disk
soc_model_stats <- error_statistics(soildata[, estoque], soc_model$predictions)
data.table::fwrite(soc_model_stats, "res/tab/soc_model02_model_statistics.txt", sep = "\t")
print(round(soc_model_stats, 2))

# Write model parameters to disk
write.table(capture.output(print(soc_model))[6:15],
  file = "res/tab/soc_model02_model_parameters.txt", sep = "\t", row.names = FALSE
)
if (FALSE) {
  # Read the model parameters from disk
  soc_model <- data.table::fread("res/tab/soc_model02_model_parameters.txt", sep = "\t")
  print(soc_model)
}

# Check absolute error
abs_error_tolerance <- soc_model_stats$rmse * 2
soildata[, predicted := round(soc_model$predictions)]
soildata[, abs_error := abs(estoque - predicted)]
if (any(soildata[, abs_error] >= abs_error_tolerance)) {
  View(soildata[
    abs_error >= abs_error_tolerance,
    .(dataset_id, year, estoque, predicted, abs_error, IFN_index)
  ])
} else {
  print(paste0("All absolute errors are below ", abs_error_tolerance, " g/m^2."))
}

# Figure: Variable importance
importance <- sort(soc_model$variable.importance, decreasing = TRUE)
importance <- round(importance / max(importance) * 100) # Normalize to 1
num_vars <- length(importance)
vars_per_plot <- ceiling(num_vars/2)
num_plots <- ceiling(num_vars / vars_per_plot)
dev.off()
png("res/fig/soc_model02_variable_importance.png", width = 480 * 3, height = 480 * 3, res = 72 * 2)
par(mar = c(5, 10, 2, 2), mfrow = c(1, num_plots))
xlim <- c(0, max(importance))
for (i in 1:num_plots) {
  start_idx <- (i - 1) * vars_per_plot + 1
  end_idx <- min(i * vars_per_plot, num_vars)
  barplot(
    rev(importance[start_idx:end_idx]),
    names.arg = rev(names(importance)[start_idx:end_idx]),
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
mtext("Variable Importance: SOC stock Model",
  side = 3, line = -2, outer = TRUE, cex = 1, font = 1
)
dev.off()

# Figure: Plot fitted versus observed values
# Set color of points as a function of the absolute error, that is, abs(y - x). The absolute error
# ranges from 0 to 1.
color_breaks <- seq(0, ceiling(abs_error_tolerance / 1000), length.out = 7)
color_class <- cut(soildata[, abs_error / 1000], breaks = color_breaks, include.lowest = TRUE)
color_palette <- RColorBrewer::brewer.pal(length(color_breaks) - 1, "Purples")
xylim <- range(soildata[, estoque / 1000])
dev.off()
png("res/fig/soc_model02_observed_versus_oob.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(mar = c(4, 4.5, 2, 2) + 0.1)
plot(
  y = soildata[, estoque / 1000], x = soc_model$predictions / 1000,
  xlim = xylim, ylim = xylim,
  panel.first = grid(),
  pch = 21, bg = color_palette[as.numeric(color_class)],
  ylab = expression("Observed soil organic carbon stock, kg m"^-2),
  xlab = expression("Fitted soil organic carbon stock (OOB), kg m"^-2)
)
abline(0, 1)
legend("topleft", title = expression("Absolute error, kg m"^-2),
  legend = levels(color_class),
  pt.bg = color_palette, border = "white", box.lwd = 0, pch = 21
)
dev.off()
