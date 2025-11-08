# title: MapBiomas Soil
# subtitle: Helper functions
# author: Alessandro Samuel-Rosa
# data: 2025 CC-BY

# Install required packages
if (!requireNamespace("data.table")) {
  install.packages("data.table")
}
if (!requireNamespace("sf")) {
  install.packages("sf")
}

# R function for the simple imputation of missing values in the columns of a data.frame.
# The argument 'x' is a data.frame containing multiple columns. Columns of x can be of type continuous (numeric) or categorical (factor or character). Three simple imputation methods are available:
# 1. measure of central tendency (MCT): for continuous variables, the missing values are replaced by the median of the non-missing values, appending 'med' to the variable name; for categorical variables, the missing values are replaced by the mode of the non-missing values, appending 'mod' to the variable name;
# 2. missingness incorporated in attributes (MIA): continuous variables are duplicated and the missing values are replaced by +Inf and -Inf, appending 'pinf' and 'minf' to variable names, respectively; for categorical variables, the missing values are replaced by a new category called 'UNKNOWN', appending 'mia' to the variable name;
# 3. out-of-range value (OOR): for continuous variables, the missing values are replaced by a value outside the range of the variable (e.g., -99999), appending 'oor' to the variable name; for categorical variables, the missing values are replaced by a new category called 'UNKNOWN', appending 'oor' to the variable name.
# The argument 'method' is a character vector with the name of the method to be used.
# The argument 'indicator' is a logical vector indicating if a 0/1 indicator vector of missingness should be created or not for each column of x with missing values, with 0 = non-missing and 1 = missing. The default is to create the indicator.
imputation <- function(x, method = c("mct", "mia"), na.indicator = TRUE,
  na.replacement = list(cont = Inf, cat = "???")) {
    # Check arguments
    # Check if x can be coerced to a data.frame
    if (!is.data.frame(x)) {
      stop("x must be a data.frame")
    }
    x <- as.data.frame(x)
    # Identify the columns of x with any missing value.
    # Stop if there is any column without missing values.
    # Print a message indicating which columns do not have NAs.
    is_na <- apply(x, 2, function(x) any(is.na(x)))
    if (!any(is_na)) {
      stop(paste(
        "The following columns do not have missing values:",
        paste(names(x)[!is_na], collapse = ", ")
      ))
    }
    # Check if method is a character vector
    if (!is.character(method)) {
      stop("method must be a character vector")
    }
    # Check if method is a valid method
    if (!method %in% c("mct", "mia")) {
      stop(paste("Unknown method:", method))
    }
    # Check if na.indicator is a logical vector
    if (!is.logical(na.indicator)) {
      stop("na.indicator must be a logical vector")
    }
    # Check if na.replacement is a list
    if (!is.list(na.replacement)) {
      stop("na.replacement must be a list")
    }
    # Check if na.replacement has two elements
    if (length(na.replacement) != 2) {
      stop("na.replacement must have two elements")
    }
    # Check if na.replacement has the elements 'cont' and 'cat'
    if (!all(c("cont", "cat") %in% names(na.replacement))) {
      stop("na.replacement must have the elements 'cont' and 'cat'")
    }
    # Check if na.replacement$cont is numeric
    if (!is.numeric(na.replacement[["cont"]])) {
      stop("na.replacement$cont must be numeric")
    }
    # Check if na.replacement$cat is character and is not empty
    if (!is.character(na.replacement[["cat"]]) |
      nchar(na.replacement[["cat"]]) == 0) {
      stop("na.replacement$cat must be character and not empty")
    }
    # Data processing
    # Separate columns with missing values
    x0 <- x[, !is_na]
    x <- x[, is_na]
    # Missingness indicator
    #  If required, create a 0/1 indicator of missingness for each column of x with missing
    if (na.indicator) {
      y <- is.na(x)
      y <- as.data.frame(y)
      names(y) <- paste(names(y), "isna", sep = "_")
      # Set indicators as factors
      y <- lapply(y, as.factor)
    }
    # Categorical variables
    # Identify the numeric variables
    num <- sapply(x, is.numeric)
    # Check if there is any categorical variable
    if (any(!num)) {
      if (method == "mia") {
        # Replace missing values by na.replacement
        x <- lapply(x, function(x) {
          if (!is.numeric(x)) {
            x[is.na(x)] <- na.replacement[["cat"]]
          }
          return(x)
        })
        x <- as.data.frame(x)
        # Rename columns of categorical variables
        names(x)[!num] <- paste(names(x)[!num], method, sep = "_")
      } else {
        # Replace missing values by the mode (most common value)
        x <- lapply(x, function(x) {
          if (!is.numeric(x)) {
            x[is.na(x)] <- names(which.max(table(x)))
          }
          return(x)
        })
        x <- as.data.frame(x)
        # Rename columns of categorical variables by pasting the suffix 'mod'
        names(x)[!num] <- paste(names(x)[!num], "mod", sep = "_")
      }
      # Set categorical variables as factors
      x[!num] <- lapply(x[!num], as.factor)
    }
    # Continuous variables
    # Check if there is any numeric variable
    if (any(num)) {
      if (method == "mia") {
        # Replace missing values by a value outside the range of the variable
        x2 <- as.data.frame(x[num])
        x <- lapply(x, function(x) {
          if (is.numeric(x)) {
            x[is.na(x)] <- na.replacement[["cont"]] * -1
          }
          return(x)
        })
        x <- as.data.frame(x)
        x2 <- lapply(x2, function(x) {
          if (is.numeric(x)) {
            x[is.na(x)] <- na.replacement[["cont"]]
          }
          return(x)
        })
        x2 <- as.data.frame(x2)
        # Rename columns of numeric variables by pasting the suffix 'pinf' and 'minf'
        names(x2) <- paste(names(x)[num], "pinf", sep = "_")
        names(x)[num] <- paste(names(x)[num], "minf", sep = "_")
        # Bind columns of x and x2
        x <- cbind(x, x2)
      } else {
        # Replace missing values by the median of the non-missing values
        x <- lapply(x, function(x) {
          if (is.numeric(x)) {
            x[is.na(x)] <- median(x, na.rm = TRUE)
          }
          return(x)
        })
        x <- as.data.frame(x)
        # Rename columns of numeric variables by pasting the suffix 'med'
        names(x)[num] <- paste(names(x)[num], "med", sep = "_")
      }
    }
    # Output
    # Return output as data.frame
    x <- as.data.frame(x)
    if (na.indicator) {
      x <- cbind(x, y)
    }
    # Merge x and x0 if x0 has columns
    if (ncol(x0) > 0) {
      x <- cbind(x0, x)
    }
    # Arrange columns by alphabetical order
    x <- x[, order(colnames(x))]
    return(x)
  }
# Skewness
# Create function to compute the skewness of a distribution. Use an argument na.rm = TRUE.
skewness <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- na.omit(x)
  }
  n <- length(x)
  # skew <- sum((x - mean(x))^3) / (n * sd(x)^3)
  skew <- (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3 / 2) # moments::skewness
  return(skew)
}
# Compute regression model statistics
error_statistics <-
  function(observed, predicted) {
    error <- predicted - observed
    residual <- mean(observed) - observed
    me <- mean(error)
    mae <- mean(abs(error))
    mse <- mean(error^2)
    rmse <- sqrt(mse)
    mec <- 1 - mse / mean(residual^2)
    slope <- coef(lm(observed ~ predicted))[2]
    return(data.frame(me, mae, mse, rmse, mec, slope))
  }
# Describe soil data ###############################################################################
# Create function to describe a data.frame. Use an argument na.rm = TRUE.
summary_soildata <- function(x, na.rm = TRUE) {
  # If 'id' is missing, generate temporary 'id' column by concatenating 'dataset_id' and 'observacao_id'
  if (!"id" %in% names(x) & all(c("dataset_id", "observacao_id") %in% names(x))) {
    x[, id := paste0(dataset_id, "_", observacao_id)]
    temp_id <- TRUE
  } else {
    temp_id <- FALSE
  }
  cat("Column names:")
  cat("\n", paste(sort(names(x))), collapse = " ")
  cat("\nLayers:", nrow(x))
  cat("\nEvents:", nrow(unique(x[, "id"])))
  cat("\nGeoreferenced events:", nrow(unique(x[!is.na(coord_x) & !is.na(coord_y), "id"])))
  if ("dataset_id" %in% names(x)) {
    cat("\nDatasets:", length(unique(x[, dataset_id])))
  }
  cat("\n")
  if (temp_id) {
    x[, id := NULL] # Remove temporary 'id' column
  }
}

# Browse Google Maps:
# https://www.google.com.br/maps/place/-9.765833,-65.73528
# x is a data frame
google_maps <- function(x, coords = c("coord_x", "coord_y")) {
  x <- x[, ..coords]
  x <- as.data.frame(x)
  x <- setNames(x, c("lon", "lat"))
  x <- paste0("https://www.google.com.br/maps/place/", x$lat, ",", x$lon)
  browseURL(x)
}
# Read Google Sheet
google_sheet <- function(gs, gid) {
  sheet_path <- paste0(
    "https://docs.google.com/spreadsheets/u/1/d/",
    gs,
    "/export?format=tsv&id=",
    gs,
    "&gid=",
    gid
  )
  dt <- data.table::fread(sheet_path, dec = ",", sep = "\t")
  return(dt)
}
# Read Insync file #################################################################################
# This function reads a file from the Insync folder, which is a local sync of Google Drive.
# The Insync folder is located at
# "~/Insync/alessandrosamuelrosa@gmail.com/Google Drive/Earth Engine Exports/"
# The file_name argument is the name of the file to be read.
# The function uses data.table::fread to read the file and returns a data.table object
# Note: Make sure to set the correct path to your Insync folder.
# The Insync folder is a local sync of Google Drive, so the file_name should be
# the name of the file in your Google Drive that you want to read.
# Example: read_insync("my_file.csv") 
read_insync <- function(file_name, ...) {
  insync_path <- "~/Insync/alessandrosamuelrosa@gmail.com/Google Drive/Earth Engine Exports/"
  file_path <- file.path(insync_path, file_name)
  data.table::fread(file_path, ...)
}

# Compute the clay content at 5, 15, and 25 cm depth
# psd_data <- psd_data[
#   ,
#   .(
#     coord_x = mean(coord_x, na.rm = TRUE),
#     coord_y = mean(coord_y, na.rm = TRUE),
#     clay0_10 = round(spline(x = depth_mid, y = argila, xout = 5, method = "natural")$y),
#     silt0_10 = round(spline(x = depth_mid, y = silte, xout = 5, method = "natural")$y),
#     sand0_10 = round(spline(x = depth_mid, y = areia, xout = 5, method = "natural")$y),
#     clay10_20 = round(spline(x = depth_mid, y = argila, xout = 15, method = "natural")$y),
#     silt10_20 = round(spline(x = depth_mid, y = silte, xout = 15, method = "natural")$y),
#     sand10_20 = round(spline(x = depth_mid, y = areia, xout = 15, method = "natural")$y),
#     clay20_30 = round(spline(x = depth_mid, y = argila, xout = 25, method = "natural")$y),
#     silt20_30 = round(spline(x = depth_mid, y = silte, xout = 25, method = "natural")$y),
#     sand20_30 = round(spline(x = depth_mid, y = areia, xout = 25, method = "natural")$y)
#   ),
#   by = id
# ]


# Check the sum of the particle size distribution
# psd_data[, psd0_10 := clay0_10 + silt0_10 + sand0_10]
# psd_data[, psd10_20 := clay10_20 + silt10_20 + sand10_20]
# psd_data[, psd20_30 := clay20_30 + silt20_30 + sand20_30]
# summary(psd_data[, .(psd0_10, psd10_20, psd20_30)])

# Correct the particle size distribution to sum 100%
# 0-10 cm depth
# psd_data[, clay0_10 := round(clay0_10 / psd0_10 * 100)]
# psd_data[, sand0_10 := round(sand0_10 / psd0_10 * 100)]
# psd_data[, silt0_10 := 100 - clay0_10 - sand0_10]

# 10-20 cm depth
# psd_data[, clay10_20 := round(clay10_20 / psd10_20 * 100)]
# psd_data[, sand10_20 := round(sand10_20 / psd10_20 * 100)]
# psd_data[, silt10_20 := 100 - clay10_20 - sand10_20]

# 20-30 cm depth
# psd_data[, clay20_30 := round(clay20_30 / psd20_30 * 100)]
# psd_data[, sand20_30 := round(sand20_30 / psd20_30 * 100)]
# psd_data[, silt20_30 := 100 - clay20_30 - sand20_30]

# summary(psd_data)

# Plot using mapview
# if (FALSE) {
#   psd_data_sf <- sf::st_as_sf(psd_data, coords = c("coord_x", "coord_y"), crs = 4326)
#   mapview::mapview(psd_data_sf, zcol = "clay0_10")
# }
# Check for missing layers #########################################################################
# This function checks for missing layers in a data.table containing soil layer data. It identifies
# gaps in the depth intervals of soil layers for each observation ID. Gaps are defined as instances
# where the upper depth of one layer does not match the lower depth of the next layer. They occur
# when soil sampling in the field does not cover the entire depth range, leading to missing layers
# in the dataset. The function returns a data.table containing the missing layers, including the
# observation ID, layer name, and depth intervals.
# layer_data: data.table containing soil layer data
# Returns: data.table with missing layers, including 'id', 'camada_nome',
#          'profund_sup', and 'profund_inf'
# Example usage: check_missing_layer(layer_data)
# Note: The function assumes that the data.table package is loaded and that the data has the necessary
# columns for observation ID, layer name, and depth intervals. If missing layers are found, a message
# is printed to the console.
check_missing_layer <- function(layer_data) {
  # Start by sorting the data.table by id, profund_sup, and profund_inf
  layer_data <- layer_data[order(id, profund_sup, profund_inf)]
  # Check for each id if there is a missing layer
  missing_layers <- layer_data[
    data.table::shift(profund_inf) != profund_sup & profund_sup > 0,
    .(id, camada_nome, profund_sup, profund_inf)
  ]
  if (nrow(missing_layers) > 0) {
    message(
      "Missing layers were found. ",
      "Check the source dataset if this is correct or if there has been an error ",
      "when recording the layer depth limits."
    )
    # Return the missing layers
    return(missing_layers)
  } else {
    message("No missing layers were found. You can proceed.")
  }
}
# Add missing layers ###############################################################################
# This function adds missing layers to a data.table containing soil layer data.
# It checks for each event ID if the top layer is missing (i.e., if the minimum depth of the top
# layer is greater than 0). If the top layer is missing, a new row is added to the data.table with
# depth_top set to 0 and depth_bottom set to the minimum depth of the top layer. The function also
# identifies gaps in the depth intervals of soil layers for each event ID and adds rows for these
# missing layers. The final result is a data.table with all layers, including the added missing
# layers, ordered by event ID and depth_top.
# x: data.table containing soil layer data with columns for event ID, depth top, depth bottom, and layer ID.
# event.id: column name for event ID (default is "id")
# depth.top: column name for depth top (default is "profund_sup")
# depth.bottom: column name for depth bottom (default is "profund_inf")
# layer.id: column name for layer ID (default is "camada_id")
# Returns: data.table with all layers, including added missing layers, ordered by event ID and depth_top.
# Example usage: add_missing_layer(layer_data)
# Note: The function assumes that the data.table package is loaded and that the data has the
# necessary columns for event ID, depth top, depth bottom, and layer ID.
add_missing_layer <- function(
    x, event.id = "id", depth.top = "profund_sup", depth.bottom = "profund_inf",
    layer.id = "camada_id", layer.name = "camada_nome") {
  # Ensure x is a data.table
  data.table::setDT(x)

  # Rename columns
  old_names <- c(event.id, depth.top, depth.bottom, layer.id, layer.name)
  new_names <- c("event_id", "depth_top", "depth_bottom", "layer_id", "layer_name")
  data.table::setnames(x, old = old_names, new = new_names)

  # Check for each event_id if it is missing the top layer, i.e. min(depth_top) > 0
  x[, missing_top := min(depth_top) > 0, by = event_id]

  # If the top layer is missing
  if (any(x$missing_top)) {
    message("Missing top layer found. Adding a new row with depth_top = 0.")
    # Add a row to the data.table. Then, for the new row, set
    # depth_top = 0 and depth_bottom = min(depth_top)
    x <- rbind(x, x[missing_top == TRUE, .(
      event_id = event_id,
      depth_top = 0,
      depth_bottom = min(depth_top)
    )], fill = TRUE)
  } else {
    message("No missing top layer found.")
  }
  x[, missing_top := NULL]

  # Order x by profile_id and depth_top
  data.table::setorder(x, event_id, depth_top)

  # Create a new data.table to store the missing layers
  missing_layers <- x[, .(
    depth_top = depth_bottom[-.N],
    depth_bottom = data.table::shift(depth_top, type = "lead")[-.N]
  ), by = event_id][depth_top != depth_bottom]

  # Combine the original data with the missing layers
  result <- rbind(x, missing_layers, fill = TRUE)

  # Set layer_name using the depth limits (top-bottom)
  result[is.na(layer_name), layer_name := paste0(depth_top, "-", depth_bottom)]

  # Order the result by event.id and depth_top
  data.table::setorder(result, event_id, depth_top)

  # Reset layer_id according to the new order
  result[, layer_id := seq_len(.N), by = event_id]

  # Rename columns
  data.table::setnames(result, old = new_names, new = old_names)

  # Return the result
  return(result)
}
# Unified gap-filling function #####################################################################
# fill_empty_layer()
# Purpose:
#   Fill missing values (NA) in short 1‑D soil property profiles using one of two strategies:
#   1. Propagation (default when x is NULL): forward-fill (last observation carried forward),
#      optionally filling leading and/or trailing NA runs.
#   2. Conservative natural spline interpolation (when x is supplied): interpolate multiple
#      isolated internal NA values only when safety conditions are satisfied.
#
# Method selection:
#   - If 'x' is NULL or missing  -> propagation mode.
#   - If 'x' is provided (same length as y, numeric, no NA) -> spline mode.
#
# Arguments:
#   y : numeric vector (may contain NA). Returned length always equals length(y).
#   x : (optional) numeric vector of same length as y (e.g., depth midpoints). Triggers spline mode.
#   ylim : optional length-2 numeric vector c(min,max) to clamp spline output (ignored in propagation).
#   propagate.leading  : logical; fill leading NA run with first non-NA (propagation mode only).
#   propagate.trailing : logical; fill trailing NA run with last non-NA (propagation mode only).
#
# Propagation mode details:
#   - Each internal NA is replaced by the previous observed value (forward fill).
#   - Leading NAs filled only if propagate.leading = TRUE; otherwise kept as NA.
#   - Trailing NAs filled only if propagate.trailing = TRUE; otherwise kept as NA.
#   - Does NOT average or look ahead; strictly carries previous value forward.
#
# Spline mode safety checks (must all pass; any failure -> original y returned):
#   - At least one NA present.
#   - Non-NA count ≥ NA count.
#   - First element not NA (avoids extrapolation at top).
#   - Not length 2 with a single NA (unstable fit).
#   - Not pattern length 3 where last element is NA and only two non-NAs (avoids extrapolation).
#   - No consecutive NA runs (only isolated NAs allowed; e.g., [2, NA, 5, NA, 8] is OK,
#     but [2, NA, NA, 5] is rejected).
#
# Returns:
#   Numeric vector with gaps filled (propagation or spline) or original y if conditions fail.
#
# Messages:
#   Concise messages indicating chosen mode and whether filling was applied or skipped.
#
# Examples:
#   # --- Propagation mode (x omitted) ---
#   # Forward fill internal gaps; keep leading/trailing NA by default
#   fill_empty_layer(c(2, NA, NA, 5))
#   # [1] 2 2 2 5
#
#   fill_empty_layer(c(NA, 3, NA, 4))
#   # [1] NA  3  3  4  (leading NA not filled by default)
#
#   fill_empty_layer(c(NA, 3, NA, 4), propagate.leading = TRUE)
#   # [1] 3 3 3 4
#
#   fill_empty_layer(c(2, NA, 5, NA), propagate.trailing = FALSE)
#   # [1]  2  2  5 NA  (trailing NA kept)
#
#   # --- Spline mode (x provided) ---
#   # Interpolate isolated NAs using depth as x-axis
#   fill_empty_layer(y = c(14.5, NA, 5.3), x = c(7.5, 22.5, 35))
#   # Interpolates the middle value using natural spline
#
#   # Multiple isolated NAs allowed if non-NA ≥ NA
#   fill_empty_layer(y = c(14.5, NA, 5.3, NA, 3.8), x = c(7.5, 22.5, 35, 45, 52.5))
#   # Interpolates both NAs
#
#   # Consecutive NAs rejected (returns original)
#   fill_empty_layer(y = c(2, NA, NA, 5), x = c(5, 15, 25, 35))
#   # "Spline conditions not met. Returning original vector."
#
#   # Clamp output to valid range (e.g., clay content 0–100%)
#   fill_empty_layer(y = c(45, NA, 15), x = c(5, 15, 25), ylim = c(0, 100))
fill_empty_layer <- function(
  y,
  x = NULL,
  ylim,
  propagate.leading = FALSE,
  propagate.trailing = TRUE
) {
  if (!is.numeric(y)) stop("y must be numeric")

  use_spline <- !is.null(x)

  if (!use_spline) {
    # PROPAGATION MODE
    if (length(y) == 0L || all(is.na(y))) {
      message("Propagation: empty or all NA. Returning original.")
      return(y)
    }
    good <- which(!is.na(y))
    if (length(good) == 0L) {
      message("Propagation: no non-NA values. Returning original.")
      return(y)
    }
    if (propagate.leading && good[1] > 1) {
      y[1:(good[1] - 1)] <- y[good[1]]
    }
    for (i in seq_along(good)) {
      from <- good[i]
      to <- if (i < length(good)) good[i + 1] - 1L else length(y)
      if (to >= from + 1L) y[(from + 1L):to] <- y[from]
    }
    if (!propagate.trailing) {
      last_good <- good[length(good)]
      if (last_good < length(y)) y[(last_good + 1L):length(y)] <- NA_real_
    }
    message("Propagation applied.")
    return(y)
  }

  # SPLINE MODE
  if (!is.numeric(x)) stop("x must be numeric when provided")
  if (length(x) != length(y)) stop("x and y must have same length")
  if (any(is.na(x))) stop("x must not contain NA")

  no_interp <- "Spline conditions not met. Returning original vector."

  if (all(!is.na(y))) { message("No NA values. Returning original."); return(y) }
  if (length(y) == 1L) { message(no_interp); return(y) }
  if (sum(!is.na(y)) < sum(is.na(y))) { message(no_interp); return(y) }
  if (length(y) == 2L && sum(is.na(y)) == 1L) { message(no_interp); return(y) }
  if (is.na(y[1])) { message(no_interp); return(y) }
  if (sum(!is.na(y)) == 0L) { message(no_interp); return(y) }
  if (length(y) == 3L && sum(!is.na(y)) == 2L && is.na(y[3])) { message(no_interp); return(y) }
  # Allow multiple isolated NAs; reject any consecutive NA runs
  na_runs <- rle(is.na(y))
  if (any(na_runs$values & na_runs$lengths > 1L)) { message(no_interp); return(y) }

  message("Spline conditions met. Interpolating.")
  # Fit on observed pairs; evaluate on full x
  x_clean <- x[!is.na(y)]
  y_clean <- y[!is.na(y)]
  out <- spline(x = x_clean, y = y_clean, xout = x, method = "natural")$y
  if (!missing(ylim)) {
    out[out < ylim[1]] <- ylim[1]
    out[out > ylim[2]] <- ylim[2]
  }
  return(out)
}