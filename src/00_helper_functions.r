# title: SoilData - Soil Organic Carbon Stock
# subtitle: Helper functions
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY

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
  cat("\nDatasets:", length(unique(x[, dataset_id])))
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
