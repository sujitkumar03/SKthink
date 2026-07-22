# ============================================================
# SKstats - Statistical Functions
# ============================================================

#' Check and Report Missing Values
#'
#' Identifies and reports missing values in a dataset with detailed summary.
#'
#' @param data A data frame or matrix
#' @param report Logical; if TRUE, prints detailed report
#' @return A data frame with missing value summary
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(x = c(1, 2, NA, 4), y = c(NA, 2, 3, NA))
#' sk_missing(data)
#' }
sk_missing <- function(data, report = TRUE) {

  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Input must be a data frame or matrix")
  }

  missing_count <- colSums(is.na(data))
  missing_percent <- round(colSums(is.na(data)) / nrow(data) * 100, 2)

  result <- data.frame(
    Variable = names(missing_count),
    Missing_Count = missing_count,
    Missing_Percent = missing_percent,
    Total = nrow(data),
    Complete = nrow(data) - missing_count
  )

  rownames(result) <- NULL

  if (report) {
    cat("\n========== MISSING VALUE REPORT ==========\n")
    cat("Total rows:", nrow(data), "\n")
    cat("Total columns:", ncol(data), "\n")
    cat("Total missing values:", sum(missing_count), "\n")
    cat("Overall missing percentage:",
        round(sum(missing_count) / (nrow(data) * ncol(data)) * 100, 2), "%\n")
    cat("\n")
    print(result)

    if (sum(missing_count) > 0) {
      cat("\nRows with missing values:", sum(complete.cases(data) == FALSE), "\n")
      cat("Complete cases:", sum(complete.cases(data)), "\n")
    }
    cat("==========================================\n")
  }

  invisible(result)
}

#' Remove Rows with Missing Values
#'
#' Removes rows containing missing values from a dataset.
#'
#' @param data A data frame or matrix
#' @param cols Optional vector of column names to check for missing values.
#'        If NULL, checks all columns.
#' @param verbose Logical; if TRUE, prints summary of removed rows
#' @return A data frame with rows containing missing values removed
#' @export
sk_missing_remove <- function(data, cols = NULL, verbose = TRUE) {

  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Input must be a data frame or matrix")
  }

  original_rows <- nrow(data)

  if (!is.null(cols)) {
    if (!all(cols %in% colnames(data))) {
      stop("Some columns not found in data")
    }
    rows_with_na <- apply(data[, cols, drop = FALSE], 1, function(x) any(is.na(x)))
  } else {
    rows_with_na <- !complete.cases(data)
  }

  clean_data <- data[!rows_with_na, , drop = FALSE]

  if (verbose) {
    removed <- sum(rows_with_na)
    cat("\n========== MISSING VALUE REMOVAL ==========\n")
    cat("Original rows:", original_rows, "\n")
    cat("Rows removed:", removed, "\n")
    cat("Remaining rows:", nrow(clean_data), "\n")
    if (removed > 0) {
      cat("Removed", round(removed/original_rows * 100, 2), "% of data\n")
    }
    cat("===========================================\n")
  }

  return(clean_data)
}

#' Impute Missing Values
#'
#' Imputes missing values using various methods.
#'
#' @param data A data frame
#' @param method Imputation method: "mean", "median", "mode"
#' @param cols Optional vector of column names to impute
#' @param verbose Logical; if TRUE, prints summary of imputation
#' @return A data frame with imputed values
#' @export
sk_impute <- function(data, method = "mean", cols = NULL, verbose = TRUE) {

  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  if (is.null(cols)) {
    cols <- names(data)[colSums(is.na(data)) > 0]
  }

  if (length(cols) == 0) {
    cat("No missing values found in specified columns\n")
    return(data)
  }

  imputed_data <- data
  imputed_count <- 0

  for (col in cols) {
    if (!col %in% names(data)) {
      warning(paste("Column", col, "not found. Skipping."))
      next
    }

    missing_idx <- is.na(data[[col]])

    if (sum(missing_idx) == 0) next

    if (is.numeric(data[[col]])) {
      if (method == "mean") {
        value <- mean(data[[col]], na.rm = TRUE)
        imputed_data[[col]][missing_idx] <- value
        imputed_count <- imputed_count + sum(missing_idx)
      } else if (method == "median") {
        value <- median(data[[col]], na.rm = TRUE)
        imputed_data[[col]][missing_idx] <- value
        imputed_count <- imputed_count + sum(missing_idx)
      } else if (method == "mode") {
        value <- as.numeric(names(sort(table(data[[col]]), decreasing = TRUE))[1])
        imputed_data[[col]][missing_idx] <- value
        imputed_count <- imputed_count + sum(missing_idx)
      }
    } else {
      value <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
      imputed_data[[col]][missing_idx] <- value
      imputed_count <- imputed_count + sum(missing_idx)
    }
  }

  if (verbose) {
    cat("\n========== IMPUTATION SUMMARY ==========\n")
    cat("Method:", method, "\n")
    cat("Columns imputed:", length(cols), "\n")
    cat("Total values imputed:", imputed_count, "\n")
    cat("=========================================\n")
  }

  return(imputed_data)
}

#' Find and Remove Duplicate Rows
#'
#' Identifies and removes duplicate rows from a dataset.
#'
#' @param data A data frame
#' @param cols Optional vector of column names to check for duplicates.
#'        If NULL, checks all columns.
#' @param keep First or last occurrence: "first", "last"
#' @param verbose Logical; if TRUE, prints summary of duplicates
#' @return A data frame with duplicate rows removed
#' @export
sk_duplicate_remove <- function(data, cols = NULL, keep = "first", verbose = TRUE) {

  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  original_rows <- nrow(data)

  if (!is.null(cols)) {
    if (!all(cols %in% colnames(data))) {
      stop("Some columns not found in data")
    }
    unique_data <- data[!duplicated(data[, cols, drop = FALSE]), , drop = FALSE]
  } else {
    unique_data <- unique(data)
  }

  if (verbose) {
    duplicates <- original_rows - nrow(unique_data)
    cat("\n========== DUPLICATE REMOVAL ==========\n")
    cat("Original rows:", original_rows, "\n")
    cat("Duplicate rows:", duplicates, "\n")
    cat("Remaining rows:", nrow(unique_data), "\n")
    if (duplicates > 0) {
      cat("Removed", round(duplicates/original_rows * 100, 2), "% of data\n")
    }
    cat("========================================\n")
  }

  return(unique_data)
}

#' Comprehensive Descriptive Statistics
#'
#' Calculates comprehensive descriptive statistics for numeric variables.
#'
#' @param data A data frame or numeric vector
#' @param digits Number of decimal places (default: 2)
#' @param transposed Logical; if TRUE, transposes the output
#' @param verbose Logical; if TRUE, prints the output (default: TRUE)
#' @return A data frame with descriptive statistics
#' @export
sk_stat <- function(data, digits = 2, transposed = FALSE, verbose = TRUE) {

  if (is.vector(data) && is.numeric(data)) {
    data <- data.frame(Value = data)
  }

  if (!is.data.frame(data)) {
    stop("Input must be a data frame or numeric vector")
  }

  numeric_cols <- sapply(data, is.numeric)

  if (sum(numeric_cols) == 0) {
    stop("No numeric columns found in data")
  }

  numeric_data <- data[, numeric_cols, drop = FALSE]

  stats_list <- list()

  for (col in names(numeric_data)) {
    x <- numeric_data[[col]]
    stats_list[[col]] <- c(
      N = sum(!is.na(x)),
      Missing = sum(is.na(x)),
      Mean = mean(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      SE = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))),
      Min = min(x, na.rm = TRUE),
      Q1 = quantile(x, 0.25, na.rm = TRUE),
      Median = median(x, na.rm = TRUE),
      Q3 = quantile(x, 0.75, na.rm = TRUE),
      Max = max(x, na.rm = TRUE),
      IQR = IQR(x, na.rm = TRUE)
    )
  }

  result <- as.data.frame(do.call(rbind, stats_list))
  result <- round(result, digits)

  if (transposed) {
    result <- as.data.frame(t(result))
  }

  if (verbose) {
    cat("\n========== DESCRIPTIVE STATISTICS ==========\n")
    cat("Variables analyzed:", ncol(numeric_data), "\n")
    cat("Total observations:", nrow(numeric_data), "\n")
    cat("==============================================\n\n")
    print(result)
  }

  invisible(result)
}

#' Simple Mean Calculation with Options
#'
#' Calculates mean with various options for handling missing values.
#'
#' @param x Numeric vector
#' @param na.rm Logical; if TRUE, removes NA values (default: TRUE)
#' @param trim Fraction of observations to trim (default: 0)
#' @param conf_level Confidence level for CI (default: 0.95)
#' @param verbose Logical; if TRUE, prints detailed summary
#' @return A list with mean, SD, SE, and confidence interval
#' @export
sk_mean <- function(x, na.rm = TRUE, trim = 0, conf_level = 0.95, verbose = TRUE) {

  if (!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    stop("No non-missing values")
  }

  n <- length(x)
  mean_value <- mean(x, trim = trim)
  sd_value <- sd(x)
  se_value <- sd_value / sqrt(n)

  alpha <- 1 - conf_level
  t_crit <- qt(1 - alpha/2, df = n - 1)
  ci_lower <- mean_value - t_crit * se_value
  ci_upper <- mean_value + t_crit * se_value

  result <- list(
    N = n,
    Mean = mean_value,
    SD = sd_value,
    SE = se_value,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper,
    Conf_Level = conf_level,
    Trim = trim
  )

  class(result) <- "sk_mean"

  if (verbose) {
    cat("\n========== MEAN ANALYSIS ==========\n")
    cat("N:", n, "\n")
    cat("Mean:", round(mean_value, 4), "\n")
    cat("SD:", round(sd_value, 4), "\n")
    cat("SE:", round(se_value, 4), "\n")
    cat(conf_level * 100, "% CI: [",
        round(ci_lower, 4), ", ", round(ci_upper, 4), "]\n", sep = "")
    if (trim > 0) {
      cat("Trimmed:", trim * 100, "%\n")
    }
    cat("====================================\n")
  }

  invisible(result)
}

#' Print Method for sk_mean
#' @export
print.sk_mean <- function(x, ...) {
  cat("\nMean Analysis Results\n")
  cat("----------------------\n")
  cat("Mean:", round(x$Mean, 4), "\n")
  cat("SD:  ", round(x$SD, 4), "\n")
  cat("SE:  ", round(x$SE, 4), "\n")
  cat("N:   ", x$N, "\n")
  cat(x$Conf_Level * 100, "% CI: [",
      round(x$CI_Lower, 4), ", ", round(x$CI_Upper, 4), "]\n", sep = "")
}
