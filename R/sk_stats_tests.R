# ============================================================
# SKstats - Complete Statistical Tests Module
# ============================================================

# ============================================================
# 1. T-TESTS
# ============================================================

#' One-Sample T-Test
#'
#' Performs one-sample t-test with detailed output.
#'
#' @param x Numeric vector
#' @param mu Population mean to test against (default: 0)
#' @param alternative Alternative hypothesis: "two.sided", "less", "greater"
#' @param conf_level Confidence level (default: 0.95)
#' @param verbose Logical; if TRUE, prints detailed results
#' @return List with test results
#' @export
sk_t_test_one <- function(x, mu = 0, alternative = "two.sided",
                          conf_level = 0.95, verbose = TRUE) {

  if (!is.numeric(x)) stop("Input must be numeric")
  x <- x[!is.na(x)]

  if (length(x) < 2) {
    warning("Sample size too small for t-test")
    return(invisible(NULL))
  }

  result <- t.test(x, mu = mu, alternative = alternative, conf.level = conf_level)

  if (verbose) {
    cat("\n========== ONE-SAMPLE T-TEST ==========\n")
    cat("Data: x\n")
    cat("t =", round(result$statistic, 4), ", df =", result$parameter,
        ", p-value =", format(result$p.value, scientific = FALSE, digits = 4), "\n")
    cat("Alternative hypothesis: true mean is", alternative, "than", mu, "\n")
    cat(conf_level * 100, "% confidence interval:\n", sep = "")
    cat("  [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n", sep = "")
    cat("Sample estimate:\n")
    cat("  mean of x =", round(result$estimate, 4), "\n")
    cat("=========================================\n")
  }

  invisible(result)
}

#' Two-Sample T-Test
#'
#' Performs two-sample t-test with detailed output.
#'
#' @param x Numeric vector or data frame
#' @param y Numeric vector (if x is vector) or grouping variable (if x is data frame)
#' @param paired Logical; if TRUE, performs paired t-test
#' @param var_equal Logical; if TRUE, assumes equal variances
#' @param alternative Alternative hypothesis: "two.sided", "less", "greater"
#' @param conf_level Confidence level (default: 0.95)
#' @param verbose Logical; if TRUE, prints detailed results
#' @return List with test results
#' @export
sk_t_test_two <- function(x, y = NULL, paired = FALSE,
                          var_equal = FALSE, alternative = "two.sided",
                          conf_level = 0.95, verbose = TRUE) {

  # Handle data frame input
  if (is.data.frame(x) && !is.null(y)) {
    if (!y %in% names(x)) stop("Group variable not found")
    numeric_cols <- sapply(x, is.numeric)
    if (sum(numeric_cols) != 1) stop("Data frame must have one numeric column")
    value_col <- names(x)[numeric_cols]
    formula <- as.formula(paste(value_col, "~", y))
    result <- t.test(formula, data = x, paired = paired,
                     var.equal = var_equal, alternative = alternative,
                     conf.level = conf_level)
  } else {
    # Handle vector input
    if (is.null(y)) {
      stop("For two-sample test, provide both x and y")
    }

    # Remove NAs
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]

    if (length(x) < 2 || length(y) < 2) {
      warning("Sample size too small for t-test")
      return(invisible(NULL))
    }

    result <- t.test(x, y, paired = paired, var.equal = var_equal,
                     alternative = alternative, conf.level = conf_level)
  }

  if (verbose) {
    cat("\n==========", ifelse(paired, "PAIRED", "TWO-SAMPLE"), "T-TEST ==========\n")
    if (paired) {
      cat("Paired t-test\n")
    } else {
      cat("Two-sample t-test\n")
      cat("Variances:", ifelse(var_equal, "equal", "not equal"), "\n")
    }
    cat("t =", round(result$statistic, 4), ", df =", result$parameter,
        ", p-value =", format(result$p.value, scientific = FALSE, digits = 4), "\n")
    cat("Alternative hypothesis: true difference in means is", alternative, "\n")
    cat(conf_level * 100, "% confidence interval:\n", sep = "")
    cat("  [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n", sep = "")
    cat("Sample estimates:\n")
    if (paired) {
      cat("  mean difference =", round(result$estimate, 4), "\n")
    } else {
      if (length(result$estimate) == 2) {
        cat("  mean of x =", round(result$estimate[1], 4), "\n")
        cat("  mean of y =", round(result$estimate[2], 4), "\n")
      }
    }
    cat("================================================\n")
  }

  invisible(result)
}

# ============================================================
# 2. WILCOXON TESTS
# ============================================================

#' Wilcoxon Rank Sum Test
#'
#' Performs Wilcoxon test (non-parametric alternative to t-test).
#'
#' @param x Numeric vector
#' @param y Numeric vector (for two-sample) or NULL (for one-sample)
#' @param paired Logical; if TRUE, performs paired test
#' @param mu Median to test against (for one-sample)
#' @param alternative Alternative hypothesis: "two.sided", "less", "greater"
#' @param conf_level Confidence level (default: 0.95)
#' @param verbose Logical; if TRUE, prints detailed results
#' @return List with test results
#' @export
sk_wilcox_test <- function(x, y = NULL, paired = FALSE, mu = 0,
                           alternative = "two.sided", conf_level = 0.95,
                           verbose = TRUE) {

  # Remove NAs
  x <- x[!is.na(x)]

  if (is.null(y)) {
    if (length(x) < 2) {
      warning("Sample size too small for Wilcoxon test")
      return(invisible(NULL))
    }
    result <- wilcox.test(x, mu = mu, alternative = alternative,
                          conf.int = TRUE, conf.level = conf_level)
    test_name <- "ONE-SAMPLE WILCOXON"
  } else {
    y <- y[!is.na(y)]
    if (length(x) < 2 || length(y) < 2) {
      warning("Sample size too small for Wilcoxon test")
      return(invisible(NULL))
    }
    result <- wilcox.test(x, y, paired = paired, alternative = alternative,
                          conf.int = TRUE, conf.level = conf_level)
    test_name <- ifelse(paired, "PAIRED WILCOXON", "TWO-SAMPLE WILCOXON")
  }

  if (verbose) {
    cat("\n==========", test_name, "TEST ==========\n")
    cat("W =", round(result$statistic, 4),
        ", p-value =", format(result$p.value, scientific = FALSE, digits = 4), "\n")
    cat("Alternative hypothesis: true location is", alternative, "\n")
    if (!is.null(result$estimate)) {
      cat("Sample estimate:", round(result$estimate, 4), "\n")
    }
    if (!is.null(result$conf.int)) {
      cat(conf_level * 100, "% confidence interval:\n", sep = "")
      cat("  [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n", sep = "")
    }
    cat("=======================================\n")
  }

  invisible(result)
}

# ============================================================
# 3. CHI-SQUARE TESTS
# ============================================================

#' Chi-Square Test
#'
#' Performs chi-square test for categorical data.
#'
#' @param x Data frame or matrix or vector
#' @param y Vector (for two-way table)
#' @param p Expected probabilities (for goodness of fit)
#' @param simulate.p.value Logical; if TRUE, simulates p-value
#' @param B Number of simulations (default: 2000)
#' @param verbose Logical; if TRUE, prints detailed results
#' @return List with test results
#' @export
sk_chi_square <- function(x, y = NULL, p = NULL, simulate.p.value = FALSE,
                          B = 2000, verbose = TRUE) {

  if (is.vector(x) && !is.matrix(x)) {
    # Goodness of fit
    if (is.null(p)) {
      p <- rep(1/length(x), length(x))
    }
    result <- chisq.test(x, p = p, simulate.p.value = simulate.p.value, B = B)
    test_name <- "CHI-SQUARE GOODNESS OF FIT"
  } else {
    # Contingency table
    if (is.null(y)) {
      if (!is.matrix(x) && !is.data.frame(x)) {
        stop("Input must be a matrix or data frame")
      }
      result <- chisq.test(x, simulate.p.value = simulate.p.value, B = B)
    } else {
      # Create table from two vectors
      if (length(x) != length(y)) stop("x and y must have same length")
      if (!is.factor(x)) x <- as.factor(x)
      if (!is.factor(y)) y <- as.factor(y)
      result <- chisq.test(x, y, simulate.p.value = simulate.p.value, B = B)
    }
    test_name <- "CHI-SQUARE TEST OF INDEPENDENCE"
  }

  if (verbose) {
    cat("\n==========", test_name, "==========\n")
    cat("X-squared =", round(result$statistic, 4),
        ", df =", result$parameter,
        ", p-value =", format(result$p.value, scientific = FALSE, digits = 4), "\n")
    if (simulate.p.value) {
      cat("Simulated p-value based on", B, "replicates\n")
    }
    if (!is.null(result$observed)) {
      cat("\nObserved counts:\n")
      print(result$observed)
    }
    if (!is.null(result$expected)) {
      cat("\nExpected counts:\n")
      print(round(result$expected, 2))
    }
    cat("========================================\n")
  }

  invisible(result)
}

#' Fisher's Exact Test
#'
#' Performs Fisher's exact test for small sample categorical data.
#'
#' @param x Matrix or data frame or vector
#' @param y Vector (for two variables)
#' @param alternative Alternative hypothesis: "two.sided", "less", "greater"
#' @param simulate.p.value Logical; if TRUE, simulates p-value
#' @param B Number of simulations (default: 2000)
#' @param verbose Logical; if TRUE, prints detailed results
#' @return List with test results
#' @export
sk_fisher_test <- function(x, y = NULL, alternative = "two.sided",
                           simulate.p.value = FALSE, B = 2000,
                           verbose = TRUE) {

  if (!is.null(y)) {
    # Create contingency table
    if (length(x) != length(y)) stop("x and y must have same length")
    if (!is.factor(x)) x <- as.factor(x)
    if (!is.factor(y)) y <- as.factor(y)
    x <- table(x, y)
  }

  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("Input must be a matrix or data frame")
  }

  # Check if table is 2x2
  if (nrow(x) != 2 || ncol(x) != 2) {
    warning("Fisher's exact test works best with 2x2 tables")
  }

  result <- fisher.test(x, alternative = alternative,
                        simulate.p.value = simulate.p.value, B = B)

  if (verbose) {
    cat("\n========== FISHER'S EXACT TEST ==========\n")
    cat("p-value =", format(result$p.value, scientific = FALSE, digits = 4), "\n")
    cat("Alternative hypothesis: true odds ratio is", alternative, "\n")
    if (!is.null(result$estimate)) {
      cat("Odds ratio:", round(result$estimate, 4), "\n")
    }
    if (!is.null(result$conf.int)) {
      cat("95% confidence interval:\n")
      cat("  [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n", sep = "")
    }
    cat("==========================================\n")
  }

  invisible(result)
}

# ============================================================
# 4. ANOVA
# ============================================================

#' One-Way ANOVA
#'
#' Performs one-way ANOVA with detailed output.
#'
#' @param formula Formula: response ~ group
#' @param data Data frame
#' @param verbose Logical; if TRUE, prints detailed results
#' @return List with test results
#' @export
sk_anova <- function(formula, data, verbose = TRUE) {

  # Check if data has enough observations
  if (nrow(data) < 3) {
    warning("Too few observations for ANOVA")
    return(invisible(NULL))
  }

  result <- aov(formula, data = data)
  summary_result <- summary(result)

  if (verbose) {
    cat("\n========== ONE-WAY ANOVA ==========\n")
    print(summary_result)

    # Check if we can do post-hoc
    if (length(unique(data[[all.vars(formula)[2]]])) >= 3) {
      cat("\nTukey's HSD Post-hoc Test:\n")
      print(TukeyHSD(result))
    }
    cat("====================================\n")
  }

  invisible(result)
}

#' Kruskal-Wallis Test
#'
#' Performs Kruskal-Wallis test (non-parametric alternative to ANOVA).
#'
#' @param formula Formula: response ~ group
#' @param data Data frame
#' @param verbose Logical; if TRUE, prints detailed results
#' @return List with test results
#' @export
sk_kruskal_test <- function(formula, data, verbose = TRUE) {

  # Check if data has enough observations
  if (nrow(data) < 3) {
    warning("Too few observations for Kruskal-Wallis test")
    return(invisible(NULL))
  }

  result <- kruskal.test(formula, data = data)

  if (verbose) {
    cat("\n========== KRUSKAL-WALLIS TEST ==========\n")
    cat("Chi-squared =", round(result$statistic, 4),
        ", df =", result$parameter,
        ", p-value =", format(result$p.value, scientific = FALSE, digits = 4), "\n")
    cat("==========================================\n")
  }

  invisible(result)
}

# ============================================================
# 5. CORRELATION TESTS
# ============================================================

#' Correlation Analysis
#'
#' Performs correlation analysis between variables.
#'
#' @param x Numeric vector or data frame
#' @param y Numeric vector (if x is vector)
#' @param method Correlation method: "pearson", "spearman", "kendall"
#' @param alternative Alternative hypothesis: "two.sided", "less", "greater"
#' @param conf_level Confidence level (default: 0.95)
#' @param verbose Logical; if TRUE, prints detailed results
#' @return List with test results
#' @export
sk_correlation_test <- function(x, y = NULL, method = "pearson",
                                alternative = "two.sided", conf_level = 0.95,
                                verbose = TRUE) {

  if (is.data.frame(x) && is.null(y)) {
    # Correlation matrix
    numeric_cols <- sapply(x, is.numeric)
    if (sum(numeric_cols) < 2) {
      stop("Need at least 2 numeric columns for correlation matrix")
    }
    result <- cor(x[, numeric_cols], method = method)
    if (verbose) {
      cat("\n========== CORRELATION MATRIX ==========\n")
      cat("Method:", method, "\n")
      print(round(result, 4))
      cat("=========================================\n")
    }
    invisible(result)
  } else {
    # Single correlation test
    if (is.null(y)) {
      stop("For correlation test, provide both x and y")
    }

    # Remove NAs
    complete_idx <- complete.cases(x, y)
    x <- x[complete_idx]
    y <- y[complete_idx]

    if (length(x) < 3) {
      warning("Too few observations for correlation test")
      return(invisible(NULL))
    }

    result <- cor.test(x, y, method = method, alternative = alternative,
                       conf.level = conf_level)

    if (verbose) {
      cat("\n========== CORRELATION TEST ==========\n")
      cat("Method:", method, "\n")
      cat("Correlation =", round(result$estimate, 4),
          ", p-value =", format(result$p.value, scientific = FALSE, digits = 4), "\n")
      cat("Alternative hypothesis: true correlation is", alternative, "\n")
      cat(conf_level * 100, "% confidence interval:\n", sep = "")
      cat("  [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n", sep = "")
      cat("========================================\n")
    }

    invisible(result)
  }
}

# ============================================================
# 6. PROPORTION TESTS
# ============================================================

#' Test of Proportions
#'
#' Performs one or two-sample test of proportions.
#'
#' @param x Number of successes or vector of counts
#' @param n Number of trials or vector of totals
#' @param p Population proportion to test against (for one-sample)
#' @param alternative Alternative hypothesis: "two.sided", "less", "greater"
#' @param conf_level Confidence level (default: 0.95)
#' @param verbose Logical; if TRUE, prints detailed results
#' @return List with test results
#' @export
sk_prop_test <- function(x, n, p = NULL, alternative = "two.sided",
                         conf_level = 0.95, verbose = TRUE) {

  if (length(x) == 1) {
    # One-sample test
    if (is.null(p)) p <- 0.5
    if (x > n) stop("x cannot be greater than n")
    result <- prop.test(x, n, p = p, alternative = alternative,
                        conf.level = conf_level)
    test_name <- "ONE-SAMPLE PROPORTION TEST"
  } else {
    # Two-sample test
    if (length(x) != length(n)) stop("x and n must have same length")
    if (any(x > n)) stop("x cannot be greater than n")
    result <- prop.test(x, n, alternative = alternative,
                        conf.level = conf_level)
    test_name <- "TWO-SAMPLE PROPORTION TEST"
  }

  if (verbose) {
    cat("\n==========", test_name, "==========\n")
    cat("X-squared =", round(result$statistic, 4),
        ", df =", result$parameter,
        ", p-value =", format(result$p.value, scientific = FALSE, digits = 4), "\n")
    cat("Alternative hypothesis: true proportion is", alternative, "\n")
    cat(conf_level * 100, "% confidence interval:\n", sep = "")
    cat("  [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n", sep = "")
    cat("Sample estimates:\n")
    cat("  proportion =", round(result$estimate, 4), "\n")
    cat("==========================================\n")
  }

  invisible(result)
}

# ============================================================
# 7. NORMALITY TESTS
# ============================================================

#' Shapiro-Wilk Test of Normality
#'
#' Performs Shapiro-Wilk test for normality.
#'
#' @param x Numeric vector
#' @param verbose Logical; if TRUE, prints detailed results
#' @return List with test results
#' @export
sk_shapiro_test <- function(x, verbose = TRUE) {

  x <- x[!is.na(x)]

  if (length(x) < 3) {
    warning("Sample size must be at least 3")
    return(invisible(NULL))
  }

  if (length(x) > 5000) {
    warning("Shapiro-Wilk test may be unreliable for large samples (>5000)")
  }

  result <- shapiro.test(x)

  if (verbose) {
    cat("\n========== SHAPIRO-WILK NORMALITY TEST ==========\n")
    cat("W =", round(result$statistic, 4),
        ", p-value =", format(result$p.value, scientific = FALSE, digits = 4), "\n")
    cat("Null hypothesis: data is normally distributed\n")
    if (result$p.value < 0.05) {
      cat("Conclusion: Data is NOT normally distributed (p < 0.05)\n")
    } else {
      cat("Conclusion: Data appears normally distributed (p >= 0.05)\n")
    }
    cat("====================================================\n")
  }

  invisible(result)
}

#' Kolmogorov-Smirnov Test of Normality
#'
#' Performs Kolmogorov-Smirnov test for normality.
#'
#' @param x Numeric vector
#' @param verbose Logical; if TRUE, prints detailed results
#' @return List with test results
#' @export
sk_ks_test <- function(x, verbose = TRUE) {

  x <- x[!is.na(x)]

  if (length(x) < 2) {
    warning("Sample size too small for KS test")
    return(invisible(NULL))
  }

  # Remove ties for KS test (approximate)
  x_unique <- unique(x)

  if (length(x_unique) < 2) {
    warning("Too few unique values for KS test")
    return(invisible(NULL))
  }

  # Use original x with ties for the test
  # The warning is informative but not critical
  result <- ks.test(x, "pnorm", mean(x), sd(x))

  if (verbose) {
    cat("\n========== KOLMOGOROV-SMIRNOV NORMALITY TEST ==========\n")
    cat("D =", round(result$statistic, 4),
        ", p-value =", format(result$p.value, scientific = FALSE, digits = 4), "\n")
    cat("Null hypothesis: data is normally distributed\n")
    if (result$p.value < 0.05) {
      cat("Conclusion: Data is NOT normally distributed (p < 0.05)\n")
    } else {
      cat("Conclusion: Data appears normally distributed (p >= 0.05)\n")
    }
    cat("=========================================================\n")
  }

  invisible(result)
}

# ============================================================
# 8. EFFECT SIZE
# ============================================================

#' Calculate Effect Size
#'
#' Calculates Cohen's d, Hedges' g, and other effect size measures.
#'
#' @param x Numeric vector or group 1
#' @param y Numeric vector (group 2) or grouping variable
#' @param paired Logical; if TRUE, calculates paired effect size
#' @param verbose Logical; if TRUE, prints detailed results
#' @return List with effect size measures
#' @export
sk_effect_size <- function(x, y = NULL, paired = FALSE,
                           verbose = TRUE) {

  # Remove NAs
  x <- x[!is.na(x)]

  if (paired) {
    if (is.null(y)) stop("For paired effect size, provide both x and y")
    y <- y[!is.na(y)]

    if (length(x) != length(y)) stop("x and y must have same length")
    if (length(x) < 2) {
      warning("Too few observations for paired effect size")
      return(invisible(NULL))
    }

    d <- mean(x - y, na.rm = TRUE) / sd(x - y, na.rm = TRUE)
    result <- list(
      cohen_d = d,
      interpretation = ifelse(abs(d) < 0.2, "negligible",
                              ifelse(abs(d) < 0.5, "small",
                                     ifelse(abs(d) < 0.8, "medium", "large")))
    )
  } else {
    if (is.null(y)) {
      # If y is a grouping variable (factor or character)
      if (is.factor(x) || is.character(x)) {
        stop("Please provide numeric vectors or use formula interface")
      }
      stop("For two-sample, provide both x and y")
    }

    # Check if y is a grouping variable
    if (is.factor(y) || is.character(y)) {
      # Grouping variable provided
      groups <- unique(y)
      if (length(groups) != 2) {
        stop("Effect size requires exactly 2 groups")
      }
      group1 <- x[y == groups[1]]
      group2 <- x[y == groups[2]]
    } else {
      # Two numeric vectors
      group1 <- x
      group2 <- y[!is.na(y)]
    }

    # Remove NAs
    group1 <- group1[!is.na(group1)]
    group2 <- group2[!is.na(group2)]

    if (length(group1) < 2 || length(group2) < 2) {
      warning("Too few observations in one or both groups")
      return(invisible(NULL))
    }

    n1 <- length(group1)
    n2 <- length(group2)
    mean1 <- mean(group1)
    mean2 <- mean(group2)
    sd1 <- sd(group1)
    sd2 <- sd(group2)

    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))

    # Cohen's d
    cohen_d <- (mean1 - mean2) / pooled_sd

    # Hedges' g (bias corrected)
    correction <- 1 - 3 / (4 * (n1 + n2) - 9)
    hedges_g <- cohen_d * correction

    result <- list(
      cohen_d = cohen_d,
      hedges_g = hedges_g,
      pooled_sd = pooled_sd,
      mean1 = mean1,
      mean2 = mean2,
      n1 = n1,
      n2 = n2,
      cohen_interpretation = ifelse(abs(cohen_d) < 0.2, "negligible",
                                    ifelse(abs(cohen_d) < 0.5, "small",
                                           ifelse(abs(cohen_d) < 0.8, "medium", "large"))),
      hedges_interpretation = ifelse(abs(hedges_g) < 0.2, "negligible",
                                     ifelse(abs(hedges_g) < 0.5, "small",
                                            ifelse(abs(hedges_g) < 0.8, "medium", "large")))
    )
  }

  if (verbose) {
    cat("\n========== EFFECT SIZE ==========\n")
    if (paired) {
      cat("Paired effect size:\n")
      cat("Cohen's d =", round(result$cohen_d, 4), "\n")
      cat("Interpretation:", result$interpretation, "\n")
    } else {
      cat("Group 1 (n =", result$n1, "): Mean =", round(result$mean1, 4), "\n")
      cat("Group 2 (n =", result$n2, "): Mean =", round(result$mean2, 4), "\n")
      cat("\nCohen's d =", round(result$cohen_d, 4), "\n")
      cat("Interpretation:", result$cohen_interpretation, "\n\n")
      cat("Hedges' g =", round(result$hedges_g, 4), "\n")
      cat("Interpretation:", result$hedges_interpretation, "\n")
    }
    cat("==================================\n")
  }

  invisible(result)
}

# ============================================================
# 9. COMPREHENSIVE ANALYSIS
# ============================================================

#' Comprehensive Statistical Analysis
#'
#' Performs comprehensive statistical analysis including descriptive statistics,
#' normality tests, and appropriate hypothesis tests.
#'
#' @param data Data frame
#' @param x Variable name (numeric)
#' @param y Variable name (grouping variable or numeric)
#' @param verbose Logical; if TRUE, prints detailed results
#' @return List with all test results
#' @export
sk_comprehensive_analysis <- function(data, x = NULL, y = NULL,
                                      verbose = TRUE) {

  results <- list()

  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
    cat("COMPREHENSIVE STATISTICAL ANALYSIS\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
  }

  # Check if x is provided and exists
  if (!is.null(x)) {
    if (!x %in% names(data)) {
      stop(paste("Variable", x, "not found in data"))
    }

    # Check if x is numeric
    if (!is.numeric(data[[x]])) {
      stop(paste("Variable", x, "must be numeric"))
    }

    # Remove NAs for analysis
    x_data <- data[[x]][!is.na(data[[x]])]

    if (length(x_data) < 2) {
      warning("Too few observations for analysis")
      return(invisible(NULL))
    }

    # Descriptive statistics
    cat("--- DESCRIPTIVE STATISTICS ---\n")
    results$descriptive <- sk_stat(data[, x, drop = FALSE])
    cat("\n")

    # Normality tests
    cat("--- NORMALITY TESTS ---\n")
    results$shapiro <- sk_shapiro_test(data[[x]], verbose = verbose)
    results$ks <- sk_ks_test(data[[x]], verbose = verbose)
    cat("\n")
  }

  # Hypothesis tests (if y is provided)
  if (!is.null(x) && !is.null(y)) {
    if (!y %in% names(data)) {
      stop(paste("Variable", y, "not found in data"))
    }

    cat("--- HYPOTHESIS TESTS ---\n")

    # Case 1: x is numeric, y is categorical (grouping variable)
    if (is.numeric(data[[x]]) && (is.factor(data[[y]]) || is.character(data[[y]]))) {

      # Get unique groups
      groups <- unique(data[[y]])
      groups <- groups[!is.na(groups)]

      if (length(groups) >= 2) {
        # Check if each group has enough data
        valid_groups <- sapply(groups, function(g) {
          sum(!is.na(data[[x]][data[[y]] == g])) >= 2
        })

        if (all(valid_groups)) {
          # Two-sample t-test if exactly 2 groups
          if (length(groups) == 2) {
            group1 <- data[[x]][data[[y]] == groups[1]]
            group2 <- data[[x]][data[[y]] == groups[2]]
            group1 <- group1[!is.na(group1)]
            group2 <- group2[!is.na(group2)]

            if (length(group1) >= 2 && length(group2) >= 2) {
              results$t_test <- sk_t_test_two(group1, group2)
              results$effect_size <- sk_effect_size(group1, group2)
            }
          }

          # ANOVA (for 2+ groups)
          if (length(groups) >= 3) {
            results$anova <- sk_anova(as.formula(paste0("`", x, "` ~ `", y, "`")), data)
          }

          # Kruskal-Wallis (non-parametric)
          results$kruskal <- sk_kruskal_test(as.formula(paste0("`", x, "` ~ `", y, "`")), data)
        } else {
          cat("  Some groups have insufficient data for tests\n")
        }
      } else {
        cat("  Less than 2 groups available for testing\n")
      }

    } else if (is.numeric(data[[x]]) && is.numeric(data[[y]])) {
      # Case 2: Both variables are numeric - Correlation
      # Remove NAs
      complete_idx <- complete.cases(data[[x]], data[[y]])
      x_data <- data[[x]][complete_idx]
      y_data <- data[[y]][complete_idx]

      if (length(x_data) >= 3) {
        results$correlation <- sk_correlation_test(x_data, y_data)

        # Also do regression
        model <- lm(x_data ~ y_data)
        results$regression <- summary(model)
      } else {
        cat("  Too few observations for correlation\n")
      }

    } else if ((is.factor(data[[x]]) || is.character(data[[x]])) &&
               (is.factor(data[[y]]) || is.character(data[[y]]))) {
      # Case 3: Both categorical - Chi-square
      # Remove rows with NA
      complete_idx <- complete.cases(data[[x]], data[[y]])
      x_data <- data[[x]][complete_idx]
      y_data <- data[[y]][complete_idx]

      # Create contingency table
      table_data <- table(x_data, y_data)

      # Check if table is valid (at least 2x2)
      if (nrow(table_data) >= 2 && ncol(table_data) >= 2) {
        # Check if all expected frequencies >= 5
        expected <- chisq.test(table_data)$expected
        if (all(expected >= 5)) {
          results$chi_square <- sk_chi_square(table_data)
        } else {
          # Use Fisher's exact test for small samples
          results$fisher <- sk_fisher_test(table_data)
        }
      } else {
        cat("  Cannot perform chi-square test: table has less than 2x2\n")
      }
    }
    cat("\n")
  }

  if (verbose) {
    cat(paste(rep("=", 60), collapse = ""), "\n")
    cat("ANALYSIS COMPLETE\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }

  invisible(results)
}
