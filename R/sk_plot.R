# ============================================================
# SKplot - Complete Visualization Module with ggplot2
# ============================================================

# ============================================================
# SKTHEME - Theme Function (Required)
# ============================================================

#' Custom SKthink Theme for ggplot2
#'
#' Returns a consistent theme for all SKthink plots.
#'
#' @param theme_name Theme name: "minimal", "classic", "bw", "dark", "light", "void"
#' @param base_size Base font size (default: 12)
#' @param legend_position Legend position: "top", "bottom", "left", "right", "none"
#' @return ggplot2 theme object
#' @export
sk_theme <- function(theme_name = "minimal", base_size = 12, legend_position = "right") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }

  # Base theme
  if (theme_name == "minimal") {
    theme <- ggplot2::theme_minimal(base_size = base_size)
  } else if (theme_name == "classic") {
    theme <- ggplot2::theme_classic(base_size = base_size)
  } else if (theme_name == "bw") {
    theme <- ggplot2::theme_bw(base_size = base_size)
  } else if (theme_name == "dark") {
    theme <- ggplot2::theme_dark(base_size = base_size)
  } else if (theme_name == "light") {
    theme <- ggplot2::theme_light(base_size = base_size)
  } else if (theme_name == "void") {
    theme <- ggplot2::theme_void(base_size = base_size)
  } else {
    theme <- ggplot2::theme_minimal(base_size = base_size)
  }

  # Add legend position
  theme <- theme +
    ggplot2::theme(
      legend.position = legend_position,
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = base_size + 2),
      axis.title = ggplot2::element_text(face = "bold", size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1)
    )

  return(theme)
}

# ============================================================
# 1. BASIC PLOTS
# ============================================================

#' Scatter Plot
#'
#' Creates a publication-ready scatter plot with options for regression line,
#' confidence bands, and customization.
#'
#' @param data Data frame
#' @param x X-axis variable
#' @param y Y-axis variable
#' @param color Grouping variable for colors
#' @param shape Grouping variable for shapes
#' @param size Point size (default: 3)
#' @param alpha Point transparency (default: 0.7)
#' @param add_smooth Logical; if TRUE, adds LOESS smooth line
#' @param add_regression Logical; if TRUE, adds linear regression line
#' @param add_ci Logical; if TRUE, adds confidence interval
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param theme ggplot2 theme: "minimal", "classic", "bw", "dark", "light", "void"
#' @param legend_position Legend position: "top", "bottom", "left", "right", "none"
#' @param base_size Base font size (default: 12)
#' @param ... Additional arguments passed to geom_point()
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' sk_scatter(mtcars, x = "wt", y = "mpg", color = "cyl", add_regression = TRUE)
#' }
sk_scatter <- function(data, x, y, color = NULL, shape = NULL,
                       size = 3, alpha = 0.7,
                       add_smooth = FALSE, add_regression = FALSE, add_ci = TRUE,
                       title = NULL, xlab = NULL, ylab = NULL,
                       theme = "minimal", legend_position = "right",
                       base_size = 12, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required. Install with: install.packages('ggplot2')")
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]]))

  # Add points
  if (!is.null(color) && !is.null(shape)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = .data[[color]], shape = .data[[shape]]),
                                 size = size, alpha = alpha, ...)
  } else if (!is.null(color)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = .data[[color]]),
                                 size = size, alpha = alpha, ...)
  } else if (!is.null(shape)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(shape = .data[[shape]]),
                                 size = size, alpha = alpha, ...)
  } else {
    p <- p + ggplot2::geom_point(size = size, alpha = alpha, ...)
  }

  # Add smooth line
  if (add_smooth) {
    p <- p + ggplot2::geom_smooth(method = "loess", se = add_ci, color = "red")
  }

  # Add regression line
  if (add_regression) {
    p <- p + ggplot2::geom_smooth(method = "lm", se = add_ci, color = "blue")
  }

  # Labels
  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  if (!is.null(xlab)) p <- p + ggplot2::xlab(xlab) else p <- p + ggplot2::xlab(x)
  if (!is.null(ylab)) p <- p + ggplot2::ylab(ylab) else p <- p + ggplot2::ylab(y)

  # Theme
  p <- p + sk_theme(theme, base_size, legend_position)

  return(p)
}

#' Line Plot
#'
#' Creates line plots for time series or trends.
#'
#' @param data Data frame
#' @param x X-axis variable
#' @param y Y-axis variable
#' @param color Grouping variable for colors
#' @param linetype Grouping variable for linetypes
#' @param line_size Line width (default: 1)
#' @param add_points Logical; if TRUE, adds points
#' @param point_size Point size (default: 2)
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param theme ggplot2 theme
#' @param legend_position Legend position
#' @param base_size Base font size (default: 12)
#' @return ggplot object
#' @export
sk_line <- function(data, x, y, color = NULL, linetype = NULL,
                    line_size = 1, add_points = FALSE, point_size = 2,
                    title = NULL, xlab = NULL, ylab = NULL,
                    theme = "minimal", legend_position = "right",
                    base_size = 12) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]]))

  if (!is.null(color) && !is.null(linetype)) {
    p <- p + ggplot2::geom_line(ggplot2::aes(color = .data[[color]], linetype = .data[[linetype]]),
                                size = line_size)
  } else if (!is.null(color)) {
    p <- p + ggplot2::geom_line(ggplot2::aes(color = .data[[color]]), size = line_size)
  } else if (!is.null(linetype)) {
    p <- p + ggplot2::geom_line(ggplot2::aes(linetype = .data[[linetype]]), size = line_size)
  } else {
    p <- p + ggplot2::geom_line(size = line_size)
  }

  if (add_points) {
    p <- p + ggplot2::geom_point(size = point_size)
  }

  # Labels
  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  if (!is.null(xlab)) p <- p + ggplot2::xlab(xlab) else p <- p + ggplot2::xlab(x)
  if (!is.null(ylab)) p <- p + ggplot2::ylab(ylab) else p <- p + ggplot2::ylab(y)

  p <- p + sk_theme(theme, base_size, legend_position)

  return(p)
}

#' Bar Plot
#'
#' Creates bar plots with error bars.
#'
#' @param data Data frame
#' @param x X-axis variable (categorical)
#' @param y Y-axis variable (numeric)
#' @param fill Fill color/grouping
#' @param position Position: "stack", "dodge", "fill"
#' @param add_error Logical; if TRUE, adds error bars
#' @param error_type Error type: "sd", "se", "ci"
#' @param stat Statistical transformation: "count", "identity", "summary"
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param theme ggplot2 theme
#' @param legend_position Legend position
#' @param base_size Base font size (default: 12)
#' @return ggplot object
#' @export
sk_bar <- function(data, x, y = NULL, fill = NULL, position = "dodge",
                   add_error = FALSE, error_type = "sd",
                   stat = "identity", title = NULL, xlab = NULL, ylab = NULL,
                   theme = "minimal", legend_position = "right",
                   base_size = 12) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }

  if (!is.null(y)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]]))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]]))
  }

  if (!is.null(fill)) {
    p <- p + ggplot2::geom_bar(ggplot2::aes(fill = .data[[fill]]),
                               position = position, stat = stat)
  } else {
    p <- p + ggplot2::geom_bar(position = position, stat = stat)
  }

  # Add error bars (simplified version)
  if (add_error && !is.null(y)) {
    # Calculate summary stats
    error_summary <- aggregate(data[[y]], by = list(data[[x]]),
                               FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
    names(error_summary) <- c("x", "stats")
    error_summary$mean <- error_summary$stats[, "mean"]
    error_summary$sd <- error_summary$stats[, "sd"]
    error_summary$n <- error_summary$stats[, "n"]

    if (error_type == "sd") {
      error_summary$ymin <- error_summary$mean - error_summary$sd
      error_summary$ymax <- error_summary$mean + error_summary$sd
    } else if (error_type == "se") {
      error_summary$ymin <- error_summary$mean - error_summary$sd / sqrt(error_summary$n)
      error_summary$ymax <- error_summary$mean + error_summary$sd / sqrt(error_summary$n)
    } else if (error_type == "ci") {
      error_summary$ymin <- error_summary$mean - 1.96 * error_summary$sd / sqrt(error_summary$n)
      error_summary$ymax <- error_summary$mean + 1.96 * error_summary$sd / sqrt(error_summary$n)
    }

    p <- p + ggplot2::geom_errorbar(data = error_summary,
                                    ggplot2::aes(x = x, ymin = ymin, ymax = ymax),
                                    width = 0.2)
  }

  # Labels
  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  if (!is.null(xlab)) p <- p + ggplot2::xlab(xlab) else p <- p + ggplot2::xlab(x)
  if (!is.null(ylab)) p <- p + ggplot2::ylab(ylab) else if (!is.null(y)) p <- p + ggplot2::ylab(y)

  p <- p + sk_theme(theme, base_size, legend_position)

  return(p)
}

#' Histogram
#'
#' Creates histograms with density overlay options.
#'
#' @param data Data frame or numeric vector
#' @param x Variable name or numeric vector
#' @param bins Number of bins
#' @param binwidth Width of bins
#' @param fill Fill color
#' @param color Border color
#' @param alpha Transparency (default: 0.7)
#' @param add_density Logical; if TRUE, adds density curve
#' @param add_normal Logical; if TRUE, adds normal curve
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param theme ggplot2 theme
#' @param base_size Base font size (default: 12)
#' @return ggplot object
#' @export
sk_histogram <- function(data, x, bins = 30, binwidth = NULL,
                         fill = "skyblue", color = "white", alpha = 0.7,
                         add_density = FALSE, add_normal = FALSE,
                         title = NULL, xlab = NULL, ylab = "Count",
                         theme = "minimal", base_size = 12) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }

  # Handle vector input
  if (is.numeric(data) && missing(x)) {
    x_vals <- data
    p <- ggplot2::ggplot(data.frame(x = x_vals), ggplot2::aes(x = x))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]]))
  }

  p <- p + ggplot2::geom_histogram(bins = bins, binwidth = binwidth,
                                   fill = fill, color = color, alpha = alpha)

  if (add_density) {
    p <- p + ggplot2::geom_density(alpha = 0.3, fill = "red")
  }

  if (add_normal) {
    if (is.numeric(data) && missing(x)) {
      mean_val <- mean(x_vals, na.rm = TRUE)
      sd_val <- sd(x_vals, na.rm = TRUE)
      p <- p + ggplot2::stat_function(fun = function(z) {
        diff(hist(x_vals, plot = FALSE)$breaks[1:2]) * length(x_vals) * dnorm(z, mean_val, sd_val)
      }, color = "red", size = 1)
    } else {
      mean_val <- mean(data[[x]], na.rm = TRUE)
      sd_val <- sd(data[[x]], na.rm = TRUE)
      p <- p + ggplot2::stat_function(fun = function(z) {
        diff(hist(data[[x]], plot = FALSE)$breaks[1:2]) * length(data[[x]]) * dnorm(z, mean_val, sd_val)
      }, color = "red", size = 1)
    }
  }

  # Labels
  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  if (!is.null(xlab)) p <- p + ggplot2::xlab(xlab) else p <- p + ggplot2::xlab(x)
  if (!is.null(ylab)) p <- p + ggplot2::ylab(ylab)

  p <- p + sk_theme(theme, base_size)

  return(p)
}

#' Box Plot
#'
#' Creates box plots with jittered points.
#'
#' @param data Data frame
#' @param x X-axis variable (categorical)
#' @param y Y-axis variable (numeric)
#' @param fill Fill color/grouping
#' @param add_points Logical; if TRUE, adds jittered points
#' @param point_size Point size (default: 1)
#' @param point_alpha Point transparency (default: 0.5)
#' @param notch Logical; if TRUE, adds notches
#' @param outlier_shape Shape for outliers (default: 19)
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param theme ggplot2 theme
#' @param legend_position Legend position
#' @param base_size Base font size (default: 12)
#' @return ggplot object
#' @export
sk_boxplot <- function(data, x, y, fill = NULL,
                       add_points = FALSE, point_size = 1, point_alpha = 0.5,
                       notch = FALSE, outlier_shape = 19,
                       title = NULL, xlab = NULL, ylab = NULL,
                       theme = "minimal", legend_position = "right",
                       base_size = 12) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }

  if (!is.null(fill)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill]]))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]]))
  }

  p <- p + ggplot2::geom_boxplot(notch = notch, outlier.shape = outlier_shape)

  if (add_points) {
    p <- p + ggplot2::geom_jitter(width = 0.2, size = point_size, alpha = point_alpha)
  }

  # Labels
  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  if (!is.null(xlab)) p <- p + ggplot2::xlab(xlab) else p <- p + ggplot2::xlab(x)
  if (!is.null(ylab)) p <- p + ggplot2::ylab(ylab) else p <- p + ggplot2::ylab(y)

  p <- p + sk_theme(theme, base_size, legend_position)

  return(p)
}

#' Violin Plot
#'
#' Creates violin plots with box plot overlay.
#'
#' @param data Data frame
#' @param x X-axis variable (categorical)
#' @param y Y-axis variable (numeric)
#' @param fill Fill color/grouping
#' @param add_boxplot Logical; if TRUE, adds box plot overlay
#' @param add_points Logical; if TRUE, adds points
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param theme ggplot2 theme
#' @param legend_position Legend position
#' @param base_size Base font size (default: 12)
#' @return ggplot object
#' @export
sk_violin <- function(data, x, y, fill = NULL,
                      add_boxplot = TRUE, add_points = FALSE,
                      title = NULL, xlab = NULL, ylab = NULL,
                      theme = "minimal", legend_position = "right",
                      base_size = 12) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }

  if (!is.null(fill)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill]]))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]]))
  }

  p <- p + ggplot2::geom_violin(trim = FALSE)

  if (add_boxplot) {
    p <- p + ggplot2::geom_boxplot(width = 0.1, fill = "white")
  }

  if (add_points) {
    p <- p + ggplot2::geom_jitter(width = 0.2, size = 1, alpha = 0.5)
  }

  # Labels
  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  if (!is.null(xlab)) p <- p + ggplot2::xlab(xlab) else p <- p + ggplot2::xlab(x)
  if (!is.null(ylab)) p <- p + ggplot2::ylab(ylab) else p <- p + ggplot2::ylab(y)

  p <- p + sk_theme(theme, base_size, legend_position)

  return(p)
}

#' Density Plot
#'
#' Creates density plots for distribution visualization.
#'
#' @param data Data frame or numeric vector
#' @param x Variable name or numeric vector
#' @param fill Fill color/grouping
#' @param color Line color
#' @param alpha Transparency (default: 0.7)
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param theme ggplot2 theme
#' @param legend_position Legend position
#' @param base_size Base font size (default: 12)
#' @return ggplot object
#' @export
sk_density <- function(data, x, fill = NULL, color = "blue", alpha = 0.7,
                       title = NULL, xlab = NULL, ylab = "Density",
                       theme = "minimal", legend_position = "right",
                       base_size = 12) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }

  # Handle vector input
  if (is.numeric(data) && missing(x)) {
    x_vals <- data
    p <- ggplot2::ggplot(data.frame(x = x_vals), ggplot2::aes(x = x))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]]))
  }

  if (!is.null(fill)) {
    p <- p + ggplot2::geom_density(ggplot2::aes(fill = .data[[fill]]), alpha = alpha)
  } else {
    p <- p + ggplot2::geom_density(fill = fill, color = color, alpha = alpha)
  }

  # Labels
  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  if (!is.null(xlab)) p <- p + ggplot2::xlab(xlab) else p <- p + ggplot2::xlab(x)
  if (!is.null(ylab)) p <- p + ggplot2::ylab(ylab)

  p <- p + sk_theme(theme, base_size, legend_position)

  return(p)
}

# ============================================================
# 2. ADVANCED PLOTS
# ============================================================

#' QQ Plot
#'
#' Creates quantile-quantile plot for normality assessment.
#'
#' @param x Numeric vector
#' @param distribution Distribution to compare against
#' @param title Plot title
#' @param theme ggplot2 theme
#' @param base_size Base font size (default: 12)
#' @return ggplot object
#' @export
sk_qqplot <- function(x, distribution = "normal",
                      title = "Q-Q Plot", theme = "minimal", base_size = 12) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }

  # Manual QQ plot
  n <- length(x)
  theo_quantiles <- qnorm((1:n - 0.5) / n)
  samp_quantiles <- sort(x)

  p <- ggplot2::ggplot(data.frame(sample = samp_quantiles, theoretical = theo_quantiles),
                       ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    ggplot2::xlab("Theoretical Quantiles") +
    ggplot2::ylab("Sample Quantiles")

  if (!is.null(title)) p <- p + ggplot2::labs(title = title)

  p <- p + sk_theme(theme, base_size)

  return(p)
}

#' Correlation Matrix Plot
#'
#' Creates a correlation matrix heatmap.
#'
#' @param data Data frame or matrix
#' @param method Correlation method: "pearson", "spearman", "kendall"
#' @param type Plot type: "full", "lower", "upper"
#' @param show_values Logical; if TRUE, shows correlation values
#' @param digits Number of decimal places (default: 2)
#' @param title Plot title
#' @param theme ggplot2 theme
#' @param base_size Base font size (default: 12)
#' @return ggplot object
#' @export
sk_correlation_plot <- function(data, method = "pearson", type = "full",
                                show_values = TRUE, digits = 2,
                                title = NULL, theme = "minimal", base_size = 12) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }

  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("reshape2 package is required")
  }

  if (!is.matrix(data)) {
    numeric_cols <- sapply(data, is.numeric)
    if (sum(numeric_cols) < 2) {
      stop("Need at least 2 numeric columns")
    }
    data <- data[, numeric_cols]
  }

  cor_matrix <- cor(data, method = method)

  if (type == "lower") {
    cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
  } else if (type == "upper") {
    cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
  }

  cor_melt <- reshape2::melt(cor_matrix, na.rm = TRUE)

  p <- ggplot2::ggplot(cor_melt, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                                  midpoint = 0, limit = c(-1, 1)) +
    ggplot2::xlab("") + ggplot2::ylab("")

  if (show_values) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = round(value, digits)))
  }

  if (!is.null(title)) p <- p + ggplot2::labs(title = title)

  p <- p + sk_theme(theme, base_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(p)
}

#' Heatmap
#'
#' Creates a heatmap for matrix data.
#'
#' @param data Data frame or matrix
#' @param scale Scale: "row", "column", "none"
#' @param cluster_rows Logical; if TRUE, clusters rows
#' @param cluster_cols Logical; if TRUE, clusters columns
#' @param color_palette Color palette: "viridis", "magma", "plasma", "inferno"
#' @param title Plot title
#' @param theme ggplot2 theme
#' @param base_size Base font size (default: 12)
#' @return ggplot object
#' @export
sk_heatmap <- function(data, scale = "none", cluster_rows = FALSE,
                       cluster_cols = FALSE, color_palette = "viridis",
                       title = NULL, theme = "minimal", base_size = 12) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }

  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("reshape2 package is required")
  }

  if (!is.matrix(data)) {
    numeric_cols <- sapply(data, is.numeric)
    if (sum(numeric_cols) < 2) {
      stop("Need at least 2 numeric columns")
    }
    data <- as.matrix(data[, numeric_cols])
  }

  if (cluster_rows || cluster_cols) {
    if (!requireNamespace("stats", quietly = TRUE)) {
      stop("stats package is required for clustering")
    }
    if (cluster_rows) {
      row_order <- stats::hclust(stats::dist(data))$order
      data <- data[row_order, ]
    }
    if (cluster_cols) {
      col_order <- stats::hclust(stats::dist(t(data)))$order
      data <- data[, col_order]
    }
  }

  if (scale == "row") {
    data <- t(scale(t(data)))
  } else if (scale == "column") {
    data <- scale(data)
  }

  data_melt <- reshape2::melt(data)

  p <- ggplot2::ggplot(data_melt, ggplot2::aes(x = Var2, y = Var1, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::xlab("") + ggplot2::ylab("")

  if (color_palette == "viridis") {
    p <- p + ggplot2::scale_fill_viridis_c()
  } else if (color_palette == "magma") {
    p <- p + ggplot2::scale_fill_viridis_c(option = "A")
  } else if (color_palette == "plasma") {
    p <- p + ggplot2::scale_fill_viridis_c(option = "C")
  } else if (color_palette == "inferno") {
    p <- p + ggplot2::scale_fill_viridis_c(option = "B")
  }

  if (!is.null(title)) p <- p + ggplot2::labs(title = title)

  p <- p + sk_theme(theme, base_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(p)
}

#' PCA Plot
#'
#' Creates PCA biplot or scatter plot.
#'
#' @param data Data frame or matrix
#' @param x PC1 variable (default: 1)
#' @param y PC2 variable (default: 2)
#' @param color Grouping variable for colors
#' @param shape Grouping variable for shapes
#' @param scale Scale data before PCA (default: TRUE)
#' @param show_loadings Logical; if TRUE, shows variable loadings
#' @param title Plot title
#' @param theme ggplot2 theme
#' @param base_size Base font size (default: 12)
#' @return ggplot object
#' @export
sk_pca_plot <- function(data, x = 1, y = 2, color = NULL, shape = NULL,
                        scale = TRUE, show_loadings = FALSE,
                        title = NULL, theme = "minimal", base_size = 12) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }

  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("stats package is required")
  }

  if (!is.matrix(data)) {
    numeric_cols <- sapply(data, is.numeric)
    if (sum(numeric_cols) < 2) {
      stop("Need at least 2 numeric columns")
    }
    data <- data[, numeric_cols]
  }

  # Perform PCA
  pca_result <- prcomp(data, scale = scale, center = TRUE)

  # Get scores
  scores <- as.data.frame(pca_result$x)

  # Create plot
  if (!is.null(color) && !is.null(shape)) {
    p <- ggplot2::ggplot(scores, ggplot2::aes(x = .data[[paste0("PC", x)]],
                                              y = .data[[paste0("PC", y)]],
                                              color = .data[[color]],
                                              shape = .data[[shape]]))
  } else if (!is.null(color)) {
    p <- ggplot2::ggplot(scores, ggplot2::aes(x = .data[[paste0("PC", x)]],
                                              y = .data[[paste0("PC", y)]],
                                              color = .data[[color]]))
  } else if (!is.null(shape)) {
    p <- ggplot2::ggplot(scores, ggplot2::aes(x = .data[[paste0("PC", x)]],
                                              y = .data[[paste0("PC", y)]],
                                              shape = .data[[shape]]))
  } else {
    p <- ggplot2::ggplot(scores, ggplot2::aes(x = .data[[paste0("PC", x)]],
                                              y = .data[[paste0("PC", y)]]))
  }

  p <- p + ggplot2::geom_point(size = 3, alpha = 0.7)

  # Add loadings
  if (show_loadings) {
    loadings <- as.data.frame(pca_result$rotation[, c(x, y)])
    names(loadings) <- c("x", "y")
    p <- p + ggplot2::geom_segment(data = loadings,
                                   ggplot2::aes(x = 0, y = 0, xend = x * 5, yend = y * 5),
                                   arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                                   color = "red") +
      ggplot2::geom_text(data = loadings,
                         ggplot2::aes(x = x * 5.5, y = y * 5.5, label = rownames(loadings)),
                         color = "red", size = 3)
  }

  # Labels
  var_explained <- summary(pca_result)$importance[2, c(x, y)] * 100
  p <- p + ggplot2::xlab(paste0("PC", x, " (", round(var_explained[1], 1), "%)")) +
    ggplot2::ylab(paste0("PC", y, " (", round(var_explained[2], 1), "%)"))

  if (!is.null(title)) p <- p + ggplot2::labs(title = title)

  p <- p + sk_theme(theme, base_size)

  return(p)
}
