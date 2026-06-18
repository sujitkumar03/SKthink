# SKthink 📊

<div align="center">

**Statistical Thinking Tools for Data Analysis in R**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-%3E%3D4.0.0-blue.svg)](https://www.r-project.org/)
[![GitHub](https://img.shields.io/badge/GitHub-SKthink-green.svg)](https://github.com/sujitkumar03/SKthink)

</div>

---

## 🌟 Overview

**SKthink** is an R package designed to streamline statistical analysis, data visualization, and automated session logging. It provides a comprehensive toolkit for researchers, data scientists, and statisticians who need efficient tools for exploratory data analysis, hypothesis testing, and reproducible research.

### 🎯 Key Features

- 📝 **Automated Session Logging** - Capture code, output, and figures automatically
- 📈 **Advanced Visualization** - Enhanced plotting functions with smart defaults
- 📊 **Statistical Analysis** - Comprehensive statistical testing tools
- 🧪 **Hypothesis Testing** - Easy-to-use statistical test functions
- 🖥️ **System Information** - Auto-capture environment details for reproducibility
- 📄 **Multiple Export Formats** - HTML, Markdown, TXT, Clipboard support

---

## 📦 Installation

### Install from GitHub (Development Version)


#### Option 1: Using pak (Fastest)
```r
install.packages("pak")
pak::pkg_install("sujitkumar03/SKthink")
```
#### Option 2: Using devtools
```r
# Install devtools if not already installed
install.packages("devtools")

# Install SKthink from GitHub
devtools::install_github( "sujitkumar03/SKthink", build_vignettes = TRUE, dependencies = TRUE )

# Load the package
library(SKthink)

```
---

## 🚀 Quick Start

### 1. Session Logging

```r
library(SKthink)
library(ggplot2)

# 1. Start logging session
sknote(format = "html", file_path = "my_analysis.html", save_plots = TRUE)

# 2. Descriptive Statistics
summary(mtcars)
cat("Mean MPG:", mean(mtcars$mpg), "\n")

# 3. ggplot2 Visualization (Automatically captured by sknote)
p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Weight vs MPG by Cylinder",
    x = "Weight (1000 lbs)",
    y = "Miles Per Gallon",
    color = "Cylinders"
  ) +
  theme_minimal(base_size = 14)

print(p)

# 4. Stop logging and finalize report
sknote_stop()

# 5. Open the generated HTML report
browseURL("my_analysis.html")
```

### 2. Data Cleaning

```r
data(airquality)

sk_missing(airquality)

airquality_clean <- sk_impute(
  airquality,
  method = "median"
)
```

### 3. Descriptive Statistics

```r
sk_stat(mtcars)
```

### 4. Statistical Analysis

```r
sk_shapiro_test(mtcars$mpg)

sk_t_test_two(
  mtcars$mpg[mtcars$am == 0],
  mtcars$mpg[mtcars$am == 1]
)
```

### 5. Data Visualization

```r
sk_scatter(
  data = mtcars,
  x = "wt",
  y = "mpg",
  add_regression = TRUE
)
```

---

## 📖 Documentation

Browse all package tutorials:
Note: Please install the package using devtools before accessing the tutorials.

```r
browseVignettes("SKthink")
```

Open the main vignette:

```r
vignette(
  "skthink-tutorial",
  package = "SKthink"
)
```

---

## 📚 Citation

If you use SKthink in research, reports, or publications, please cite the package.

```r
citation("SKthink")
```

---

## 👤 Author

**Sujit Kumar**

📧 sujit.kumar.stat@gmail.com

🔗 GitHub: https://github.com/sujitkumar03/SKthink

---

## 🤝 Contributing

Contributions, feature requests, and bug reports are welcome.

Please open an issue or submit a pull request through GitHub.

---

## 📄 License

This project is released under the MIT License.

---

<div align="center">

### Built for Reproducible Statistical Analysis in R

⭐ Star the repository if you find SKthink useful.
<br>
© 2026 Sujit Kumar. All rights reserved.
</div>

