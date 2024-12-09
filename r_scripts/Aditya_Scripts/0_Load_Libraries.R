# 1_Load_Libraries.R

# Data Reading & Manipulation
library(tidyr)
library(dplyr)
library(data.table)
library(lubridate)
library(tidyverse)
library(stringr)

# Data Visualization
library(viridis)  # For better color palettes
library(hrbrthemes)  # For theme_ipsum()
library(plotly)  # For interactive plots
library(gtsummary)  # For creating summary tables

# Check versions of critical packages
required_versions <- list(
  tidyverse = "1.3.0",
  data.table = "1.14.0",
  lubridate = "1.7.10",
  stringr = "1.4.0",
  viridis = "0.6.1",
  hrbrthemes = "0.8.0",
  plotly = "4.9.3",
  gtsummary = "1.4.2"
)

for (pkg in names(required_versions)) {
  if (packageVersion(pkg) < required_versions[[pkg]]) {
    warning(paste("Package", pkg, "is older than the required version", required_versions[[pkg]]))
  }
}
