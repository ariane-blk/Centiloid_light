# MODERN DEPENDENCY MANAGEMENT --------------------------------------------

#library(shiny)
options(repos = c(CRAN = "https://cloud.r-project.org"))

library(pandoc)

# Install renv if needed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Initialize renv project (once per project)
if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE)
}

# Restore package versions from renv.lock if it exists
# (won’t do anything if you're running for the first time)
renv::restore(prompt = FALSE)

# List of required packages
required_packages <- c(
  "bslib", 
  "bsicons",
  "dplyr",
  "ggplot2", 
  "gtools",
  "ggpmisc",
  "markdown",
  "oro.nifti",
  "pandoc",
  "readr",
  "lme4",
  "shiny", 
  "shinyWidgets"
  )

# Install any that are missing
new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) {
  install.packages(new.packages)
}

# Load packages
lapply(required_packages, require, character.only = TRUE)

# Snapshot current environment
renv::snapshot(prompt = FALSE)

