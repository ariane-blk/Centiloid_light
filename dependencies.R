# dependencies.R  -- for local use, ignored on Connect Cloud
if (Sys.getenv("RSTUDIO_CONNECT") != "1") {
  
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  
  if (!requireNamespace("renv", quietly = TRUE))
    install.packages("renv")
  
  # initialise or restore your project library
  if (!file.exists("renv.lock"))
    renv::init(bare = TRUE)
  else
    renv::restore(prompt = FALSE)
  
  required_packages <- c(
    "bslib", "bsicons", "dplyr", "ggplot2", "gtools",
    "ggpmisc", "markdown", "oro.nifti", "readr",
    "lme4", "shiny", "shinyWidgets"
  )
  
  install.packages(setdiff(required_packages,
                           rownames(installed.packages())))
  
  renv::snapshot(prompt = FALSE)
}
