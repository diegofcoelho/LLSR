#!/usr/bin/env Rscript
# Run package tests. Installs dependencies from DESCRIPTION if needed.
# Usage: Rscript run_tests.R   or   R -e "source('run_tests.R')"
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}
remotes::install_deps(dependencies = TRUE)
testthat::test_local(".")
