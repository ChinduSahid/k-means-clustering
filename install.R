# Packages used by this repository.
# Run once:  Rscript install.R

pkgs <- c(
  "VIM", "cluster", "clustertend", "data.table",
  "eeptools", "factoextra", "tidyverse"
)

missing <- pkgs[!(pkgs %in% rownames(installed.packages()))]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
