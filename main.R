#!/usr/bin/env Rscript
options(warn = 1)

args <- commandArgs(trailingOnly = TRUE)
cat("All given args\n")
print(args)

DIR_res <- "res"
dir.create(DIR_res)

if (!require("pacman")) {
    install.packages("pacman")
}

pacman::p_install("sessioninfo", force = FALSE)
pacman::p_install("farff", force = FALSE)
pacman::p_install("pacman", force = FALSE)
pacman::p_install("tibble", force = FALSE)
pacman::p_install("DataExplorer", force = FALSE)
pacman::p_install("dplyr", force = FALSE)
pacman::p_install("ggplot2", force = FALSE)
pacman::p_install("ggpubr", force = FALSE)
pacman::p_install("scales", force = FALSE)
pacman::p_install("qs", force = FALSE)
pacman::p_install("caret", force = FALSE)
pacman::p_install("data.table", force = FALSE)
pacman::p_install("mlr3", force = FALSE)
pacman::p_install("mlr3learners", force = FALSE)
pacman::p_install("mlr3tuning", force = FALSE)
pacman::p_install("pROC", force = FALSE)
pacman::p_install("gamlss", force = FALSE)
pacman::p_install("gamlss.dist", force = FALSE)
pacman::p_install("hexbin", force = FALSE)
pacman::p_install("stats", force = FALSE)


# If you are developing locally, you can manually set DIR_raw_data
# to the place where the data is stored. The gitlab-CI will take care of DIR_raw_data, that is ONLY
# For delevoping!
if (all(is.na(args))) {
    cat("DEVELOPING MODE! Extracting the paths from .gitlab-ci.yml")
    gitlab_ci <- yaml::read_yaml(".gitlab-ci.yml")
    only_paths <- gitlab_ci[["variables"]][["multiple_data_dirs"]]
    paths_separated <- strsplit(only_paths, "(\\n)|[ ]+")[[1]]
    paths_separated <- paths_separated[sapply(paths_separated, nchar) != 0]
    paths_separated_CI_interpreted <- paste0("/data/shared/", paths_separated)
    cat("", paste0("   ", paths_separated), "", sep = "\n")
} else {
    paths_separated_CI_interpreted <- args
}

cat("\n\nReading data from\n")
for (arg_x in paths_separated_CI_interpreted) {
    cat("  ", arg_x, "\n")
}
cat("\n\n")

DIR_raw_data <- paths_separated_CI_interpreted[1]
# DIR_raw_data_2 <- paths_separated_CI_interpreted[2]

source("src/01_download.R")
source("src/02_read_files.R")
source("src/03_histograms_freq.R")
source("src/04_join_claims.R")
source("src/05_histograms_insurance.R")
source("src/06_exploratory_clean.R")
source("src/07_rf_binary.R")
source("src/08_rf_continuous.R")
source("src/09_classical.R")


sessioninfo::session_info(to_file = "res/session_info.txt")
