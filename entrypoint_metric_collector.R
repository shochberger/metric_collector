#!/usr/bin/env Rscript

library(argparse)
library(jsonlite)
library(tibble)
library(dplyr)
library(readr)
library(rmarkdown)

# CLI
p <- ArgumentParser("Simple SoupX metrics collector")
p$add_argument("--output_dir", "-o", required = TRUE)
p$add_argument("--name", "-n", required = TRUE)
p$add_argument("--m1_dummy.summary", dest = "dummy_json", required = TRUE)
p$add_argument("--metrics_knn.summary", nargs = "+", dest = "knn_summary", help = "kNN summary .rds files (e.g. k-15 k-30 k-45)", required = TRUE)
p$add_argument("--metrics_knn.percell", nargs = "+", dest = "knn_percell", help = "kNN per-cell .rds files (one per k)", required = TRUE)
#p$add_argument("--perf_root", required = TRUE, help = "Root directory to search for performance files")
args <- p$parse_args()
dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)

# Load dummy metric
dummy <- read_json(args$dummy_json, simplifyVector = TRUE) %>%
  as_tibble() %>%
  mutate(dataset = args$name)

# Load kNN metrics  (args$knn_summary: vector of FILE paths)
knn <- bind_rows(lapply(args$knn_summary, function(fp) {
  stopifnot(file.exists(fp))
  df <- readRDS(fp)
  k_dir <- basename(dirname(fp))                         # "k-15"
  df$k <- suppressWarnings(as.integer(sub("^k-", "", k_dir)))
  df$dataset <- args$name
  df
}))

# Load per-cell metrics  (args$knn_percell: vector of FILE paths)
knn_pc <- bind_rows(lapply(args$knn_percell, function(fp) {
  stopifnot(file.exists(fp))
  df <- readRDS(fp)
  k_dir <- basename(dirname(fp))                         # "k-15"
  df$k <- suppressWarnings(as.integer(sub("^k-", "", k_dir)))
  df$dataset <- args$name
  df
}))

# helper: where this script lives
get_script_dir <- function() {
  ca <- commandArgs(trailingOnly = FALSE)
  i <- grep ("^--file=", ca)
  if (length(i) == 1) return(dirname(normalizePath(sub("^--file=", "", ca[i]))))
  normalizePath(getwd())
}

# Write CSVs
csv_knn_sum <- file.path(args$output_dir, paste0(args$name, "metrics_knn_summary.csv"))
csv_knn_pc  <- file.path(args$output_dir, paste0(args$name, "metrics_knn_per_cell.csv"))
readr::write_csv(knn, csv_knn_sum)
readr::write_csv(knn_pc, csv_knn_pc)

# Render report
rmarkdown::render(
input = file.path(get_script_dir(), "metric_collector.Rmd"),
output_file = paste0(args$name, "metrics_report.html"),
output_dir = args$output_dir,
params = list(
  dataset_name = args$name,
  dummy = dummy,
  knn = knn,
  knn_pc = knn_pc,
  csv_knn_sum = normalizePath(csv_knn_sum),
  csv_knn_pc = normalizePath(csv_knn_pc)
),
envir = new.env(parent = globalenv())
)

cat("Report written to:", file.path(args$output_dir, paste0(args$name, "metrics_report.html")), "\n")
