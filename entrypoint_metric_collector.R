#!/usr/bin/env Rscript

library(argparse)
library(jsonlite)
library(tibble)
library(dplyr)
library(rmarkdown)

# CLI
p <- ArgumentParser("Background benchmark metrics collector")
p$add_argument("--output_dir", "-o", required = TRUE)
p$add_argument("--name", "-n", required = TRUE)
p$add_argument("--m1_dummy.summary", dest = "dummy_json", required = TRUE)
p$add_argument("--metrics.scores", nargs = "+", dest = "scores_fps", required = TRUE)
p$add_argument("--metrics.percell", nargs = "+", dest = "percell_fps", required = TRUE)
args <- p$parse_args()
dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)

dataset_from_fp <- function(fp) sub("\\..*$", "", basename(fp))

# Load dummy metric (kept as-is)
dummy <- read_json(args$dummy_json, simplifyVector = TRUE) %>%
  as_tibble() %>%
  mutate(dataset = dataset_from_fp(args$dummy_json))

# Load kNN metrics (summary) -- now from metrics.scores
knn <- bind_rows(lapply(args$scores_fps, function(fp) {
  df <- as_tibble(readRDS(fp))
  if (!"dataset" %in% names(df)) df$dataset <- args$name
  df
})) |>
  mutate(
    k       = as.integer(k),
    ndim    = as.integer(ndim),
    overlap = as.numeric(overlap)
  )

# Load per-cell metrics -- now from metrics.percell
knn_pc <- bind_rows(lapply(args$percell_fps, function(fp) {
  df <- as_tibble(readRDS(fp))
  if (!"dataset" %in% names(df)) df$dataset <- args$name
  df
})) |>
  mutate(
    k    = as.integer(k),
    frac = as.numeric(frac)
  )

# helper: where this script lives
get_script_dir <- function() {
  ca <- commandArgs(trailingOnly = FALSE)
  i <- grep("^--file=", ca)
  if (length(i) == 1) return(dirname(normalizePath(sub("^--file=", "", ca[i]))))
  normalizePath(getwd())
}

# Render report (unchanged)
rmarkdown::render(
  input = file.path(get_script_dir(), "metric_collector.Rmd"),
  output_file = "metrics_report.html",
  output_dir  = args$output_dir,
  params = list(
    dummy       = dummy,
    knn         = knn,
    knn_pc      = knn_pc,
    csv_knn_sum = NULL,
    csv_knn_pc  = NULL
  ),
  envir = new.env(parent = globalenv())
)

cat("Report written to:", file.path(args$output_dir, "metrics_report.html"), "\n")
