#!/usr/bin/env Rscript

# Background correction metrics report
# Inputs: dummy (JSON->tibble), knn (scores.rds bind), knn_pc (percell.rds bind).
# Renders Rmd file that shows dummy summary table, kNN summary table, and per-cell overlap boxplot by k.

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

#Directory of where script lives (so Rmd sits beside it)
get_script_dir <- function() {
  ca <- commandArgs(trailingOnly = FALSE)
  i <- grep("^--file=", ca)
  if (length(i) == 1) return(dirname(normalizePath(sub("^--file=", "", ca[i]))))
  normalizePath(getwd())
}

# Load dummy metric
dummy <- read_json(args$dummy_json, simplifyVector = TRUE) %>%
  as_tibble() %>%
  mutate(dataset = dataset_from_fp(args$name))

# Load kNN summary (scores)
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

# Load kNN per-cell
knn_pc <- bind_rows(lapply(args$percell_fps, function(fp) {
  df <- as_tibble(readRDS(fp))
  if (!"dataset" %in% names(df)) df$dataset <- args$name
  df
})) |>
  mutate(
    k    = as.integer(k),
    frac = as.numeric(frac)
  )

# Render report
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
