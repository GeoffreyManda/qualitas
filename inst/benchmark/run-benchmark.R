#!/usr/bin/env Rscript
# Assesses a random sample of real data sets from the Rdatasets archive
# (https://vincentarelbundock.github.io/Rdatasets), one data set per source
# package so the sample is not dominated by one author's data, and writes one
# row per data set and one row per data set and check.
#
#   Rscript inst/benchmark/run-benchmark.R [n] [seed] [folder]
#
# Defaults: 40 data sets, seed 20261124, and a folder in the user's R cache.
# Data sets are read with only empty cells as missing, without a
# specification, as a new user would first run the package.

given <- commandArgs(trailingOnly = TRUE)
n <- as.integer(dplyr::coalesce(given[1], "40"))
seed <- as.integer(dplyr::coalesce(given[2], "20261124"))
folder <- dplyr::coalesce(given[3], tools::R_user_dir("qualitas", "cache"))
dir.create(file.path(folder, "data"), recursive = TRUE, showWarnings = FALSE)
index_url <- "https://vincentarelbundock.github.io/Rdatasets/datasets.csv"

# The archive's index, restricted to data sets of a size worth assessing.
index <- readr::read_csv(index_url, col_types = readr::cols(.default = readr::col_guess()),
                         progress = FALSE) |>
  dplyr::filter(.data$Rows >= 100, .data$Rows <= 200000, .data$Cols >= 4, .data$Cols <= 80)

# One data set from each of n packages drawn at random.
withr::with_seed(seed, {
  chosen <- index |>
    dplyr::slice_sample(n = 1, by = "Package") |>
    dplyr::slice_sample(n = n)
})

# Downloads one data set, assesses it and keeps what happened; a data set
# that cannot be read or assessed is recorded with its error.
assess_one <- function(package, item, url) {
  path <- file.path(folder, "data", paste0(package, "-", item, ".csv"))
  started <- Sys.time()
  outcome <- tryCatch({
    if (!file.exists(path)) utils::download.file(url, path, quiet = TRUE, mode = "wb")
    data <- readr::read_csv(path, na = "", progress = FALSE, name_repair = "minimal",
                            col_types = readr::cols(.default = readr::col_guess()))
    data <- data[, names(data) != "rownames", drop = FALSE]
    list(result = suppressMessages(qualitas::assess(data, name = item)), error = NA_character_)
  }, error = \(e) list(result = NULL, error = conditionMessage(e)))
  list(package = package, item = item, url = url, outcome = outcome,
       seconds = as.numeric(difftime(Sys.time(), started, units = "secs")))
}

runs <- purrr::pmap(list(chosen$Package, chosen$Item, chosen$CSV), assess_one,
                    .progress = TRUE)

# One row per data set.
overview <- purrr::map(runs, \(run) tibble::tibble(
  package = run$package, item = run$item, url = run$url,
  rows = run$outcome$result$n_rows %||% NA_integer_,
  columns = run$outcome$result$n_columns %||% NA_integer_,
  seconds = round(run$seconds, 2),
  issues = nrow(run$outcome$result$issues %||% tibble::tibble()),
  overall = qualitas::overall_score(run$outcome$result %||% list(scores = tibble::tibble())),
  check_errors = sum(run$outcome$result$measures$status %in% "error"),
  failure = run$outcome$error)) |>
  purrr::list_rbind()

# One row per data set and check that ran.
by_check <- purrr::map(purrr::keep(runs, \(run) !is.null(run$outcome$result)), \(run)
  dplyr::mutate(run$outcome$result$measures, package = run$package, item = run$item,
                .before = 1)) |>
  purrr::list_rbind() |>
  dplyr::select("package", "item", "check_id", "unit", "checked", "flagged", "score",
                "status", "error")

readr::write_csv(overview, file.path(folder, "benchmark-datasets.csv"), na = "")
readr::write_csv(by_check, file.path(folder, "benchmark-checks.csv"), na = "")
cat("Assessed", sum(is.na(overview$failure)), "of", nrow(overview), "data sets;",
    sum(overview$check_errors, na.rm = TRUE), "check errors. Results in", folder, "\n")
