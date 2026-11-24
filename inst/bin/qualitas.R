#!/usr/bin/env Rscript
# Assess a data file from the command line.
#
#   Rscript qualitas.R data.csv [--spec spec.csv] [--rules rules.csv]
#                     [--report report.html] [--results folder]
#
# The data may be a .csv or an .rds file. Without --spec, the specification
# is inferred from the data; --report takes a .html or .docx name; --results
# writes the issues, measures and scores as CSV and JSON. The exit status is
# 1 when a critical check found a problem, so the script can stop a pipeline.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0 || args[1] %in% c("-h", "--help")) {
  cat("Usage: Rscript qualitas.R data.csv [--spec spec.csv] [--rules rules.csv]",
      "[--report report.html] [--results folder]\n")
  quit(status = 0)
}

# The value given after a flag, or NULL when the flag is absent.
flag_value <- function(flag) {
  at <- match(flag, args)
  if (is.na(at) || at == length(args)) NULL else args[at + 1]
}

path <- args[1]
data <- if (grepl("\\.rds$", path, ignore.case = TRUE)) {
  readRDS(path)
} else {
  # Only empty cells are read as missing, so that codes such as -99 or "NA"
  # stay visible to the checks of disguised missing values.
  # Column types are guessed from the data, as the specification checks them.
  readr::read_csv(path, na = "", col_types = readr::cols(.default = readr::col_guess()),
                  progress = FALSE)
}
result <- qualitas::assess(data, spec = flag_value("--spec"), rules = flag_value("--rules"),
                           name = basename(path))
print(result)
report_file <- flag_value("--report")
if (!is.null(report_file)) qualitas::report(result, report_file)
results_dir <- flag_value("--results")
if (!is.null(results_dir)) qualitas::write_results(result, results_dir)
critical <- sum(qualitas::issues(result, severity = "critical")$check_id != "")
quit(status = as.integer(critical > 0))
