#' Assess the quality of a data set
#'
#' Runs every check of the catalogue (or those named in `checks`) on `data`
#' against its specification, and gathers what they found: each problem with
#' its row, column and value, a measure for each check stated as units
#' checked, units flagged and the share that passed, and a score for each
#' dimension.
#'
#' @param data A data frame.
#' @param spec A specification (see [spec_template()]), a path to one, or
#'   `NULL` to infer it from the data with [infer_spec()].
#' @param rules Rules records must keep: a table from [read_rules()], a path
#'   to one, or `NULL`.
#' @param options Settings from [qualitas_options()].
#' @param checks Identifiers of the checks to run, such as `c("CO01",
#'   "UN02")`; `NULL` runs them all.
#' @param name A name for the data set, used in reports.
#' @return An assessment: a list of class `qualitas_assessment` with `issues`,
#'   `measures`, `scores`, `spec`, `options` and a description of the data.
#'   Read it with [issues()], [measures()], [scores()], [report()] and
#'   [cleaning_log()].
#' @export
#' @examples
#' survey <- read.csv(system.file("extdata", "messy_survey.csv", package = "qualitas"))
#' result <- assess(survey)
#' result
#' scores(result)
assess <- function(data, spec = NULL, rules = NULL, options = qualitas_options(),
                   checks = NULL, name = deparse(substitute(data))) {
  data <- tibble::as_tibble(data, .name_repair = "minimal")
  spec <- resolve_spec(spec, data)
  options$rules <- resolve_rules(rules %||% options$rules)
  catalogue <- dplyr::filter(checks_catalogue(), is.null(checks) | .data$check_id %in% checks)
  results <- purrr::map(catalogue$`function`, run_check, data = data, spec = spec,
                        options = options)
  issues <- purrr::map2(results, catalogue$check_id,
                        \(one, id) dplyr::mutate(one$issues, check_id = id, .before = 1)) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(tibble::tibble(check_id = character()), no_issues())
  new_assessment(data, spec, options, catalogue, issues, results, name)
}

# The specification to assess against: the one given, read from a path, or
# inferred from the data (with a message saying so).
resolve_spec <- function(spec, data) {
  if (is.character(spec)) return(read_spec(spec))
  if (!is.null(spec)) return(spec_complete(spec))
  cli::cli_inform("No specification given: inferring one from the data.")
  infer_spec(data)
}

# Runs one check by the name of its function, so that a check that fails
# reports its error and the others still run.
run_check <- function(fn, data, spec, options) {
  check <- get(fn, envir = asNamespace("qualitas"), mode = "function")
  tryCatch(c(check(data, spec, options), list(error = NA_character_)),
           error = \(e) c(check_result(), list(error = conditionMessage(e))))
}

# The assessment object: issues annotated from the catalogue, measures and
# dimension scores.
new_assessment <- function(data, spec, options, catalogue, issues, results, name) {
  annotated <- dplyr::left_join(issues, dplyr::select(catalogue, "check_id", "title",
                                                      "dimension", "severity"),
                                by = "check_id", relationship = "many-to-one",
                                na_matches = "never") |>
    dplyr::relocate("dimension", "severity", .after = "check_id")
  measured <- measure_checks(catalogue, annotated, results)
  structure(list(name = name, n_rows = nrow(data), n_columns = ncol(data),
                 created = Sys.time(), spec = spec, options = options,
                 issues = annotated, measures = measured,
                 scores = score_dimensions(measured)),
            class = "qualitas_assessment")
}
