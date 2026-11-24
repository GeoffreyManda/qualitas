#' Data quality assessment with specifications and reviewable findings
#'
#' `qualitas` assesses rectangular data against a specification and optional
#' record-level rules. Results retain findings, denominators, measures and
#' dimension scores. A cleaning log records review decisions, and reassessment
#' evaluates the resulting snapshot.
#'
#' @section Start an assessment:
#' Use [assess()] with a data frame. Review a specification created by
#' [infer_spec()], or provide one through [spec_complete()] and [read_spec()].
#' Read results with [issues()], [measures()] and [scores()].
#'
#' @section Review and report:
#' [cleaning_log()] and [apply_log()] record and apply decisions.
#' [compare_data()] compares deliveries; [assess_tables()] assesses linked
#' tables. [report()] and [write_results()] export results.
#'
#' @section Worked guide:
#' Run `vignette("qualitas", package = "qualitas")` for a complete synthetic
#' workflow. The bundled CSV survey and its defects are generated test data.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data
NULL
