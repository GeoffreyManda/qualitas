#' Compare two deliveries of the same data
#'
#' Data that arrive in rounds (monthly reports, survey waves, new extracts of
#' a register) can change in ways no single assessment shows: a column is
#' dropped, a code list changes, missing values rise, a mean moves. For each
#' column, `compare_data()` reports whether it was added, removed or changed
#' type, the change in its share of missing values, and a measure of how far
#' its values moved: for numbers, the difference of the medians in units of
#' the earlier median absolute deviation; for categories, the population
#' stability index over the categories,
#' sum((new - old) x log(new / old)), which credit-risk practice reads as
#' stable below 0.1 and as a marked shift above 0.25.
#'
#' @param old,new The earlier and the later data.
#' @param options Settings from [qualitas_options()]; `drift_missing`,
#'   `drift_shift` and `drift_psi` set when a column is flagged.
#' @return A tibble with one row per column: `column`, `status`, `type_old`,
#'   `type_new`, `missing_old`, `missing_new`, `shift`, `measure` and
#'   `flagged`.
#' @export
#' @examples
#' old <- data.frame(age = c(20:40), sex = rep(c("f", "m", "f"), 7))
#' new <- data.frame(age = c(40:60), sex = rep("f", 21), region = "north")
#' compare_data(old, new)
compare_data <- function(old, new, options = qualitas_options()) {
  columns <- union(names(old), names(new))
  purrr::map(columns, \(column) column_drift(old[[column]], new[[column]], column)) |>
    purrr::list_rbind() |>
    dplyr::mutate(flagged = .data$status != "kept" |
                    dplyr::coalesce(abs(.data$missing_new - .data$missing_old) >
                                      options$drift_missing, FALSE) |
                    dplyr::coalesce(.data$measure == "median shift" &
                                      abs(.data$shift) > options$drift_shift, FALSE) |
                    dplyr::coalesce(.data$measure == "stability index" &
                                      .data$shift > options$drift_psi, FALSE))
}

# The drift of one column between the two deliveries.
column_drift <- function(old, new, column) {
  status <- dplyr::case_when(is.null(old) ~ "added", is.null(new) ~ "removed",
                             infer_type(old) != infer_type(new) ~ "type changed",
                             .default = "kept")
  both <- !is.null(old) && !is.null(new)
  numeric <- both && infer_type(old) %in% c("integer", "number")
  measure <- c(NA_character_, "stability index", "median shift")[1 + both + numeric]
  shift <- switch(measure %|NA|% "none", "median shift" = median_shift(old, new),
                  "stability index" = stability_index(old, new), none = NA_real_)
  tibble::tibble(column = column, status = status,
                 type_old = if (is.null(old)) NA_character_ else infer_type(old),
                 type_new = if (is.null(new)) NA_character_ else infer_type(new),
                 missing_old = if (is.null(old)) NA_real_ else mean(is_blank(old)),
                 missing_new = if (is.null(new)) NA_real_ else mean(is_blank(new)),
                 shift = shift, measure = measure)
}

# The move of the median, in units of the earlier median absolute deviation.
median_shift <- function(old, new) {
  spread <- stats::mad(as_number(old), na.rm = TRUE)
  if (is.na(spread) || spread == 0) return(NA_real_)
  (stats::median(as_number(new), na.rm = TRUE) - stats::median(as_number(old), na.rm = TRUE)) /
    spread
}

# The population stability index over the categories of both deliveries; a
# small share stands in for a category one delivery does not hold.
stability_index <- function(old, new) {
  levels <- union(as_text(old[!is_blank(old)]), as_text(new[!is_blank(new)]))
  if (length(levels) == 0 || length(levels) > 50) return(NA_real_)
  before <- category_shares(old, levels)
  after <- category_shares(new, levels)
  sum((after - before) * log(after / before))
}

# The share of a column's filled values in each category, at least 1e-4.
category_shares <- function(x, levels) {
  pmax(as.numeric(table(factor(as_text(x), levels = levels))) / sum(!is_blank(x)), 1e-4)
}
