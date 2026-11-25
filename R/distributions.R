#' Assess functional dependencies and grouped categorical shares
#'
#' Values are exact: callers explicitly normalise aliases before assessment.
#' Missing determinants or answers are excluded and counted. No missing value
#' is treated as an agreeing answer. Single-record dependencies are untested.
#' @param data Data frame with unique names and atomic columns.
#' @param determinants Character vector of determinant columns (a tuple).
#' @param dependent One dependent column name.
#' @param group One group column name; character, factor or logical.
#' @param column One categorical column name; character, factor or logical.
#' @param min_n Minimum nonmissing answers in both group and rest (integer).
#' @param difference Inclusive absolute share difference from 0 to 1.
#' @param tolerance Nonnegative numerical comparison tolerance, at most 1e-6.
#' @param max_cells Maximum number of group-by-category output cells.
#' @return A list with status, excluded count and a results tibble. Dependency
#'   results contain determinant values, n, distinct answers and flagged;
#'   share results contain group/category values, both counts, denominators,
#'   shares, absolute difference, eligibility, threshold and tolerance.
#'   These summaries include category and determinant values; avoid personal fields.
#' @name assess_dependencies
#' @export
assess_dependencies <- function(data, determinants, dependent) {
  advanced_frame(data, c(determinants, dependent))
  if (length(dependent) != 1L || !length(determinants)) stop("Supply determinants and one dependent.")
  good <- !advanced_missing(data[c(determinants, dependent)])
  x <- data[good, c(determinants, dependent), drop = FALSE]
  n <- utils::tail(make.unique(c(names(x), "n")), 1)
  distinct <- utils::tail(make.unique(c(names(x), n, "distinct")), 1)
  result <- x |> dplyr::group_by(dplyr::across(dplyr::all_of(determinants))) |>
    dplyr::summarise(!!n := dplyr::n(), !!distinct := dplyr::n_distinct(.data[[dependent]]), .groups = "drop")
  names(result)[match(c(n, distinct), names(result))] <- c("n", "distinct")
  if (any(determinants %in% c("n", "distinct", "eligible", "flagged"))) stop("Reserved output name in determinants.")
  result$eligible <- result$n >= 2L
  result$flagged <- ifelse(result$eligible, result$distinct > 1L, NA)
  list(status = if (any(result$eligible)) "assessed" else "not_applicable",
       excluded = sum(!good), results = result)
}
#' @rdname assess_dependencies
#' @export
assess_distributions <- function(data, group, column, min_n = 30L,
                                 difference = 0.3, tolerance = 1e-12, max_cells = 1e5) {
  advanced_frame(data, c(group, column))
  if (length(group) != 1L || length(column) != 1L ||
      !all(vapply(data[c(group, column)], function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1)))) stop("Use distinct categorical group and answer columns.")
  scalar <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (!scalar(min_n) || min_n < 1 || min_n != floor(min_n) ||
      !scalar(difference) || difference < 0 || difference > 1 ||
      !scalar(tolerance) || tolerance < 0 || tolerance > 1e-6 ||
      !scalar(max_cells) || max_cells < 1) stop("Invalid denominator, difference or tolerance.")
  cutoff <- difference
  good <- !advanced_missing(data[c(group, column)])
  g <- data[[group]][good]; v <- data[[column]][good]
  if (length(unique(g)) * as.double(length(unique(v))) > max_cells) stop("Grouped comparison exceeds max_cells.")
  results <- tibble::tibble(group = g[0], category = v[0], count_group = integer(),
    count_rest = integer(), n_group = integer(), n_rest = integer(), share_group = double(),
    share_rest = double(), difference = double(), eligible = logical(), flagged = logical(),
    threshold = double(), tolerance = double())
  for (label in unique(g)) for (category in unique(v)) {
    inside <- g == label; n1 <- sum(inside); n0 <- sum(!inside)
    k1 <- sum(v[inside] == category); k0 <- sum(v[!inside] == category)
    delta <- if (n1 && n0) abs(k1 / n1 - k0 / n0) else NA_real_
    eligible <- n1 >= min_n && n0 >= min_n
    results <- dplyr::bind_rows(results, tibble::tibble(group = label, category = category,
      count_group = k1, count_rest = k0, n_group = n1, n_rest = n0,
      share_group = k1 / n1, share_rest = if (n0) k0 / n0 else NA_real_, difference = delta,
      eligible = eligible, flagged = if (eligible) delta + tolerance >= cutoff else NA,
      threshold = cutoff, tolerance = tolerance))
  }
  list(status = if (any(results$eligible)) "assessed" else "not_applicable",
       excluded = sum(!good), min_n = min_n, results = results)
}
