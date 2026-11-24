# Checks of plausibility: values far from the rest of their column or their
# group, and dates that come before the date they must follow.

#' Checks of plausibility
#'
#' Outliers are found with the modified z-score of Iglewicz and Hoaglin
#' (1993), 0.6745 (x - median) / MAD, which the median and the median
#' absolute deviation keep from being pulled by the outliers themselves.
#' Positive values with a quartile skewness (Bowley) of at least
#' `log_skew_min`, such as incomes and laboratory results, are judged on the
#' log scale when that makes their quartiles more symmetric.
#' `check_group_outliers()` applies it within each group of a column whose
#' role is `group` (a district, a facility), for groups of at least
#' `group_min_n` values, so a value usual in one place but not in another is
#' found. `check_date_order()` compares each date column with the column its
#' specification names in `after`.
#'
#' @inheritParams structure_checks
#' @return A [check_result()].
#' @name plausibility_checks
#' @examples
#' data <- data.frame(weight = c(10:20, 95))
#' check_outliers(data, infer_spec(data), qualitas_options())
NULL

#' @rdname plausibility_checks
#' @export
check_outliers <- function(data, spec, options) {
  measured <- measured_columns(spec, data)
  found <- purrr::map(measured, \(column) column_outliers(data[[column]], column,
                                                          options$outlier_z,
                                                          options$log_skew_min)) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
  check_result(found, checked = count_numbers(data, measured))
}

# The values of one column far from its median.
column_outliers <- function(x, column, limit, min_skew) {
  z <- modified_z(outlier_scale(as_number(x), min_skew))
  cell_issues(x, column, abs(z) > limit,
              sprintf("the value lies %.1f robust deviations from the median", abs(z)))
}

#' @rdname plausibility_checks
#' @export
check_group_outliers <- function(data, spec, options) {
  pairs <- tidyr::expand_grid(column = measured_columns(spec, data),
                              group = columns_with_role(spec, data, "group"))
  found <- purrr::pmap(pairs, \(column, group) group_outliers(data, column, group, options)) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
  check_result(found, checked = count_numbers(data, unique(pairs$column)))
}

# The values of one column far from the median of their group.
group_outliers <- function(data, column, group, options) {
  scored <- tibble::tibble(row = seq_len(nrow(data)), value = as_text(data[[column]]),
                           number = as_number(data[[column]]), group = as_text(data[[group]])) |>
    dplyr::group_by(.data$group) |>
    dplyr::mutate(n = sum(!is.na(.data$number)),
                  z = modified_z(outlier_scale(.data$number, options$log_skew_min))) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$n >= options$group_min_n, abs(.data$z) > options$outlier_z)
  tibble::tibble(column = rep(column, nrow(scored)), row = scored$row, value = scored$value,
                 message = sprintf("the value lies %.1f robust deviations from the median of %s %s",
                                   abs(scored$z), group, scored$group))
}

#' @rdname plausibility_checks
#' @export
check_date_order <- function(data, spec, options) {
  ordered <- dplyr::filter(spec_present(spec, data), .data$after %in% names(data))
  found <- per_column(data, ordered, \(x, row) cell_issues(
    x, row$column, as_day(x) < as_day(data[[row$after]]),
    paste0("the date is before ", row$after, ", ", as_text(data[[row$after]]))))
  check_result(found, checked = nrow(data) * nrow(ordered))
}
