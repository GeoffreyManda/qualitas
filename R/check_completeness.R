# Checks of completeness: missing cells, missing required values, codes that
# stand in for a missing value, and records missing most of their values.

#' Checks of completeness
#'
#' @inheritParams structure_checks
#' @return A [check_result()].
#' @name completeness_checks
#' @examples
#' data <- data.frame(id = c(1, NA, 3), age = c("31", "-99", ""))
#' check_disguised_missing(data, infer_spec(data), qualitas_options())
NULL

#' @rdname completeness_checks
#' @export
check_missing_cells <- function(data, spec, options) {
  found <- purrr::imap(data, \(x, column) cell_issues(x, column, is_blank(x),
                                                       "the value is missing")) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
  check_result(found, checked = nrow(data) * ncol(data))
}

#' @rdname completeness_checks
#' @export
check_required_missing <- function(data, spec, options) {
  required <- dplyr::filter(spec_present(spec, data), .data$required)
  found <- per_column(data, required, \(x, row) cell_issues(
    x, row$column, is_blank(x), "a value is required but missing"))
  check_result(found, checked = nrow(data) * nrow(required))
}

#' @rdname completeness_checks
#' @export
check_disguised_missing <- function(data, spec, options) {
  found <- per_column(data, spec, \(x, row) disguised_cells(x, row, options$missing_codes,
                                                             options$outlier_z))
  check_result(found, checked = sum(purrr::map_int(data, \(x) sum(!is.na(x)))))
}

# The cells of one column that hold a missing-value code: one of the
# general codes or the column's own, unless the code is an allowed value.
disguised_cells <- function(x, row, general, outlier_limit) {
  codes <- setdiff(c(general, split_list(row$missing_codes)), split_list(row$allowed))
  numeric_column <- row$type %in% c("integer", "number")
  codes <- if (numeric_column) codes[!is.na(as_number(codes)) | !grepl("\\d", codes)]
           else codes
  text <- trimws(as_text(x))
  coded <- !is.na(text) & text %in% codes
  # In a numeric column a number such as -99 counts as a code only where it
  # lies far from the column's other values: -99 is a code among ages, and a
  # longitude in the Gulf of Mexico.
  number <- as_number(x)
  plausible <- numeric_column & !is.na(number) &
    !far_from(number, number[!coded], outlier_limit) %in% TRUE
  cell_issues(x, row$column, coded & !plausible,
              "the value is a code that stands for a missing value")
}

#' @rdname completeness_checks
#' @export
check_record_completeness <- function(data, spec, options) {
  share <- rowMeans(as.data.frame(purrr::map(data, is_blank)))
  over <- which(share > options$record_missing_share)
  check_result(tibble::tibble(row = over, value = sprintf("%.0f%%", 100 * share[over]),
                              message = sprintf("the record is missing %.0f%% of its values",
                                                100 * share[over])),
               checked = nrow(data))
}
