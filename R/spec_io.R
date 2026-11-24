#' An empty data specification
#'
#' The columns of a specification, one row per data column:
#' * `column`: the column name.
#' * `type`: `integer`, `number`, `date`, `datetime`, `logical`, `category`
#'   or `text`.
#' * `role`: `id`, `date`, `enumerator`, `group`, `age`, `duration`,
#'   `amount` (a sum of money or another quantity Benford's law applies to)
#'   or `""`.
#' * `required`: whether every record must have a value.
#' * `unique`: whether every value must be different (a key).
#' * `min`, `max`: the allowed range, for numbers and dates.
#' * `allowed`: the allowed values, separated by `;`.
#' * `pattern`: a regular expression every value must match.
#' * `missing_codes`: values that stand for missing in this column,
#'   separated by `;`, added to the general list.
#' * `description`: what the column holds.
#' * `after`: a date column this date must not precede.
#'
#' @return A tibble with no rows and the columns above.
#' @export
#' @examples
#' spec_template()
spec_template <- function() {
  tibble::tibble(column = character(), type = character(), role = character(),
                 required = logical(), unique = logical(), min = character(),
                 max = character(), allowed = character(), pattern = character(),
                 missing_codes = character(), description = character(),
                 after = character())
}

#' Complete a partial specification
#'
#' Adds the columns of [spec_template()] that a specification leaves out, in
#' order, reads empty cells as missing, sets `required` and `unique` to
#' `FALSE` where they are not given, and keeps every other field as text. A
#' specification can therefore be written with only the columns it needs.
#'
#' @param spec A data frame with at least a `column` column.
#' @return A specification with every column of [spec_template()].
#' @export
#' @examples
#' spec_complete(data.frame(column = "age", type = "integer", min = 0, max = 120))
spec_complete <- function(spec) {
  dplyr::bind_rows(spec_template(), tibble::as_tibble(spec) |>
                     dplyr::mutate(dplyr::across(dplyr::any_of(c("required", "unique")),
                                                 as.logical),
                                   dplyr::across(dplyr::any_of(c("min", "max")),
                                                 as.character))) |>
    dplyr::select(dplyr::all_of(names(spec_template()))) |>
    dplyr::mutate(required = dplyr::coalesce(.data$required, FALSE),
                  unique = dplyr::coalesce(.data$unique, FALSE),
                  role = dplyr::coalesce(.data$role, ""),
                  type = dplyr::coalesce(.data$type, "text"),
                  dplyr::across(c("allowed", "pattern", "missing_codes", "after",
                                  "min", "max", "description"),
                                \(x) dplyr::na_if(trimws(x), "")))
}

#' Read or write a data specification
#'
#' A specification is kept as a CSV file (one row per column, the columns of
#' [spec_template()]) or a YAML file (a list of columns, each with the same
#' fields), so it can be edited in a spreadsheet and kept under version
#' control beside the data.
#'
#' @param path Path to a `.csv`, `.yaml` or `.yml` file.
#' @param spec A specification.
#' @return `read_spec()` returns the specification; `write_spec()` returns
#'   `path`, invisibly.
#' @export
#' @examples
#' path <- tempfile(fileext = ".csv")
#' write_spec(infer_spec(mtcars), path)
#' read_spec(path)
read_spec <- function(path) {
  yaml_file <- grepl("\\.ya?ml$", path, ignore.case = TRUE)
  rows <- if (yaml_file) dplyr::bind_rows(yaml_columns(path)) else
    readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                    progress = FALSE)
  spec_complete(rows)
}

# The columns of a YAML specification, each as a one-row list of text.
yaml_columns <- function(path) {
  rlang::check_installed("yaml", reason = "to read a YAML specification")
  purrr::map(yaml::read_yaml(path)$columns, \(one) purrr::map(one, as.character))
}

#' @rdname read_spec
#' @export
write_spec <- function(spec, path) {
  readr::write_csv(spec_complete(spec), path, na = "")
  invisible(path)
}
