# Checks of consistency: rules records must keep, labels written several
# ways, stray spaces and columns that mix numbers with text.

#' Checks of consistency
#'
#' `check_rules()` applies the rules given to [assess()] (see
#' [read_rules()]). `check_label_variants()` finds values of a text or
#' category column that become the same label once case, spaces and
#' punctuation are set aside ("Male", "male ", "MALE"); every variant other
#' than the most frequent is flagged. `check_mixed_types()` flags the text
#' values of a column in which at least `mixed_type_share` of the filled
#' values are numbers.
#'
#' @inheritParams structure_checks
#' @return A [check_result()].
#' @name consistency_checks
#' @examples
#' data <- data.frame(sex = c("female", "Female ", "male", "male"))
#' check_label_variants(data, infer_spec(data), qualitas_options())
NULL

#' @rdname consistency_checks
#' @export
check_label_variants <- function(data, spec, options) {
  labelled <- dplyr::filter(spec_present(spec, data), .data$type %in% c("category", "text"),
                            !.data$role %in% "id")
  found <- per_column(data, labelled, variant_cells)
  check_result(found,
               checked = sum(purrr::map_int(labelled$column, \(c) sum(!is_blank(data[[c]])))))
}

# The cells of one column whose label is a less frequent spelling of another
# value in the same column.
variant_cells <- function(x, row) {
  text <- as_text(x)
  key <- stringr::str_to_lower(stringr::str_remove_all(dplyr::coalesce(text, ""),
                                                       "[[:punct:][:space:]]"))
  spellings <- tibble::tibble(text, key) |>
    dplyr::filter(!is_blank(.data$text)) |>
    dplyr::count(.data$key, .data$text) |>
    dplyr::group_by(.data$key) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::mutate(usual = .data$text[order(-.data$n, .data$text != trimws(.data$text))][1]) |>
    dplyr::ungroup()
  usual <- spellings$usual[match(paste(key, text), paste(spellings$key, spellings$text))]
  cell_issues(x, row$column, !is.na(usual) & text != usual,
              paste0("the label is written differently elsewhere, as \"", usual, "\""))
}

#' @rdname consistency_checks
#' @export
check_whitespace <- function(data, spec, options) {
  texts <- names(data)[purrr::map_lgl(data, \(x) is.character(x) || is.factor(x))]
  found <- purrr::map(texts, \(column) cell_issues(
    data[[column]], column, stray_spaces(as_text(data[[column]])),
    "the value has spaces at its ends or repeated spaces")) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
  check_result(found, checked = sum(purrr::map_int(texts, \(c) sum(!is.na(data[[c]])))))
}

# Whether each text value has spaces at its ends or two spaces in a row.
stray_spaces <- function(text) !is.na(text) & (text != trimws(text) | grepl("\\s{2,}", text))

#' @rdname consistency_checks
#' @export
check_mixed_types <- function(data, spec, options) {
  texts <- dplyr::filter(spec_present(spec, data), .data$type %in% c("category", "text"))
  found <- per_column(data, texts, \(x, row) mixed_cells(x, row, options$mixed_type_share))
  check_result(found,
               checked = sum(purrr::map_int(texts$column, \(c) sum(!is_blank(data[[c]])))))
}

# The text values of a column in which at least the given share of filled
# values read as numbers.
mixed_cells <- function(x, row, share) {
  filled <- !is_blank(x)
  numeric <- !is.na(as_number(trimws(as_text(x))))
  mostly_numbers <- sum(filled) > 0 && sum(numeric & filled) / sum(filled) >= share
  cell_issues(x, row$column, mostly_numbers & filled & !numeric,
              "the column holds numbers but this value is text")
}
