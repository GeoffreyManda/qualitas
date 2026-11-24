# Small helpers the checks share.

`%||%` <- function(x, y) if (is.null(x)) y else x

# The first value, or the second when the first is missing.
`%|NA|%` <- function(x, y) dplyr::coalesce(x, y)

# The specification rows of the columns the data hold.
spec_present <- function(spec, data) dplyr::filter(spec, .data$column %in% names(data))

# The names of the data's columns whose specification has one of the given
# types or roles.
columns_typed <- function(spec, data, types) {
  spec_present(spec, data)$column[spec_present(spec, data)$type %in% types]
}
columns_with_role <- function(spec, data, roles) {
  spec_present(spec, data)$column[spec_present(spec, data)$role %in% roles]
}

# A column's values as text, with missing values kept missing. Text that is
# not valid UTF-8 is read as Latin-1, the most common other encoding, so
# that no check fails on it (check_encoding() reports it).
as_text <- function(x) {
  text <- utf8_text(as.character(x))
  dplyr::if_else(is.na(x), NA_character_, text)
}

# Text with any value that is not valid UTF-8 converted from Latin-1.
utf8_text <- function(text) {
  valid <- is.na(text) | validUTF8(text)
  if (all(valid)) return(text)
  dplyr::if_else(valid, text, iconv(text, from = "latin1", to = "UTF-8"))
}

# A column's values as numbers; text that does not read as a number is
# missing. Only text written as a number is converted, so nothing warns.
as_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  text <- trimws(utf8_text(as.character(x)))
  readable <- grepl("^[-+]?(\\d+\\.?\\d*|\\.\\d+)([eE][-+]?\\d+)?$", text)
  dplyr::if_else(readable, as.numeric(dplyr::if_else(readable, text, "0")), NA_real_)
}


# Whether each value is truly missing: NA, or text that is empty or blank.
is_blank <- function(x) is.na(x) | trimws(as_text(x)) %in% ""

# The issues for the cells of one column where `bad` is TRUE.
cell_issues <- function(x, column, bad, message) {
  rows <- which(bad %in% TRUE)
  said <- if (length(message) == length(x)) message[rows] else rep(message, length(rows))
  tibble::tibble(column = rep(column, length(rows)), row = rows,
                 value = as_text(x)[rows], message = said)
}

# A text list written with ";" between its items, as a vector.
split_list <- function(text) {
  if (is.na(text) || !nzchar(text)) return(character())
  trimws(strsplit(text, ";", fixed = TRUE)[[1]])
}

# The issues of every column, from a function of one column's values and its
# specification row.
per_column <- function(data, spec, fn) {
  rows <- spec_present(spec, data)
  purrr::map(seq_len(nrow(rows)), \(i) fn(data[[rows$column[i]]], as.list(rows[i, ]))) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
}

# The numeric columns that measure something: identifiers, groups and
# enumerators are left out, and so are codes with few distinct values.
measured_columns <- function(spec, data) {
  numeric <- columns_typed(spec, data, c("integer", "number"))
  kept <- numeric[!numeric %in% columns_with_role(spec, data, c("id", "group", "enumerator"))]
  kept[purrr::map_int(kept, \(c) dplyr::n_distinct(data[[c]], na.rm = TRUE)) > 10]
}

# How many values of the given columns read as numbers.
count_numbers <- function(data, columns) {
  sum(purrr::map_int(columns, \(c) sum(!is.na(as_number(data[[c]])))))
}
