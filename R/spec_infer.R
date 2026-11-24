#' Infer a data specification from the data
#'
#' Reads each column and writes one row of the specification: its type, its
#' role (identifier, date, enumerator, group, age or duration, guessed from
#' the column name), whether it is required and unique (identifiers whose values nearly all
#' differ are), and, for factors, its
#' allowed values. Ranges, patterns and descriptions are left empty for the
#' user to fill in; an inferred specification describes the data as found
#' and should be reviewed before it is trusted.
#'
#' @param data A data frame.
#' @param key_share Share of distinct values from which an identifier column
#'   is taken as a unique key (0.95); below it, a repeated identifier groups
#'   records, as in data with one row per person and year.
#' @return A specification: a tibble with one row per column and the columns
#'   `column`, `type`, `role`, `required`, `unique`, `min`, `max`, `allowed`,
#'   `pattern`, `missing_codes`, `description` and `after` (see
#'   [spec_template()]).
#' @export
#' @examples
#' infer_spec(data.frame(id = 1:3, age = c(20, 31, 45), sex = c("f", "m", "f")))
infer_spec <- function(data, key_share = 0.95) {
  data <- tibble::as_tibble(data, .name_repair = "minimal")
  tibble::tibble(column = names(data),
                 type = purrr::map_chr(data, infer_type),
                 role = infer_role(names(data))) |>
    # A name such as "time" or "protime" suggests a date only when the
    # values are dates or the name says so.
    dplyr::mutate(role = dplyr::if_else(
      .data$role == "date" & !.data$type %in% c("date", "datetime") &
        !grepl("date", tolower(.data$column)), "", .data$role)) |>
    dplyr::mutate(
      # An identifier is a key when nearly all its values differ; one that
      # repeats, as a school in yearly records does, groups records instead.
      unique = .data$role == "id" &
        purrr::map_dbl(data, \(x) dplyr::n_distinct(x, na.rm = TRUE) / max(sum(!is.na(x)), 1)) >=
          key_share,
      required = .data$role == "id",
      allowed = purrr::map_chr(data, \(x) if (is.factor(x)) paste(levels(x), collapse = ";")
                               else NA_character_),
      type = dplyr::if_else(!is.na(.data$allowed), "category", .data$type)
    ) |>
    spec_complete() |>
    structure(inferred = TRUE)
}

# The type of a column: date, datetime, logical, integer, number, category or
# text, judged from its filled values. Text that reads as numbers is a number,
# and text with few distinct
# values is a category.
infer_type <- function(x) {
  text <- as_text(x[!is_blank(x)])
  number <- as_number(text)
  numeric_text <- length(text) > 0 && all(!is.na(number))
  whole <- numeric_text && all(number == round(number))
  distinct <- length(unique(text))
  dplyr::case_when(
    inherits(x, "Date") ~ "date",
    inherits(x, "POSIXt") ~ "datetime",
    is.logical(x) ~ "logical",
    is.factor(x) ~ "category",
    whole ~ "integer",
    numeric_text ~ "number",
    length(text) > 0 && all(is_iso_date(text)) ~ "date",
    distinct > 0 && distinct <= max(10, length(text) * 0.05) ~ "category",
    .default = "text"
  )
}

# Whether text values are dates written year-month-day.
is_iso_date <- function(text) {
  grepl("^\\d{4}-\\d{2}-\\d{2}", text) & !is.na(as_day(text))
}

# The role of each column, guessed from its name: an identifier, a date, the
# enumerator or interviewer, a grouping place, an age or an interview
# duration; "" for any other column.
infer_role <- function(names) {
  name <- tolower(names)
  dplyr::case_when(
    grepl("(^|_)(id|uuid|key|code)$|^id_|^uuid", name) ~ "id",
    grepl("enumerator|interviewer|surveyor|collector|^enum", name) ~ "enumerator",
    grepl("interview.*(duration|length|time)|(duration|length)_?min|minutes", name) ~ "duration",
    grepl("^age($|_)|_age$", name) ~ "age",
    grepl("date|time$|_at$|^start|^end|submission", name) ~ "date",
    grepl("region|district|province|state|site|facility|village|county|ward", name) ~ "group",
    .default = ""
  )
}
