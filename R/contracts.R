#' Validate shared dictionaries and indicator definitions
#'
#' Checks metadata required for exchange between organizations. This validates
#' the presence and internal consistency of declarations, not their approval
#' or agreement with a questionnaire. Units can be `not applicable` for text.
#' A shared element version can appear in several tables with the same meaning.
#' @param dictionary A data frame with `element_id`, `table`, `column`,
#'   `definition`, `type`, `unit`, `owner`, and `version`.
#' @return A tibble of defects with `requirement`, `row`, and `message`.
#'   An empty tibble means all supplied declarations passed these checks.
#' @export
#' @examples
#' validate_dictionary(data.frame(element_id = "site_id"))
validate_dictionary <- function(dictionary) {
  fields <- c("element_id", "table", "column", "definition", "type", "unit", "owner", "version")
  found <- required_metadata(dictionary, fields)
  if (!all(fields %in% names(dictionary))) return(found)
  repeated <- duplicated(dictionary[c("table", "column")]) |
    duplicated(dictionary[c("table", "column")], fromLast = TRUE)
  conflicts <- dictionary |>
    dplyr::mutate(.row = seq_len(nrow(dictionary))) |>
    dplyr::group_by(.data$element_id, .data$version) |>
    dplyr::filter(dplyr::n_distinct(.data$definition, .data$type, .data$unit) > 1L) |>
    dplyr::pull(".row")
  types <- c("integer", "number", "date", "datetime", "logical", "category", "text")
  dplyr::bind_rows(found,
    contract_defects("dictionary_mapping", which(repeated), "table and column mapping repeats"),
    contract_defects("element_id", conflicts, "one element version has conflicting meanings"),
    contract_defects("type", which(!dictionary$type %in% types), "unrecognized data type"))
}

# Missing metadata columns are structural defects; missing values retain their row.
required_metadata <- function(data, fields) {
  if (!is.data.frame(data)) cli::cli_abort("Metadata must be a data frame.")
  if (anyDuplicated(names(data))) cli::cli_abort("Metadata column names must be unique.")
  missing <- setdiff(fields, names(data))
  structural <- contract_defects(missing, rep(NA_integer_, length(missing)),
                                  paste("required metadata column is absent:", missing))
  values <- purrr::map(intersect(fields, names(data)), \(field) {
    if (!is.character(data[[field]])) {
      return(contract_defects(field, NA_integer_, "metadata must contain character values"))
    }
    contract_defects(field, which(is_blank(data[[field]])), "required metadata value is absent")
  })
  dplyr::bind_rows(structural, purrr::list_rbind(values))
}

# ISO-style lexical form and valid calendar date; offsets are checked explicitly.
valid_timestamp <- function(x, allow_date = FALSE) {
  if (!is.character(x)) return(rep(FALSE, length(x)))
  pattern <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(Z|[+-][0-9]{2}:[0-9]{2})$"
  shaped <- !is.na(x) & grepl(pattern, x)
  if (allow_date) shaped <- shaped | (!is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x))
  purrr::map2_lgl(x, shaped, \(value, ok) {
    if (!ok) return(FALSE)
    day <- tryCatch(as.Date(substr(value, 1, 10)), error = \(e) as.Date(NA))
    if (is.na(day) || format(day) != substr(value, 1, 10)) return(FALSE)
    if (nchar(value) == 10) return(TRUE)
    clock <- as.integer(substring(value, c(12, 15, 18), c(13, 16, 19)))
    offset <- if (nchar(value) == 20) c(0L, 0L) else
      as.integer(substring(value, c(21, 24), c(22, 25)))
    all(clock <= c(23, 59, 59)) && all(offset <= c(23, 59))
  })
}

# Uniform defect records for contract checks; values are deliberately omitted.
contract_defects <- function(requirement, row, message) {
  n <- length(row)
  tibble::tibble(requirement = rep_len(requirement, n), row = as.integer(row),
                 message = rep_len(message, n))
}

#' Validate indicator definitions
#'
#' Every indicator declares its population, unit, numerator, denominator,
#' reference period, disaggregation, source, owner and version. Formulae are
#' documentation and are never executed by this function.
#' @rdname validate_dictionary
#' @param indicators A data frame with the fields named in the description,
#'   plus `indicator_id` and `definition`. Use `not applicable` for a count's
#'   denominator or an indicator without disaggregation.
#' @return A tibble of defects with `requirement`, `row`, and `message`.
#' @export
#' @examples
#' validate_indicators(data.frame(indicator_id = "visits_completed"))
validate_indicators <- function(indicators) {
  fields <- c("indicator_id", "definition", "population", "unit", "numerator", "denominator",
              "period", "disaggregation", "source", "owner", "version")
  found <- required_metadata(indicators, fields)
  if (!"indicator_id" %in% names(indicators)) return(found)
  id <- indicators$indicator_id
  repeated <- duplicated(id) | duplicated(id, fromLast = TRUE)
  dplyr::bind_rows(found, contract_defects("indicator_id", which(repeated),
                                         "indicator identifier repeats"))
}
