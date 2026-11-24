# Checks of confidentiality and documentation: values and columns that hold
# personal data, and columns the specification does not describe.

#' Checks of confidentiality and documentation
#'
#' `check_pii_values()` flags values that hold an e-mail address, in any
#' column, or a telephone number (nine to fifteen digits, with an optional
#' leading +), in text columns. `check_pii_columns()` flags columns whose names suggest personal
#' data: names, telephone numbers, e-mail and postal addresses, national
#' identity or passport numbers, dates of birth and GPS coordinates. These
#' are prompts to review a column before data are shared, as ISO/IEC 25012
#' asks of confidentiality; they do not prove that a column is or is not
#' personal data. `check_documentation()` flags columns the specification
#' gives no description.
#'
#' @inheritParams structure_checks
#' @return A [check_result()].
#' @name privacy_checks
#' @examples
#' data <- data.frame(contact = c("a@example.org", "none"), phone_number = c(NA, NA))
#' check_pii_values(data, infer_spec(data), qualitas_options())
#' check_pii_columns(data, infer_spec(data), qualitas_options())
NULL

#' @rdname privacy_checks
#' @export
check_pii_values <- function(data, spec, options) {
  texts <- columns_typed(spec, data, c("text", "category"))
  found <- purrr::imap(data, \(x, column) {
    text <- trimws(as_text(x))
    email <- grepl(options$email_pattern, text, perl = TRUE)
    phone <- column %in% texts & grepl(options$phone_pattern, text, perl = TRUE)
    said <- dplyr::if_else(email, "the value looks like an e-mail address",
                           "the value looks like a telephone number")
    cell_issues(x, column, email | phone, said)
  }) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
  check_result(found, checked = sum(purrr::map_int(data, \(x) sum(!is_blank(x)))))
}

#' @rdname privacy_checks
#' @export
check_pii_columns <- function(data, spec, options) {
  personal <- names(data)[grepl(options$pii_name_pattern, names(data),
                                ignore.case = TRUE, perl = TRUE)]
  check_result(tibble::tibble(column = personal,
                              message = "the column name suggests personal data"),
               checked = ncol(data))
}

#' @rdname privacy_checks
#' @export
check_documentation <- function(data, spec, options) {
  # A specification inferred from the data describes nothing, so there is
  # no documentation to check.
  if (isTRUE(attr(spec, "inferred"))) return(check_result())
  present <- spec_present(spec, data)
  check_result(tibble::tibble(column = present$column[is.na(present$description)],
                              message = "the specification gives the column no description"),
               checked = nrow(present))
}
