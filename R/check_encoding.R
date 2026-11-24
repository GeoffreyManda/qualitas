# The check of text encoding, documented with the structure checks.

#' @rdname structure_checks
#' @export
check_encoding <- function(data, spec, options) {
  texts <- names(data)[purrr::map_lgl(data, \(x) is.character(x) || is.factor(x))]
  found <- purrr::map(texts, \(column) {
    raw <- as.character(data[[column]])
    cell_issues(data[[column]], column, !is.na(raw) & !validUTF8(raw),
                "the text is not valid UTF-8; it is read as Latin-1")
  }) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
  check_result(found, checked = sum(purrr::map_int(texts, \(c) sum(!is.na(data[[c]])))))
}
