#' Check expected answers and choice cardinality
#'
#' `assess_answers()` uses an explicit logical applicability mask: TRUE means
#' an answer is expected, FALSE means it is skipped, and NA means applicability
#' is unresolved. Missingness codes are supplied by the caller. Unexpected
#' answers on skipped questions remain separate from missing expected answers.
#' `assess_choices()` validates one choice field using a declared delimiter,
#' allowed codes, exclusive options and minimum/maximum selections. It does
#' not infer meanings, translate labels or split on spaces unless instructed.
#' @param data A data frame.
#' @param column One existing column name.
#' @param applicable Logical vector with one entry per row.
#' @param missing_codes Character codes representing missing answers.
#' @param allowed Character vector of permissible choice codes.
#' @param multiple Whether multiple selections are permitted.
#' @param separator Literal separator for multiple choices.
#' @param exclusive Codes that must occur alone.
#' @param minimum,maximum Selection bounds for nonmissing answers.
#' @return A tibble of defects with `requirement`, `row`, and `message`.
#'   Values are omitted. Blank answers are handled by `assess_answers()`.
#' @name answer_contracts
#' @export
#' @examples
#' data <- data.frame(consent = c("yes", "no"), response = c("a;b", "a"))
#' assess_answers(data, "response", data$consent == "yes")
#' assess_choices(data, "response", c("a", "b"), multiple = TRUE)
assess_answers <- function(data, column, applicable, missing_codes = character()) {
  key_frame(data, column)
  if (length(column) != 1 || !is.logical(applicable) || length(applicable) != nrow(data)) {
    cli::cli_abort("Supply one column and a logical applicability value for each row.")
  }
  blank <- is_blank(data[[column]]) | as_text(data[[column]]) %in% missing_codes
  dplyr::bind_rows(
    contract_defects("applicability_unknown", which(is.na(applicable)),
                      "expected-answer status is unresolved"),
    contract_defects("expected_missing", which(applicable & blank), "expected answer is absent"),
    contract_defects("skipped_answer", which(!applicable & !blank),
                      "answer is present on a skipped question"))
}

#' @rdname answer_contracts
#' @export
assess_choices <- function(data, column, allowed, multiple = FALSE, separator = ";",
                            exclusive = character(), minimum = 1L, maximum = Inf) {
  key_frame(data, column)
  if (length(column) != 1 || !is.character(allowed) || any(is_blank(allowed)) ||
      anyDuplicated(allowed) || !length(allowed) || !all(exclusive %in% allowed)) {
    cli::cli_abort("Declare one column, unique allowed codes and valid exclusive codes.")
  }
  if (!is.logical(multiple) || length(multiple) != 1 || is.na(multiple) ||
      length(separator) != 1 || is.na(separator) || !nzchar(separator) ||
      !is.numeric(minimum) || length(minimum) != 1 || !is.finite(minimum) || minimum < 0 ||
      !is.numeric(maximum) || length(maximum) != 1 || is.na(maximum) || maximum < minimum) {
    cli::cli_abort("Declare valid choice cardinality and one nonempty separator.")
  }
  if (multiple && any(grepl(separator, allowed, fixed = TRUE))) {
    cli::cli_abort("Allowed codes cannot contain the separator; use a child table instead.")
  }
  text <- as_text(data[[column]])
  purrr::map(seq_len(nrow(data)), \(i) {
    if (is_blank(text[i])) return(contract_defects(character(), integer(), character()))
    tokens <- if (multiple) trimws(strsplit(paste0(text[i], separator, "__END__"),
                                            separator, fixed = TRUE)[[1]]) else trimws(text[i])
    if (multiple) tokens <- utils::head(tokens, -1L)
    n <- length(tokens)
    bad <- c(choice_unknown = any(!tokens %in% allowed),
              choice_duplicate = anyDuplicated(tokens) > 0,
              choice_exclusive = n > 1 && any(tokens %in% exclusive),
              choice_cardinality = n < minimum || n > min(maximum, if (multiple) Inf else 1))
    rules <- names(bad)[bad]
    contract_defects(rules, rep(i, length(rules)), gsub("_", " ", rules))
  }) |> purrr::list_rbind() |>
    dplyr::bind_rows(contract_defects(character(), integer(), character()))
}
