#' Rules records must keep
#'
#' A rule is an R expression that is `TRUE` for a valid record, such as
#' `age >= 18`, `end_date >= start_date` or `if (pregnant == "yes") sex ==
#' "female"` written as `pregnant != "yes" | sex == "female"`. Rules are kept
#' as data, in a CSV file or a table with the columns `rule_id`,
#' `expression` and `description`, so they can be reviewed and versioned
#' apart from the code, as in the validate package (van der Loo and de Jonge
#' 2021).
#'
#' @param x A path to a CSV file, a data frame with `rule_id`, `expression`
#'   and `description`, or a named character vector of expressions.
#' @return A tibble with `rule_id`, `expression` and `description`.
#' @export
#' @examples
#' read_rules(c(adult = "age >= 18", ordered = "end >= start"))
read_rules <- function(x) {
  table <- if (is.character(x) && length(x) == 1 && file.exists(x)) {
    readr::read_csv(x, col_types = readr::cols(.default = readr::col_character()),
                    progress = FALSE)
  } else if (is.character(x)) {
    tibble::tibble(rule_id = names(x) %||% paste0("R", seq_along(x)), expression = unname(x))
  } else {
    tibble::as_tibble(x)
  }
  dplyr::bind_rows(tibble::tibble(rule_id = character(), expression = character(),
                                  description = character()), table) |>
    dplyr::mutate(description = dplyr::coalesce(.data$description, .data$expression))
}

# The rules to apply: none, or those given, read into the standard table.
resolve_rules <- function(rules) if (is.null(rules)) NULL else read_rules(rules)

#' @rdname consistency_checks
#' @export
check_rules <- function(data, spec, options) {
  rules <- options$rules
  if (is.null(rules) || nrow(rules) == 0) return(check_result())
  found <- purrr::pmap(rules, \(rule_id, expression, description, ...)
                       rule_breaks(data, rule_id, expression, description)) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
  check_result(found, checked = nrow(data))
}

# The records that break one rule. The value shown is the record's value of
# each column the rule names; a rule that cannot be evaluated is reported
# once, without a row.
rule_breaks <- function(data, rule_id, expression, description) {
  parsed <- rlang::parse_expr(expression)
  held <- tryCatch(rlang::eval_tidy(parsed, data), error = \(e) conditionMessage(e))
  if (is.character(held)) {
    return(tibble::tibble(column = NA_character_, row = NA_integer_, value = expression,
                          message = paste0("rule ", rule_id, " could not be evaluated: ", held)))
  }
  named <- intersect(all.vars(parsed), names(data))
  broken <- which(rep_len(held, nrow(data)) %in% FALSE)
  shown <- purrr::map_chr(broken, \(i) paste(named, as_text(unlist(data[i, named])),
                                             sep = " = ", collapse = "; "))
  tibble::tibble(column = paste(named, collapse = ";"), row = broken, value = shown,
                 message = rep(paste0("rule ", rule_id, " is broken: ", description),
                               length(broken)))
}
