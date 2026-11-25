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
                       rule_breaks(data, rule_id, expression, description))
  failed <- purrr::compact(purrr::map(found, "error"))
  error <- if (length(failed)) paste(failed, collapse = "; ") else NA_character_
  issues <- purrr::map(found, "issues") |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
  check_result(issues, checked = nrow(data), error = error)
}

# The records that break one rule, with the error if the rule cannot be
# evaluated. The value shown is the record's value of each column the rule
# names. A rule that cannot be evaluated makes the check an error, since the
# records it would have tested were not tested.
rule_breaks <- function(data, rule_id, expression, description) {
  parsed <- rlang::parse_expr(expression)
  held <- tryCatch(rlang::eval_tidy(parsed, data), error = \(e) conditionMessage(e))
  if (is.character(held)) {
    return(list(issues = no_issues(),
                error = paste0("rule ", rule_id, " could not be evaluated: ", held)))
  }
  named <- intersect(all.vars(parsed), names(data))
  if (!is.logical(held) || !is.atomic(held) || !is.null(dim(held)) ||
      length(held) != nrow(data)) {
    return(list(issues = no_issues(), error = paste0("rule ", rule_id,
      " must return a logical vector with exactly one value per record")))
  }
  unresolved <- if (anyNA(held)) paste0("rule ", rule_id, " has unresolved NA results") else NULL
  broken <- which(!is.na(held) & !held)
  shown <- purrr::map_chr(broken, \(i) paste(named, as_text(unlist(data[i, named])),
                                             sep = " = ", collapse = "; "))
  list(issues = tibble::tibble(column = paste(named, collapse = ";"), row = broken,
                               value = shown,
                               message = rep(paste0("rule ", rule_id, " is broken: ",
                                                    description), length(broken))),
       error = unresolved)
}
