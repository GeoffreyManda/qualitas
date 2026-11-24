# The Whipple index of age heaping, documented with the other digit checks
# in check_digits.R.

#' @rdname digit_checks
#' @export
check_age_heaping <- function(data, spec, options) {
  ages <- columns_with_role(spec, data, "age")
  tested <- purrr::map(ages, \(column) whipple_test(data[[column]], column, options)) |>
    purrr::compact()
  check_result(dplyr::bind_rows(purrr::list_rbind(tested), no_issues()), checked = length(tested))
}

# The Whipple index of one age column: an issue above the limit, NULL when
# too few ages fall between 23 and 62.
whipple_test <- function(x, column, options) {
  age <- as_number(x)
  adult <- age[!is.na(age) & age >= 23 & age <= 62 & age %% 1 == 0]
  if (length(adult) < options$digit_min_n) return(NULL)
  whipple <- 100 * sum(adult %% 5 == 0) / (length(adult) / 5)
  if (whipple <= options$whipple_max) return(no_issues())
  tibble::tibble(column = column, value = sprintf("Whipple %.0f", whipple),
                 message = "ages heap on numbers ending in 0 or 5")
}
