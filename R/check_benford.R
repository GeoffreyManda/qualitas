# The Benford test of first digits, documented with the other digit
# checks in check_digits.R.

#' @rdname digit_checks
#' @export
check_benford <- function(data, spec, options) {
  # Benford's law describes amounts that grow by multiplication, such as
  # payments or populations; it is tested only on columns declared as amounts.
  amounts <- columns_with_role(spec, data, "amount")
  tested <- purrr::map(amounts, \(column) benford_test(data[[column]], column, options)) |>
    purrr::compact()
  check_result(dplyr::bind_rows(purrr::list_rbind(tested), no_issues()), checked = length(tested))
}

# The Benford test of one column: an issue when its first digits deviate,
# NULL when the column is too small or too narrow to test.
benford_test <- function(x, column, options) {
  value <- as_number(x)
  value <- value[!is.na(value) & value > 0]
  wide <- length(value) >= options$benford_min_n && max(value) / min(value) >= 100
  if (!wide) return(NULL)
  first <- as.integer(substr(formatC(value, format = "e"), 1, 1))
  observed <- as.numeric(table(factor(first, levels = 1:9))) / length(first)
  deviation <- mean(abs(observed - log10(1 + 1 / 1:9)))
  if (deviation <= options$benford_mad) return(no_issues())
  tibble::tibble(column = column, value = sprintf("MAD %.3f", deviation),
                 message = "first digits do not follow Benford's law")
}
