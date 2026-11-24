# Checks of the digits of recorded numbers: last digits that are not
# uniform, ages heaped on multiples of five, and first digits that do not
# follow Benford's law. Each is a sign that values were rounded, guessed or
# made up rather than measured.

#' Checks of digit patterns
#'
#' `check_digit_preference()` tests whether the last digits of a measured
#' column are uniform, with a chi-square test on nine degrees of freedom, and
#' reports the digit preference score of the SMART methodology,
#' 100 sqrt(chi-square / (n x 9)), where 0 means no preference. A column is
#' flagged when the p-value is below `digit_p` and the score above
#' `digit_score_max` (20, the band SMART calls problematic), with at least
#' `digit_min_n` values whose middle half spans at least `digit_min_iqr`
#' steps of the recorded place (values of 1 to 12 end in 1 and 2 more often
#' by nature). Values recorded in tens are read one place up, and values
#' recorded in fives are not tested. The
#' recorded place is the number of decimals most values carry; columns
#' recorded to more than `digit_max_places` decimals are computed values
#' and are not tested.
#' `check_age_heaping()` computes the Whipple index over ages 23 to
#' 62 of each column whose role is `age`: 100 means no heaping on ages
#' ending in 0 or 5, 500 means every age ends in 0 or 5. `check_benford()`
#' compares the first digits of each column whose role is `amount`, positive
#' and spanning at least two orders of magnitude, with Benford's law, by mean
#' absolute deviation (Nigrini 2012); the law holds for amounts such as
#' payments, not for measurements, counts or identifiers, so no column is
#' tested unless the specification declares it. A column too small or too narrow for a test is not
#' counted as checked.
#'
#' @inheritParams structure_checks
#' @return A [check_result()].
#' @name digit_checks
#' @examples
#' data <- data.frame(age = rep(c(25, 30, 35, 40, 33), 20))
#' check_age_heaping(data, infer_spec(data), qualitas_options())
NULL

#' @rdname digit_checks
#' @export
check_digit_preference <- function(data, spec, options) {
  measured <- measured_columns(spec, data)
  tested <- purrr::map(measured, \(column) digit_test(data[[column]], column, options)) |>
    purrr::compact()
  check_result(dplyr::bind_rows(purrr::list_rbind(tested), no_issues()), checked = length(tested))
}

# The terminal-digit test of one column: an issue when it fails, none when
# it passes, NULL when the column cannot be tested.
digit_test <- function(x, column, options) {
  value <- as_number(x)
  value <- value[!is.na(value)]
  if (length(value) < options$digit_min_n) return(NULL)
  # The recorded place is the number of decimals most values carry; the last
  # digit is read at that place, so 65.0 beside 64.3 ends in 0, and one
  # 13862.4 among whole numbers does not move the place. Values with more
  # than `digit_max_places` decimals were computed, not written down.
  decimals <- dplyr::coalesce(nchar(stringr::str_extract(as_text(value), "(?<=\\.)\\d+")), 0L)
  places <- as.integer(names(which.max(table(decimals))))
  if (places > options$digit_max_places) return(NULL)
  steps <- round(abs(value) * 10^places)
  # Values recorded in tens (SAT scores, prices in whole tens) are read one
  # place up; values recorded in fives (wind in 5-knot steps) follow a
  # convention and are not tested.
  tens <- purrr::map_lgl(1:3, \(k) mean(steps %% 10^k == 0) >= options$digit_coarse_share)
  steps <- steps / 10^sum(cumprod(tens))
  if (mean(steps %% 5 == 0) >= options$digit_coarse_share) return(NULL)
  last <- as.character(steps %% 10)
  # The last digit carries a preference only when the middle half of the
  # values spans at least `digit_min_iqr` steps; small counts piled up near
  # zero end in low digits by nature.
  if (diff(stats::quantile(steps, c(0.25, 0.75), names = FALSE)) < options$digit_min_iqr) {
    return(NULL)
  }
  observed <- table(factor(last, levels = 0:9))
  expected <- length(last) / 10
  chi <- sum((observed - expected)^2 / expected)
  p <- stats::pchisq(chi, df = 9, lower.tail = FALSE)
  score <- 100 * sqrt(chi / (length(last) * 9))
  if (p >= options$digit_p || score <= options$digit_score_max) return(no_issues())
  tibble::tibble(column = column, value = sprintf("score %.1f", score),
                 message = sprintf("last digits are not uniform (p %s); most often %s",
                                   format.pval(p, digits = 2), names(which.max(observed))))
}
