# Robust statistics shared by the outlier checks.

# The modified z-score of each value against a reference set of values (by
# default the values themselves), with a separate median absolute deviation
# for each side of the median (the "double MAD"), so a distribution with a
# long tail on one side is not judged by the spread of the other. A side
# that does not vary uses the overall spread; missing when nothing varies.
modified_z <- function(value, reference = value) {
  kept <- reference[!is.na(reference)]
  centre <- stats::median(kept)
  overall <- stats::median(abs(kept - centre))
  if (length(kept) == 0 || is.na(overall) || overall == 0) {
    return(rep(NA_real_, length(value)))
  }
  lower <- half_spread(kept[kept <= centre], centre, overall)
  upper <- half_spread(kept[kept >= centre], centre, overall)
  0.6745 * (value - centre) / dplyr::if_else(value < centre, lower, upper)
}

# The median absolute deviation of one side's values from the centre, or the
# overall one when that side does not vary.
half_spread <- function(side, centre, overall) {
  spread <- stats::median(abs(side - centre))
  if (is.na(spread) || spread == 0) overall else spread
}

# Values on the scale outliers are judged on: positive values whose quartile
# skewness is at least `min_skew`, such as incomes and laboratory results, on
# the log scale when that makes their quartiles more symmetric; other values
# as they are.
outlier_scale <- function(value, min_skew) {
  kept <- value[!is.na(value)]
  positive <- length(kept) > 3 && all(kept > 0)
  skewed <- positive && quartile_skew(kept) >= min_skew &&
    abs(quartile_skew(log(kept))) < abs(quartile_skew(kept))
  if (skewed) log(value) else value
}

# Bowley's quartile skewness, (Q3 + Q1 - 2 Q2) / (Q3 - Q1); 0 when the
# quartiles coincide.
quartile_skew <- function(value) {
  q <- stats::quantile(value, c(0.25, 0.5, 0.75), names = FALSE)
  if (q[3] == q[1]) 0 else (q[3] + q[1] - 2 * q[2]) / (q[3] - q[1])
}

# Whether each value is far from a set of reference values: beyond `limit`
# modified z-scores.
far_from <- function(value, reference, limit) abs(modified_z(value, reference)) > limit
