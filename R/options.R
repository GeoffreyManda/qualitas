#' Settings of an assessment
#'
#' The thresholds the checks apply. Each has a default taken from the
#' literature; change any of them by name.
#'
#' @param ... Settings to change, by name, for example `outlier_z = 3`.
#' @return A named list of settings:
#'   * `as_of`: the date the assessment refers to (today).
#'   * `missing_codes`: values read as stand-ins for a missing value.
#'   * `record_missing_share`: share of a record's values that may be
#'     missing before the record is flagged (0.5).
#'   * `outlier_z`: modified z-score beyond which a value is an outlier (3.5,
#'     Iglewicz and Hoaglin 1993; WHO Data Quality Review 2017).
#'   * `log_skew_min`: quartile skewness from which positive values are
#'     judged for outliers on the log scale (0.2).
#'   * `group_min_n`: fewest values a group needs for group outliers (10).
#'   * `near_duplicate_share`: share of values two records must share to be
#'     near duplicates (0.95; Kuriakose and Robbins 2016 used 0.85).
#'   * `digit_p`: p-value below which terminal digits are not uniform (0.001).
#'   * `digit_min_n`: fewest values for the terminal-digit test (50).
#'   * `digit_min_iqr`: fewest steps of the recorded place the middle half of
#'     the values must span for the terminal-digit test (20).
#'   * `digit_score_max`: digit preference score above which last digits are
#'     flagged (20, problematic in the SMART methodology).
#'   * `digit_coarse_share`: share of values that must be multiples of 10 (or
#'     5) for a column to be read as recorded in tens (or fives) (0.95).
#'   * `digit_max_places`: most decimals a column may carry for the
#'     terminal-digit test; more show a computed value (2).
#'   * `benford_mad`: mean absolute deviation of first digits beyond which a
#'     column does not conform to Benford's law (0.015, Nigrini 2012).
#'   * `benford_min_n`: fewest values for the Benford test (1000, as Nigrini
#'     2012 advises for the first-digit test).
#'   * `whipple_max`: Whipple index above which ages heap (110; the United
#'     Nations grades 105 to 110 as fairly accurate).
#'   * `mixed_type_share`: share of a column's values that must be numbers
#'     before its text values are flagged (0.8).
#'   * `min_duration`: shortest plausible interview, in the duration
#'     column's unit (`NA`, not checked).
#'   * `short_duration_share`: when `min_duration` is not set, the share of
#'     the median duration below which an interview is too short (1/3).
#'   * `longest_interview`: longest plausible interview in minutes (`NA`,
#'     not checked).
#'   * `working_hours`: the first and last time of day an interview may
#'     start or end, such as `c("07:00", "19:00")` (`NA`, not checked).
#'   * `alignment_share`, `alignment_record_share`, `alignment_min_cells`,
#'     `alignment_min_columns`: for [check_row_alignment()], the share of a
#'     column's values that sets its usual kind (0.9), the share of a record's
#'     values of another kind that flags it (0.5), the fewest such values (3)
#'     and the fewest typed columns needed (5).
#'   * `straightline_min_items`: fewest items of a block for straightlining (5).
#'   * `enumerator_difference`: difference between an enumerator's mean and
#'     the other enumerators', in standard deviations of their records,
#'     beyond which the enumerator's records differ markedly (0.5).
#'   * `max_age_days`: oldest the latest record may be (`NA`, not checked).
#'   * `period`: the period expected to hold records, `"month"`, `"week"` or
#'     `"quarter"` (`NA`, not checked).
#'   * `email_pattern`, `phone_pattern`: regular expressions for e-mail
#'     addresses and telephone numbers in values.
#'   * `pii_name_pattern`: regular expression for column names that suggest
#'     personal data.
#'   * `drift_missing`, `drift_shift`, `drift_psi`: change in the share of
#'     missing values (0.1), move of the median in median absolute
#'     deviations (0.5) and population stability index (0.25) beyond which
#'     [compare_data()] flags a column.
#'   * `rules`: rules records must keep (see [read_rules()]).
#'   * `tables`, `relations`: other tables and the keys that link them (see
#'     [assess_tables()]).
#' @export
#' @examples
#' qualitas_options(outlier_z = 3)$outlier_z
qualitas_options <- function(...) utils::modifyList(option_defaults(), list(...))
