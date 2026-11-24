# Checks for survey interviews: interviews too short to have been held,
# blocks of questions answered all alike, and enumerators whose records
# differ markedly from everyone else's.

#' Checks of survey interviews
#'
#' These follow the high-frequency checks used in field surveys (Innovations
#' for Poverty Action; World Bank DIME). `check_short_duration()` flags
#' interviews in a column whose role is `duration` shorter than
#' `min_duration`, or, when that is not set, shorter than
#' `short_duration_share` of the median. Without a duration column, the
#' length is the time from the column whose role is `start` to the one whose
#' role is `end`, in minutes, and an interview that ends before it starts is
#' flagged too, as is one longer than `longest_interview` when that is set.
#' `check_interview_hours()` flags starts and ends outside `working_hours`,
#' given as two times such as `c("07:00", "19:00")`; it runs only when they
#' are set, since no general rule fixes the hours of fieldwork. `check_straightlining()` treats
#' columns whose names differ only by a final number (`q1`, `q2`, `q3`) as
#' one block, and flags records that give the same answer to every item of
#' a block of at least `straightline_min_items` items.
#' `check_enumerator_effects()` compares each enumerator's mean of each
#' measured column, and their share of missing values, with the other
#' enumerators' records, and flags differences of more than
#' `enumerator_difference` standard deviations of those records.
#'
#' @inheritParams structure_checks
#' @return A [check_result()].
#' @name interview_checks
#' @examples
#' data <- data.frame(duration_min = c(40, 35, 42, 38, 4))
#' check_short_duration(data, infer_spec(data), qualitas_options())
NULL

#' @rdname interview_checks
#' @export
check_straightlining <- function(data, spec, options) {
  items <- setdiff(names(data), columns_with_role(spec, data, c("id", "date", "enumerator")))
  blocks <- tibble::tibble(column = items, block = stringr::str_remove(items, "[_.]?\\d+$")) |>
    dplyr::filter(.data$block != .data$column, nzchar(.data$block)) |>
    dplyr::group_by(.data$block) |>
    dplyr::filter(dplyr::n() >= options$straightline_min_items) |>
    dplyr::summarise(columns = list(.data$column), .groups = "drop")
  found <- purrr::map2(blocks$block, blocks$columns, \(block, columns) {
    answers <- as.data.frame(purrr::map(data[columns], as_text))
    alike <- purrr::pmap_lgl(answers, \(...) dplyr::n_distinct(c(...), na.rm = TRUE) == 1 &&
                               sum(!is.na(c(...))) == length(columns))
    tibble::tibble(column = block, row = which(alike), value = answers[[1]][alike],
                   message = rep(sprintf("every item of %s has the same answer", block),
                                 sum(alike)))
  }) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
  check_result(found, checked = nrow(data) * nrow(blocks))
}

#' @rdname interview_checks
#' @export
check_enumerator_effects <- function(data, spec, options) {
  enumerators <- columns_with_role(spec, data, "enumerator")
  if (length(enumerators) == 0) return(check_result())
  who <- as_text(data[[enumerators[1]]])
  measures <- c(purrr::map(data[measured_columns(spec, data)], as_number),
                list(share_missing = rowMeans(as.data.frame(purrr::map(data, is_blank)))))
  found <- purrr::imap(measures, \(value, column) enumerator_gap(value, who, column, options)) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
  check_result(found, checked = length(measures) * dplyr::n_distinct(who, na.rm = TRUE))
}

# The enumerators whose mean of one measure lies far from the others'.
enumerator_gap <- function(value, who, column, options) {
  means <- tibble::tibble(who, value) |>
    dplyr::filter(!is.na(.data$who), !is.na(.data$value)) |>
    dplyr::summarise(n = dplyr::n(), mean = mean(.data$value), .by = "who") |>
    dplyr::filter(.data$n >= options$group_min_n)
  if (nrow(means) < 3) return(no_issues())
  # Each enumerator is set against the other enumerators' records alone, in
  # standard deviations of those records.
  kept <- !is.na(who) & !is.na(value)
  means$z <- purrr::map_dbl(means$who, \(one) {
    others <- value[kept & who != one]
    (mean(value[kept & who == one]) - mean(others)) / stats::sd(others)
  })
  means <- dplyr::filter(means, is.finite(.data$z), abs(.data$z) > options$enumerator_difference)
  tibble::tibble(column = rep(column, nrow(means)), value = means$who,
                 message = sprintf(paste("this enumerator's mean, %.3g, differs by %.1f",
                                         "standard deviations from the others'"),
                                   means$mean, abs(means$z)))
}
