# The check of interview length, documented with the interview checks.

#' @rdname interview_checks
#' @export
check_short_duration <- function(data, spec, options) {
  lengths <- interview_lengths(data, spec)
  found <- purrr::imap(lengths, \(minutes, column) {
    limit <- dplyr::coalesce(options$min_duration,
                             options$short_duration_share * stats::median(minutes, na.rm = TRUE))
    longest <- dplyr::coalesce(options$longest_interview, Inf)
    said <- dplyr::case_when(
      minutes < 0 ~ "the interview ends before it starts",
      minutes < limit ~ sprintf("the interview is shorter than %.1f minutes", limit),
      .default = sprintf("the interview is longer than %s minutes", options$longest_interview))
    cell_issues(round(minutes, 1), column, minutes < limit | minutes > longest, said)
  }) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
  check_result(found, checked = sum(purrr::map_int(lengths, \(m) sum(!is.na(m)))))
}

# The interview lengths in minutes, named by their column: the columns whose
# role is duration, or else the time from the start column to the end column.
interview_lengths <- function(data, spec) {
  timed <- columns_with_role(spec, data, "duration")
  if (length(timed) > 0) return(purrr::map(rlang::set_names(timed), \(c) as_number(data[[c]])))
  start <- columns_with_role(spec, data, "start")
  end <- columns_with_role(spec, data, "end")
  if (length(start) != 1 || length(end) != 1) return(list())
  minutes <- as.numeric(difftime(as_time(data[[end]]), as_time(data[[start]]), units = "mins"))
  rlang::set_names(list(minutes), paste(start, "to", end))
}
