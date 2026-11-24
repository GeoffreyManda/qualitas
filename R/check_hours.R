# The check of the hour of interviews, documented with the interview checks.

#' @rdname interview_checks
#' @export
check_interview_hours <- function(data, spec, options) {
  window <- options$working_hours
  if (length(window) != 2 || anyNA(window)) return(check_result())
  bounds <- clock_minutes(window)
  timed <- columns_with_role(spec, data, c("start", "end"))
  found <- purrr::map(timed, \(column) {
    at <- as_time(data[[column]])
    minute <- clock_minutes(format(at, "%H:%M"))
    # A date-time at midnight in every record carries no time of day.
    has_time <- !all(minute %in% c(0, NA))
    outside <- has_time & (minute < bounds[1] | minute > bounds[2])
    cell_issues(data[[column]], column, outside,
                sprintf("the interview %s at %s, outside the working hours %s to %s",
                        dplyr::if_else(column %in% columns_with_role(spec, data, "end"),
                                       "ends", "starts"),
                        format(at, "%H:%M"), window[1], window[2]))
  }) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
  check_result(found, checked = sum(purrr::map_int(timed, \(c) sum(!is.na(as_time(data[[c]]))))))
}

# Minutes after midnight of times written HH:MM.
clock_minutes <- function(text) {
  parts <- stringr::str_match(text, "^(\\d{1,2}):(\\d{2})")
  as.numeric(parts[, 2]) * 60 + as.numeric(parts[, 3])
}
