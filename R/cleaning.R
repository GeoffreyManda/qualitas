#' Keep a cleaning log and apply it
#'
#' A cleaning log records, for each problem found, what is to be done about
#' it and why, as in the data-cleaning guidance of REACH and the World Bank
#' DIME team: nothing is changed in the data until a person has decided, and
#' every change can be traced back to the problem that prompted it.
#' `cleaning_log()` turns the issues of an assessment into a log to fill in:
#' write it to a CSV file, set `action` for each row to `keep`, `change` or
#' `remove`, give `new_value` for changes and `reason` for each decision.
#' `apply_log()` then makes the changes and removals, and returns the cleaned
#' data; the original data are not modified.
#'
#' @param x An assessment from [assess()].
#' @param data The data the assessment was run on.
#' @param log A cleaning log: a data frame or a path to a CSV file.
#' @return `cleaning_log()` returns a tibble with one row per issue that
#'   points to a record. `apply_log()` returns the cleaned data, with the
#'   log of what was applied as its `cleaning` attribute.
#' @export
#' @examples
#' data <- data.frame(id = 1:3, age = c(30, 300, 31))
#' result <- assess(data, name = "toy", checks = "VA02",
#'                  spec = spec_complete(tibble::tibble(column = c("id", "age"),
#'                                                      type = "integer", max = c(NA, "120"))))
#' log <- cleaning_log(result)
#' log$action <- "change"
#' log$new_value <- "30"
#' log$reason <- "the interviewer recorded 30 on the paper form"
#' apply_log(data, log)
cleaning_log <- function(x) {
  x$issues |>
    dplyr::filter(!is.na(.data$row)) |>
    dplyr::transmute(log_id = dplyr::row_number(), .data$check_id, .data$severity,
                     .data$column, .data$row, .data$value, .data$message,
                     action = NA_character_, new_value = NA_character_,
                     reason = NA_character_, decided_by = NA_character_)
}

#' @rdname cleaning_log
#' @export
apply_log <- function(data, log) {
  log <- read_log(log)
  unknown <- setdiff(stats::na.omit(log$action), c("keep", "change", "remove"))
  if (length(unknown) > 0) {
    cli::cli_abort("The action{?s} {.val {unknown}} {?is/are} not keep, change or remove.")
  }
  changes <- dplyr::filter(log, .data$action %in% "change", .data$column %in% names(data),
                           !is.na(.data$row), stringr::str_detect(.data$column, ";", negate = TRUE))
  cleaned <- purrr::reduce(seq_len(nrow(changes)), \(current, i) {
    column <- changes$column[i]
    current[[column]][changes$row[i]] <- as_type_of(changes$new_value[i], current[[column]])
    current
  }, .init = data)
  removed <- unique(log$row[log$action %in% "remove"])
  cleaned <- cleaned[!seq_len(nrow(cleaned)) %in% removed, , drop = FALSE]
  attr(cleaned, "cleaning") <- dplyr::filter(log, .data$action %in% c("change", "remove"))
  cli::cli_inform("Changed {nrow(changes)} value{?s} and removed {length(removed)} record{?s}.")
  cleaned
}

# A cleaning log read from a file, or taken as it is, with every column as
# text except the row number.
read_log <- function(log) {
  if (is.character(log)) {
    log <- readr::read_csv(log, col_types = readr::cols(.default = readr::col_character()),
                           progress = FALSE)
  }
  dplyr::mutate(tibble::as_tibble(log), dplyr::across(-"row", as.character),
                row = as.integer(.data$row))
}

# A new value read as the type of the column it goes into.
as_type_of <- function(value, column) {
  if (is.numeric(column)) return(as_number(value))
  if (inherits(column, "Date")) return(as_day(value))
  if (is.logical(column)) return(as.logical(value))
  if (is.factor(column)) return(factor(value, levels = levels(column)))
  value
}
