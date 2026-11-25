#' Assess requirement coverage using reviewed evidence
#'
#' Matches exact requirement and scope identifiers; missing evidence, failed
#' tests, expiry, and reviewed exceptions remain separate. A met result means
#' the supplied evidence passed its declared verification and review, not that
#' qualitas performed the field visit, access test, or restoration itself.
#' @param requirements Table with `requirement_id`, `scope_id`, `source`, `owner`.
#'   The scope identifies the actual dataset, period, system and test covered.
#' @param evidence Table with the same two identifiers and `path`, `sha256`,
#'   `outcome` (passed, failed, explained), `reviewer`, `reviewed_at`, `reason`;
#'   optional `valid_until` is a date. Paths refer to readable local evidence files.
#' @param as_of Date or YYYY-MM-DD string, including the whole UTC day.
#'   Review timestamps are converted to UTC; date-only reviews use their stated
#'   calendar day. Time-of-day cutoffs are rejected rather than truncated.
#' @return Requirement table with status and evidence path; no file contents.
#' @export
#' @examples
#' req <- data.frame(requirement_id = "restore", scope_id = "system-A:2026-Q3",
#'                   source = "agreement section 4", owner = "IA")
#' assess_requirements(req)
assess_requirements <- function(requirements, evidence = NULL, as_of = Sys.Date()) {
  fields <- c("requirement_id", "scope_id", "source", "owner")
  if (nrow(required_metadata(requirements, fields))) {
    cli::cli_abort("Requirements need complete textual identity, scope, source and owner.")
  }
  key <- c("requirement_id", "scope_id")
  if (anyDuplicated(requirements[key])) cli::cli_abort("Requirement and scope must be unique.")
  if (length(as_of) != 1L || !(inherits(as_of, "Date") ||
      (is.character(as_of) && isTRUE(valid_timestamp(as_of, allow_date = TRUE)) && nchar(as_of) == 10L)))
    cli::cli_abort("Supply one assessment Date or YYYY-MM-DD, without a time cutoff.")
  as_of <- as.Date(as_of)
  if (!is.finite((as.numeric(as_of) + 1) * 86400) || as.numeric(as_of) != floor(as.numeric(as_of)))
    cli::cli_abort("Supply one finite assessment date.")
  if (is.null(evidence)) return(dplyr::mutate(requirements, status = "missing evidence"))
  fields <- c(key, "path", "sha256", "outcome", "reviewer", "reviewed_at", "reason")
  if (nrow(required_metadata(evidence, fields))) {
    cli::cli_abort("Evidence needs complete identity, file hash, outcome and review metadata.")
  }
  if (anyDuplicated(evidence[key])) cli::cli_abort("Evidence must uniquely cover each scope.")
  if (!all(evidence$outcome %in% c("passed", "failed", "explained")) ||
      !all(valid_timestamp(evidence$reviewed_at, allow_date = TRUE))) {
    cli::cli_abort("Evidence must declare a valid outcome and review date.")
  }
  reviewed <- purrr::map_dbl(evidence$reviewed_at, function(value) {
    day <- as.numeric(as.Date(substr(value, 1, 10))) * 86400
    if (nchar(value) == 10L) return(day)
    clock <- as.numeric(substring(value, c(12, 15, 18), c(13, 16, 19)))
    offset <- if (nchar(value) == 20L) 0 else
      sum(as.numeric(substring(value, c(21, 24), c(22, 25))) * c(3600, 60)) *
        ifelse(substr(value, 20, 20) == "-", -1, 1)
    day + sum(clock * c(3600, 60, 1)) - offset
  })
  evidence$.future <- reviewed >= (as.numeric(as_of) + 1) * 86400
  verified <- purrr::map_lgl(seq_len(nrow(evidence)), \(i) {
    path <- evidence$path[i]
    file.exists(path) && !dir.exists(path) && grepl("^[a-fA-F0-9]{64}$", evidence$sha256[i]) &&
      identical(digest::digest(file = path, algo = "sha256"), tolower(evidence$sha256[i]))
  })
  evidence$.verified <- verified
  evidence$.present <- rep(TRUE, nrow(evidence))
  if (!"valid_until" %in% names(evidence)) evidence$valid_until <- rep(NA_character_, nrow(evidence))
  expiry <- suppressWarnings(as.Date(evidence$valid_until, format = "%Y-%m-%d"))
  invalid <- !is.na(evidence$valid_until) & (is.na(expiry) |
    format(expiry) != evidence$valid_until)
  if (any(invalid)) cli::cli_abort("Evidence expiry must be a valid calendar date.")
  evidence$.expired <- !is.na(expiry) & expiry < as_of
  result <- dplyr::left_join(requirements, evidence, by = key, relationship = "one-to-one", na_matches = "never")
  result |>
    dplyr::mutate(status = dplyr::case_when(is.na(.data$.present) ~ "missing evidence",
      .data$.future ~ "not yet reviewed", !.data$.verified ~ "unverified evidence",
      .data$.expired ~ "expired evidence",
      .data$outcome == "failed" ~ "failed", .data$outcome == "explained" ~ "explained",
      .default = "met from reviewed evidence")) |>
    dplyr::select(-dplyr::starts_with("."))
}

#' Assess numeric precision and missing-answer patterns
#'
#' Precision uses an explicitly declared decimal resolution. Missing patterns
#' use an explicit applicability matrix with the same dimensions as the data.
#' @param data A data frame.
#' @param column One numeric column.
#' @param resolution Positive finite measurement step, in the column's units.
#'   Floating-point limits produce `resolution_unverifiable` findings separately
#'   from off-grid or infinite values (`measurement_resolution`). NA/NaN are skipped.
#' @param applicable Logical data frame or matrix; TRUE identifies expected cells.
#' @param minimum Minimum records sharing a missing-answer pattern for reporting.
#' @return Defect table; pattern rows include their record index, without values.
#' @rdname assess_requirements
#' @export
#' @examples
#' assess_precision(data.frame(x = c(1.1, 1.25)), "x", resolution = 0.1)
assess_precision <- function(data, column, resolution) {
  key_frame(data, column)
  if (length(column) != 1 || !is.numeric(data[[column]]) || is.complex(data[[column]]) || length(resolution) != 1 ||
      !is.numeric(resolution) || is.complex(resolution) || !is.finite(resolution) || resolution <= 0) {
    cli::cli_abort("Precision needs one numeric column and a positive finite resolution.")
  }
  value <- data[[column]]; x <- value / resolution
  tolerance <- 8 * .Machine$double.eps * abs(x)
  uncertain <- !is.na(value) & is.finite(value) & (!is.finite(x) | tolerance >= 0.25)
  bad <- !is.na(value) & !uncertain & (!is.finite(value) |
    (value != 0 & abs(value) < resolution) | abs(x - round(x)) > tolerance)
  dplyr::bind_rows(contract_defects("measurement_resolution", which(bad), "value exceeds declared resolution"),
    contract_defects("resolution_unverifiable", which(uncertain), "floating-point precision cannot establish resolution")) |>
    dplyr::arrange(.data$row)
}

#' @rdname assess_requirements
#' @export
assess_missing_patterns <- function(data, applicable, minimum = 2L) {
  mask <- as.matrix(applicable)
  if (!is.logical(mask) || !identical(dim(mask), dim(data)) || anyNA(mask) ||
      length(minimum) != 1 || !is.finite(minimum) || minimum < 2) {
    cli::cli_abort("Supply a complete logical applicability matrix and minimum of at least two.")
  }
  if (!nrow(data)) return(contract_defects(character(), integer(), character()))
  absent <- as.data.frame(lapply(data, is_blank)) & mask
  signature <- apply(absent, 1, \(row) paste(which(row), collapse = ";"))
  counts <- table(signature)
  repeated <- nzchar(signature) & as.integer(counts[signature]) >= minimum
  contract_defects("missing_pattern", which(repeated), "expected-answer missing pattern repeats")
}
