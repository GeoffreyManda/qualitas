#' Check the files of a delivery against its manifest
#'
#' Compares required files, SHA-256 hashes and documentation against an
#' agreed manifest. Paths must be relative to the delivery root; paths
#' escaping it, including symbolic links, are rejected. No files are changed.
#' @param manifest A data frame with `path`, `sha256`, `sender`, `sent_at`,
#'   `version`, and `role`. Roles are `data`, `dictionary`, `questionnaire`,
#'   `change_log`, or another locally defined role.
#' @param root Existing delivery directory.
#' @param required_roles Roles required by the exchange agreement.
#' @param filename_pattern Optional regular expression from the naming agreement,
#'   applied to each basename. Its documentary authority is supplied by the caller.
#' @return A tibble with `requirement`, `row`, and `message`; no file contents.
#' @export
#' @examples
#' assess_delivery(data.frame(path = "missing.csv"), tempdir())
assess_delivery <- function(manifest, root, required_roles = c("data", "dictionary"),
                              filename_pattern = NULL) {
  fields <- c("path", "sha256", "sender", "sent_at", "version", "role")
  found <- required_metadata(manifest, fields)
  if (!all(fields %in% names(manifest)) ||
      !all(purrr::map_lgl(manifest[fields], is.character))) return(found)
  if (!dir.exists(root)) cli::cli_abort("Delivery root must be an existing directory.")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  missing <- setdiff(required_roles, manifest$role)
  found <- dplyr::bind_rows(found, contract_defects("delivery_role",
    rep(NA_integer_, length(missing)), paste("required role is absent:", missing)))
  duplicate <- duplicated(manifest$path) | duplicated(manifest$path, fromLast = TRUE)
  found <- dplyr::bind_rows(found, contract_defects("delivery_path", which(duplicate),
                                                 "manifest path repeats"))
  found <- dplyr::bind_rows(found, contract_defects("delivery_timestamp",
    which(!valid_timestamp(manifest$sent_at)), "sent_at is not a valid timestamp with an offset"))
  resolved_paths <- vapply(file.path(root, manifest$path), normalizePath, character(1),
                            winslash = "/", mustWork = FALSE)
  aliases <- duplicated(resolved_paths) | duplicated(resolved_paths, fromLast = TRUE)
  found <- dplyr::bind_rows(found, contract_defects("delivery_alias", which(aliases),
                                                 "multiple entries resolve to the same file"))
  if (!is.null(filename_pattern)) {
    if (!is.character(filename_pattern) || length(filename_pattern) != 1 ||
        is.na(filename_pattern)) cli::cli_abort("Supply one filename expression.")
    found <- dplyr::bind_rows(found, contract_defects("delivery_name",
      which(!grepl(filename_pattern, basename(manifest$path))), "filename breaks naming agreement"))
  }
  hashes <- tolower(manifest$sha256)
  repeated <- duplicated(hashes) | duplicated(hashes, fromLast = TRUE)
  found <- dplyr::bind_rows(found, contract_defects("delivery_duplicate_content", which(repeated),
    "declared file content repeats; inspect whether the duplicate delivery is intentional"))
  checked <- purrr::map(seq_len(nrow(manifest)), \(i) {
    path <- manifest$path[i]
    if (is_blank(path)) return(contract_defects("delivery_path", i, "file path is absent"))
    candidate <- file.path(root, path)
    absolute <- grepl("^[/\\\\]|^[A-Za-z]:", path)
    if (absolute || any(strsplit(path, "[/\\\\]")[[1]] == "..")) {
      return(contract_defects("delivery_path", i, "file path escapes delivery root"))
    }
    if (!file.exists(candidate) || dir.exists(candidate)) {
      return(contract_defects("delivery_missing", i, "manifest file is absent"))
    }
    resolved <- normalizePath(candidate, winslash = "/", mustWork = TRUE)
    if (!startsWith(resolved, paste0(root, "/"))) {
      return(contract_defects("delivery_path", i, "file resolves outside delivery root"))
    }
    expected <- manifest$sha256[i]
    if (is.na(expected) || !grepl("^[a-fA-F0-9]{64}$", expected)) {
      return(contract_defects("delivery_hash", i, "SHA-256 declaration is invalid"))
    }
    actual <- digest::digest(file = resolved, algo = "sha256")
    if (!identical(actual, tolower(expected))) {
      return(contract_defects("delivery_hash", i, "file differs from declared SHA-256"))
    }
    if ("byte_size" %in% names(manifest)) {
      size <- manifest$byte_size[i]
      if (!is.numeric(size) || is.na(size) || !is.finite(size) || size < 0 ||
          size != file.info(resolved)$size) {
        return(contract_defects("delivery_size", i, "file size differs from the declaration"))
      }
    }
    contract_defects(character(), integer(), character())
  })
  dplyr::bind_rows(found, purrr::list_rbind(checked))
}
#' Reconcile reported totals with delivered records
#'
#' One row represents one already matched indicator, population and period.
#' Matching those definitions is the caller's responsibility. A zero reported
#' total agrees only with zero observed. Missing or invalid counts are errors.
#' @param counts Data frame with `indicator_id`, `period`, `reported`, `observed`.
#' @param tolerance Allowed absolute proportional deviation from the reported
#'   count; zero requires exact agreement. This is a local decision, not an ISO limit.
#' @return Input rows with `ratio`, `difference`, and `status`.
#' @export
#' @examples
#' reconcile_counts(data.frame(indicator_id = "visits", period = "2026-Q3",
#'                             reported = 10, observed = 9))
reconcile_counts <- function(counts, tolerance = 0) {
  fields <- c("indicator_id", "period", "reported", "observed")
  if (!all(fields %in% names(counts)) || nrow(required_metadata(counts, fields[1:2]))) {
    cli::cli_abort("Counts need complete indicator and period identifiers and both counts.")
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1 || !is.finite(tolerance) ||
      tolerance < 0) cli::cli_abort("Tolerance must be one finite nonnegative number.")
  if (!is.numeric(counts$reported) || !is.numeric(counts$observed)) {
    cli::cli_abort("Reported and observed counts must be numeric.")
  }
  r <- counts$reported
  o <- counts$observed
  valid <- is.finite(r) & is.finite(o) & r >= 0 & o >= 0 & r == floor(r) & o == floor(o)
  ratio <- ifelse(valid & r > 0, o / r, NA_real_)
  passed <- valid & ((r == 0 & o == 0) | (r > 0 & abs(o - r) <= tolerance * r))
  dplyr::mutate(counts, ratio = ratio, difference = ifelse(valid, o - r, NA_real_),
                 status = dplyr::case_when(!valid ~ "error", passed ~ "passed",
                                           .default = "issues found"))
}
