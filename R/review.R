#' Record review decisions against an assessment snapshot
#'
#' Finding identifiers bind the source SHA-256, check, row and column. Review
#' never removes flags or changes scores. Reordering the source requires a
#' new source hash and review. The caller supplies the hash of the exact file
#' assessed; this function cannot establish that file's provenance itself.
#' @param x An assessment from [assess()].
#' @param source_sha256 A 64-character SHA-256 of the assessed source file.
#' @param decisions Optional data frame with `finding_id`, `decision`,
#'   `reviewer`, `reviewed_at`, `reason`, and `evidence`. Decisions are
#'   `confirmed`, `explained`, or `pending`. Evidence is a document locator,
#'   not a copy of identifying data. Confirmed and explained require every field.
#' @param dataset_locator Stable sheet or table locator within the source file;
#'   defaults to the assessment name. It must distinguish sheets in one workbook.
#' @return The assessment issues with finding IDs and recorded decisions.
#'   Unreviewed issues remain pending. Assessment measures are unchanged.
#' @export
#' @examples
#' result <- assess(data.frame(id = c(1, 1)), checks = "UN01")
#' review_findings(result, paste(rep("a", 64), collapse = ""))
review_findings <- function(x, source_sha256, decisions = NULL, dataset_locator = paste(x$name, collapse = " ")) {
  if (length(source_sha256) != 1 || is.na(source_sha256) ||
      !grepl("^[a-fA-F0-9]{64}$", source_sha256)) cli::cli_abort("Supply one source SHA-256.")
  found <- x$issues
  if (!is.character(dataset_locator) || length(dataset_locator) != 1 || is_blank(dataset_locator)) {
    cli::cli_abort("Supply a stable dataset or sheet locator.")
  }
  contract <- digest::digest(list(dataset_locator, x$spec, x$options,
    as.character(utils::packageVersion("qualitas"))), algo = "sha256")
  identity <- purrr::map_chr(seq_len(nrow(found)), \(i)
    digest::digest(list(tolower(source_sha256), contract, found$check_id[i], found$row[i],
                        found$column[i], found$message[i]), algo = "sha256"))
  if (anyDuplicated(identity)) cli::cli_abort("Assessment has indistinguishable duplicate findings.")
  found$finding_id <- identity
  fields <- c("finding_id", "decision", "reviewer", "reviewed_at", "reason", "evidence")
  if (is.null(decisions)) decisions <- as.data.frame(stats::setNames(rep(list(character()), 6), fields))
  if (!all(fields %in% names(decisions)) || anyDuplicated(decisions$finding_id) ||
      any(is_blank(decisions$finding_id)) || !all(decisions$finding_id %in% identity)) {
    cli::cli_abort("Decisions must uniquely name findings in this source snapshot.")
  }
  if (!all(decisions$decision %in% c("confirmed", "explained", "pending"))) {
    cli::cli_abort("Decision must be confirmed, explained or pending.")
  }
  final <- decisions[decisions$decision != "pending", fields, drop = FALSE]
  if (nrow(required_metadata(final, fields))) {
    cli::cli_abort("Final decisions require reviewer, date, reason and evidence.")
  }
  if (any(!valid_timestamp(final$reviewed_at, allow_date = TRUE))) {
    cli::cli_abort("Final decision dates must be valid dates or timestamps with an offset.")
  }
  dplyr::left_join(found, decisions[fields], by = "finding_id", relationship = "one-to-one", na_matches = "never") |>
    dplyr::mutate(source_sha256 = tolower(source_sha256), dataset_locator = dataset_locator,
                   assessment_sha256 = contract,
                   decision = dplyr::coalesce(.data$decision, "pending"))
}
