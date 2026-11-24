# Checks of uniqueness: records that repeat exactly, keys that repeat, and
# records that match another on nearly all their values.

#' Checks of uniqueness
#'
#' `check_near_duplicates()` follows Kuriakose and Robbins (2016): for each
#' record it finds the largest share of values it shares with any other
#' record, over the columns that can repeat (identifiers and columns whose
#' values are nearly all different are left out), and flags shares at or
#' above `near_duplicate_share`. Exact repeats are left to
#' `check_duplicate_rows()`. It compares every pair of records, so it runs on
#' at most 5,000 records.
#'
#' @inheritParams structure_checks
#' @return A [check_result()].
#' @name uniqueness_checks
#' @examples
#' data <- data.frame(id = c(1, 2, 2), x = c("a", "b", "b"))
#' check_duplicate_rows(data, infer_spec(data), qualitas_options())
NULL

#' @rdname uniqueness_checks
#' @export
check_duplicate_rows <- function(data, spec, options) {
  keys <- do_paste(data)
  first <- match(keys, keys)
  repeat_rows <- which(duplicated(keys))
  check_result(tibble::tibble(row = repeat_rows,
                              message = paste("the record repeats record", first[repeat_rows])),
               checked = nrow(data))
}

#' @rdname uniqueness_checks
#' @export
check_duplicate_keys <- function(data, spec, options) {
  keys <- dplyr::filter(spec_present(spec, data), .data$unique)
  found <- per_column(data, keys, \(x, row) cell_issues(
    x, row$column, !is_blank(x) & (duplicated(x) | duplicated(x, fromLast = TRUE)),
    "the key value appears in more than one record"))
  check_result(found, checked = nrow(data) * nrow(keys))
}

#' @rdname uniqueness_checks
#' @export
check_near_duplicates <- function(data, spec, options) {
  ids <- columns_with_role(spec, data, "id")
  usable <- names(data)[!names(data) %in% ids &
                          purrr::map_lgl(data, \(x) dplyr::n_distinct(x) <= 0.9 * nrow(data))]
  if (nrow(data) < 3 || nrow(data) > 5000 || length(usable) < 5) return(check_result())
  share <- match_shares(data[usable])
  best <- purrr::map_dbl(seq_len(nrow(share)), \(i) max(share[i, ]))
  partner <- purrr::map_int(seq_len(nrow(share)), \(i) which.max(share[i, ]))
  near <- which(best >= options$near_duplicate_share & best < 1)
  said <- sprintf("the record shares %.0f%% of its values with record %d",
                  100 * best[near], partner[near])
  check_result(tibble::tibble(row = near, value = sprintf("%.0f%%", 100 * best[near]),
                              message = said),
               checked = nrow(data))
}

# The share of values each pair of records has in common, with a record's
# match with itself set to zero.
match_shares <- function(data) {
  same <- purrr::map(data, \(x) outer(as_text(x), as_text(x), `==`) %in% TRUE)
  share <- matrix(purrr::reduce(same, `+`), nrow(data)) / ncol(data)
  diag(share) <- 0
  share
}

# Every record's values joined into one key, so records can be compared
# whole.
do_paste <- function(data) {
  if (ncol(data) == 0) return(rep("", nrow(data)))
  purrr::reduce(purrr::map(data, \(x) dplyr::coalesce(as_text(x), "<NA>")),
                \(a, b) paste(a, b, sep = "\u001f"))
}
