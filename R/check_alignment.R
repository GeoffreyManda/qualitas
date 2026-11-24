# The check of row alignment, documented with the structure checks. Data
# pasted together from batches exported in different layouts leave some
# records with every value one or more columns away from its header: a date
# under a name, text under a number. Each column's usual kind of value
# (number, date or text) is the kind at least `alignment_share` of its
# filled values have; a record is flagged when at least
# `alignment_record_share` of its values in columns that usually hold numbers
# or dates, and at least `alignment_min_cells` of them, are of another kind.

#' @rdname structure_checks
#' @export
check_row_alignment <- function(data, spec, options) {
  kinds <- purrr::map(data, value_kind)
  usual <- purrr::map_chr(kinds, \(k) usual_kind(k, options$alignment_share, options$group_min_n))
  # Free text accepts any value, so only columns that usually hold numbers or
  # dates show a value under the wrong header.
  profiled <- names(usual)[usual %in% c("number", "date")]
  if (length(profiled) < options$alignment_min_columns) return(check_result())
  off <- purrr::map(profiled, \(c) !is.na(kinds[[c]]) & kinds[[c]] != usual[[c]]) |>
    purrr::reduce(`+`)
  filled <- purrr::reduce(purrr::map(profiled, \(c) !is.na(kinds[[c]])), `+`)
  share <- off / pmax(filled, 1)
  bad <- which(off >= options$alignment_min_cells & share >= options$alignment_record_share)
  check_result(tibble::tibble(
    row = bad, value = sprintf("%d of %d", off[bad], filled[bad]),
    message = sprintf(paste("%d of the record's %d values are not of their column's usual",
                            "kind; its values may sit under the wrong headers"),
                      off[bad], filled[bad])),
    checked = nrow(data))
}

# The kind of each value: number, date or text; missing when blank.
value_kind <- function(x) {
  text <- trimws(as_text(x))
  dplyr::case_when(is_blank(x) ~ NA_character_,
                   !is.na(as_number(text)) ~ "number",
                   !is.na(as_day(text)) & grepl("[-/.]", text) ~ "date",
                   .default = "text")
}

# The kind of value a column usually holds, or missing when it has fewer
# than `min_n` filled values or no kind reaches the given share of them.
usual_kind <- function(kinds, share, min_n) {
  kept <- kinds[!is.na(kinds)]
  if (length(kept) < min_n) return(NA_character_)
  counts <- sort(table(kept), decreasing = TRUE)
  if (counts[[1]] / length(kept) < share) NA_character_ else names(counts)[1]
}
