#' Reconcile records by complete, unique typed keys
#'
#' Key defects block all matching. Tuples are joined without delimiters.
#' Added/removed columns are reported separately; only shared columns are compared.
#' @param old,new Data frames with unique column names and atomic columns.
#' @param keys Character vector of stable key columns, including round if needed.
#' @param normalisation Named list of vector functions, applied to both deliveries.
#'   Supply `list()` for exact comparison. Functions must preserve type and length.
#' @param authorized_changes Optional table of `old_row`, `new_row`, `column`,
#'   and nonblank `evidence` references. It annotates rather than suppresses changes.
#' @param sensitive Include compared values as list-columns? Default FALSE.
#'   Evidence references are retained verbatim and should contain no personal data.
#' @return List with status, key defects, added/removed row indices, changed cells,
#'   schema differences, denominators and the supplied normalisation functions.
#'   No raw keys or cell values are returned unless `sensitive = TRUE`.
#' @name compare_records
#' @export
compare_records <- function(old, new, keys, normalisation = list(),
                            authorized_changes = NULL, sensitive = FALSE) {
  advanced_frame(old, keys); advanced_frame(new, keys)
  if (!is.null(authorized_changes)) {
    advanced_frame(authorized_changes, c("old_row", "new_row", "column", "evidence"))
    if (!setequal(names(authorized_changes), c("old_row", "new_row", "column", "evidence")) ||
        !is.character(authorized_changes$column) || !all(vapply(authorized_changes[c("old_row", "new_row")],
        function(x) is.numeric(x) && all(is.finite(x) & x >= 1 & x == floor(x)), logical(1)))) stop("Invalid authorization schema.")
    if (!is.character(authorized_changes$evidence) || any(advanced_missing(authorized_changes))) stop("Incomplete authorization evidence.")
  }
  if (!is.logical(sensitive) || length(sensitive) != 1L || is.na(sensitive)) stop("sensitive must be logical.")
  if (!is.list(normalisation) || (length(normalisation) &&
      (is.null(names(normalisation)) || anyDuplicated(names(normalisation)) ||
       !all(names(normalisation) %in% intersect(names(old), names(new)))))) stop("Invalid normalisation.")
  for (col in names(normalisation)) {
    f <- normalisation[[col]]
    if (!is.function(f)) stop("Normalisation entries must be functions.")
    for (side in c("old", "new")) {
      before <- get(side)[[col]]; after <- f(before)
      if (length(after) != length(before) || !advanced_type(before, after)) stop("Normalisation changed type or length.")
      if (side == "old") old[[col]] <- after else new[[col]] <- after
    }
  }
  defects <- dplyr::bind_rows(lapply(c("old", "new"), function(side) {
    x <- get(side)[keys]; missing <- advanced_missing(x)
    repeated <- duplicated(x) | duplicated(x, fromLast = TRUE)
    dplyr::bind_rows(tibble::tibble(side = side, row = which(missing), issue = "incomplete"),
      tibble::tibble(side = side, row = which(repeated), issue = "duplicate"))
  }))
  common <- intersect(names(old), names(new))
  incompatible <- common[!vapply(common, function(k) advanced_type(old[[k]], new[[k]]), logical(1))]
  out <- list(status = "not_applicable", key_defects = defects, added = integer(), removed = integer(),
    changes = tibble::tibble(old_row = integer(), new_row = integer(), column = character()),
    schema = list(added = setdiff(names(new), names(old)), removed = setdiff(names(old), names(new)),
                  incompatible = incompatible), matched = 0L, cells_compared = 0L,
    normalisation = normalisation, evidence = authorized_changes)
  if (nrow(defects) || length(incompatible)) { out$status <- "blocked"; return(out) }
  a <- tibble::as_tibble(old[keys]); b <- tibble::as_tibble(new[keys])
  names(a) <- names(b) <- paste0("k", seq_along(keys))
  a$.old <- seq_len(nrow(old)); b$.new <- seq_len(nrow(new))
  pairs <- dplyr::full_join(a, b, by = paste0("k", seq_along(keys)), relationship = "one-to-one", na_matches = "never")
  out$added <- pairs$.new[is.na(pairs$.old)]; out$removed <- pairs$.old[is.na(pairs$.new)]
  pairs <- pairs[!is.na(pairs$.old) & !is.na(pairs$.new), ]
  out$matched <- nrow(pairs); cols <- setdiff(common, keys)
  out$cells_compared <- nrow(pairs) * length(cols)
  out$changes <- dplyr::bind_rows(out$changes, lapply(cols, function(col) {
    i <- which(!vapply(seq_len(nrow(pairs)), function(j)
      identical(unname(old[[col]][pairs$.old[j]]), unname(new[[col]][pairs$.new[j]])), logical(1)))
    x <- tibble::tibble(old_row = pairs$.old[i], new_row = pairs$.new[i], column = col)
    if (sensitive) { x$old_value <- as.list(old[[col]][x$old_row]); x$new_value <- as.list(new[[col]][x$new_row]) }
    x
  }))
  out$changes$authorized <- FALSE; out$changes$evidence <- NA_character_
  if (!is.null(authorized_changes)) {
    for (i in seq_len(nrow(out$changes))) {
      x <- out$changes[i, ]; hit <- which(authorized_changes$old_row == x$old_row &
        authorized_changes$new_row == x$new_row & authorized_changes$column == x$column)
      if (length(hit)) { out$changes$authorized[i] <- TRUE
        out$changes$evidence[i] <- paste(authorized_changes$evidence[hit], collapse = "; ") }
    }
  }
  out$status <- if (nrow(old) + nrow(new)) "assessed" else "not_applicable"
  out
}
advanced_type <- function(x, y) identical(typeof(x), typeof(y)) && identical(attributes(x)[setdiff(names(attributes(x)), "names")], attributes(y)[setdiff(names(attributes(y)), "names")])
advanced_missing <- function(x) Reduce(`|`, lapply(x, function(v) is.na(v) | ((is.character(v) | is.factor(v)) & trimws(v) == "")))
advanced_frame <- function(data, columns) {
  if (!is.data.frame(data) || anyDuplicated(names(data)) || anyNA(names(data)) ||
      any(names(data) == "") || !is.character(columns) || !length(columns) || anyNA(columns) ||
      anyDuplicated(columns) || !all(columns %in% names(data)) ||
      !all(vapply(data, function(x) is.atomic(x) && is.null(dim(x)), logical(1)))) stop("Expected atomic data frame and unique existing columns.")
  invisible(data)
}
