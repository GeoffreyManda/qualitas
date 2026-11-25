#' Check composite keys and relational cardinality
#'
#' Checks complete unique primary keys and many-to-one references without
#' concatenating key values or coercing their types. A missing foreign key is
#' a defect unless its relation declares `required = FALSE`; incomplete keys
#' never match. Duplicate parent keys and incompatible types are defects.
#' @param tables A uniquely named list of data frames.
#' @param keys A named list of character vectors, one primary key per table.
#' @param relations Optional data frame with `from`, `table`, and list-columns
#'   `columns` and `keys`; optional logical `required` defaults to TRUE.
#' @return A tibble of defects with `requirement`, `row`, `message`, and `table`.
#' @export
#' @examples
#' assess_keys(list(sites = data.frame(ia = c("a", "a"), id = c("1", "1"))),
#'             keys = list(sites = c("ia", "id")))
assess_keys <- function(tables, keys, relations = NULL) {
  if (is.null(names(tables)) || any(is_blank(names(tables))) || anyDuplicated(names(tables)) ||
      !all(purrr::map_lgl(tables, is.data.frame))) {
    cli::cli_abort("Tables must be uniquely named data frames.")
  }
  if (length(keys) && (is.null(names(keys)) || anyDuplicated(names(keys)) ||
      !all(names(keys) %in% names(tables)))) cli::cli_abort("Keys must name existing tables.")
  primary <- purrr::imap(keys, \(cols, name) {
    data <- key_frame(tables[[name]], cols)
    blank <- key_blank(data)
    repeated <- duplicated(data) | duplicated(data, fromLast = TRUE)
    dplyr::bind_rows(contract_defects("primary_missing", which(blank), "primary key is incomplete"),
      contract_defects("primary_duplicate", which(repeated), "primary key repeats")) |>
      dplyr::mutate(table = name)
  })
  linked <- if (is.null(relations)) list() else purrr::map(seq_len(nrow(relations)), \(i)
    relation_defects(tables, relations[i, , drop = FALSE]))
  dplyr::bind_rows(tibble::tibble(requirement = character(), row = integer(),
                                  message = character(), table = character()),
                    purrr::list_rbind(unname(primary)), purrr::list_rbind(linked))
}

key_frame <- function(data, columns) {
  if (!is.character(columns) || !length(columns) || anyDuplicated(columns) ||
      !all(columns %in% names(data))) cli::cli_abort("Every key must name existing unique columns.")
  data[columns]
}

key_blank <- function(data) Reduce(`|`, lapply(data, is_blank))

relation_defects <- function(tables, link) {
  if (!all(c("from", "table", "columns", "keys") %in% names(link)) ||
      !all(c(link$from, link$table) %in% names(tables))) {
    cli::cli_abort("Relations must name existing tables and both key column lists.")
  }
  child <- key_frame(tables[[link$from]], link$columns[[1]])
  parent <- key_frame(tables[[link$table]], link$keys[[1]])
  if (ncol(child) != ncol(parent)) cli::cli_abort("Related keys must have the same width.")
  names(parent) <- names(child)
  found <- contract_defects(character(), integer(), character())
  required <- if ("required" %in% names(link)) link$required[[1]] else TRUE
  if (!is.logical(required) || length(required) != 1 || is.na(required)) {
    cli::cli_abort("Relation required must be TRUE or FALSE.")
  }
  if (required) found <- contract_defects("foreign_missing", which(key_blank(child)),
                                         "required foreign key is incomplete")
  if (any(duplicated(parent))) found <- dplyr::bind_rows(found,
    contract_defects("parent_duplicate", NA_integer_, "referenced key is not unique"))
  same_type <- purrr::map2_lgl(child, parent, \(x, y)
    identical(class(x), class(y)) && identical(typeof(x), typeof(y)))
  if (!all(same_type)) {
    found <- dplyr::bind_rows(found,
      contract_defects("foreign_type", NA_integer_, "related key types differ"))
  } else {
    row_column <- utils::tail(make.unique(c(names(child), ".qualitas_row")), 1)
    child[[row_column]] <- seq_len(nrow(child))
    unmatched <- dplyr::anti_join(child[!key_blank(child[names(parent)]), , drop = FALSE],
                                  parent, by = names(parent), na_matches = "never")
    found <- dplyr::bind_rows(found, contract_defects("foreign_unmatched",
      unmatched[[row_column]], "foreign key has no parent"))
  }
  dplyr::mutate(found, table = link$from)
}
