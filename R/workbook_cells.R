wb_cells <- function(doc, shared, styles) {
  nodes <- wb_nodes(doc, ".//sheetData/row/c")
  ref <- wb_attr(nodes, "r")
  if (any(!grepl("^[A-Z]{1,3}[1-9][0-9]*$", ref)) || anyDuplicated(ref)) stop("invalid_cells")
  col <- vapply(strsplit(sub("[0-9]+$", "", ref), ""), function(x)
    sum((match(x, LETTERS)) * 26^rev(seq_along(x) - 1L)), numeric(1))
  row <- as.integer(sub("^[A-Z]+", "", ref))
  if (anyNA(row) || any(row > 1048576L) || any(col > 16384L)) stop("invalid_cells")
  type <- wb_attr(nodes, "t", "n")
  value <- vapply(nodes, function(n) xml2::xml_text(wb_nodes(n, "./v"))[1], character(1))
  inline <- type == "inlineStr"
  value[inline] <- vapply(nodes[inline], function(n)
    paste(xml2::xml_text(wb_nodes(n, "./is//t")), collapse = ""), character(1))
  indexed <- type == "s"
  if (anyNA(value[indexed]) || any(!grepl("^[0-9]+$", value[indexed]))) stop("invalid_shared_string")
  index <- suppressWarnings(as.double(value[indexed])) + 1
  if (anyNA(index) || any(index < 1L | index > length(shared))) stop("invalid_shared_string")
  value[indexed] <- shared[index]
  style <- suppressWarnings(as.integer(wb_attr(nodes, "s", "0")))
  date <- styles$date_format[match(style, styles$style)]
  date[is.na(date)] <- FALSE
  numeric_text <- type %in% c("s", "inlineStr", "str") & !is.na(value) &
    grepl("^[-+]?(\\d+\\.?\\d*|\\.\\d+)([eE][-+]?\\d+)?$", trimws(value))
  formula <- vapply(nodes, function(n) length(wb_nodes(n, "./f")) > 0L, logical(1))
  formula_text <- vapply(nodes, function(n) xml2::xml_text(wb_nodes(n, "./f"))[1], character(1))
  formula_xml <- vapply(nodes, function(n) { f <- wb_nodes(n, "./f")
    if (length(f)) as.character(f[[1]]) else NA_character_ }, character(1))
  tibble::tibble(ref = ref, row = row, column = col, storage = type, style = style,
                 date_format = date, numeric_text = numeric_text, formula = formula,
                 formula_text = formula_text, formula_xml = formula_xml, value = value,
                 populated = (!is.na(value) & nzchar(trimws(value))) | formula)
}
wb_sheet <- function(read, part, header, shared, styles, date1904 = NULL) {
  doc <- read(part)
  if (xml2::xml_name(xml2::xml_root(doc)) != "worksheet") stop("invalid_worksheet")
  cells <- wb_cells(doc, shared, styles)
  filled <- cells[cells$populated, ]
  data <- filled[filled$row > header, ]
  headers <- cells[cells$row == header, ]
  extent <- max(c(0, headers$column, data$column))
  h <- rep("", extent)
  h[headers$column] <- ifelse(is.na(headers$value), "", headers$value)
  header_facts <- tibble::tibble(column = seq_len(extent), original = h,
    blank = !nzchar(trimws(h)), duplicate = duplicated(trimws(h)) & nzchar(trimws(h)),
    padded = h != trimws(h))
  occupied <- sort(unique(data$row))
  blank <- if (length(occupied)) setdiff(seq.int(header + 1L, max(occupied)), occupied) else integer()
  marker <- grepl("^(grand[ _-]*total|sub[ _-]*total|total|summary|dummy|filler)([ :_-]|$)",
                  trimws(data$value), ignore.case = TRUE)
  marker[is.na(marker)] <- FALSE
  summary <- sort(unique(data$row[marker]))
  merges <- wb_attr(wb_nodes(doc, ".//mergeCell"), "ref")
  endrow <- suppressWarnings(as.integer(sub("^[A-Z]+", "", sub(".*:", "", merges))))
  hidden_rows <- as.integer(wb_attr(wb_nodes(doc, ".//row[@hidden='1' or @hidden='true']"), "r"))
  columns <- wb_nodes(doc, ".//col[@hidden='1' or @hidden='true']")
  filters <- wb_nodes(doc, ".//autoFilter")
  relpart <- paste0(dirname(part), "/_rels/", basename(part), ".rels")
  tables <- wb_nodes(doc, ".//tablePart")
  if (length(tables)) {
    rels <- wb_relationships(read, relpart)
    for (table in tables) {
      id <- wb_id(table)
      target <- wb_target(paste0(dirname(part), "/"), wb_relation(rels, id, "table"))
      filters <- c(filters, wb_nodes(read(target), ".//autoFilter"))
    }
  }
  has_criteria <- any(vapply(filters, wb_filter_criteria, logical(1)))
  findings <- tibble::tibble(code = character(), row = integer(), column = integer())
  add <- function(code, rows = NA_integer_, cols = NA_integer_) {
    findings <<- dplyr::bind_rows(findings, tibble::tibble(code = code, row = rows, column = cols))
  }
  if (length(blank)) add("physical_blank_row", blank)
  if (length(summary)) add("candidate_summary_or_filler", summary)
  if (any(endrow > header, na.rm = TRUE)) add("merged_data_range")
  if (length(hidden_rows) || length(columns)) add("hidden_rows_or_columns")
  if (has_criteria) add("saved_filter_criteria")
  for (flag in c("blank", "duplicate", "padded"))
    if (any(header_facts[[flag]])) add(paste0(flag, "_header"), header, which(header_facts[[flag]]))
  outside <- data$column[!data$column %in% headers$column[nzchar(trimws(headers$value))]]
  if (length(outside)) add("data_outside_named_header", cols = sort(unique(outside)))
  protection <- wb_attr(wb_nodes(doc, "./sheetProtection"), "sheet", "false")
  if (length(protection) > 1L || any(!protection %in% c("0", "1", "false", "true"))) stop("invalid_worksheet")
  list(status = if (nrow(filled)) "inspected" else "empty", header_row = header,
       cell_fingerprint = digest::digest(list(cells = cells, styles = styles, date1904 = date1904),
                                          algo = "sha256"),
       headers = header_facts, cells = cells[, setdiff(names(cells), c("value", "formula_text", "formula_xml"))],
       storage_counts = dplyr::count(data, .data$column, .data$storage, name = "cells"),
       hidden_rows = hidden_rows, hidden_columns = tibble::tibble(
         min = wb_attr(columns, "min"), max = wb_attr(columns, "max")),
       merged_ranges = merges, merged_data_ranges = merges[!is.na(endrow) & endrow > header],
       filter_ranges = vapply(filters, function(f) wb_attr(f, "ref"), character(1)), saved_filter_criteria = has_criteria,
       blank_rows = blank, candidate_summary_rows = summary,
       sheet_protection = any(protection %in% c("1", "true")), findings = findings)
}
