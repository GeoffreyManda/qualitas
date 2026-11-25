#' Inspect the saved structure of an XLSX workbook
#'
#' Reads ZIP XML without extracting files or repairing column names. Findings
#' contain coordinates and codes, never source cell values. Header facts retain
#' original names; metadata retain the workbook's self-reported authorship.
#' Summary and filler labels are candidates requiring review, including below
#' the data block. Date formats describe storage and presentation only.
#'
#' @param path One local XLSX path.
#' @param header_rows A positive integer header row for every sheet, or a named
#'   numeric vector/list mapping sheet names to header rows. Unmapped sheets
#'   are explicitly not inspected. Multi-row headers require a caller profile.
#' @return A list with `status`, sanitized `error`, `applicability`, `facts`
#'   and a findings tibble. Invalid, unreadable and encrypted files have error
#'   status, never a passing assessment. Sheet protection is distinct from
#'   encryption. Numeric/date semantics, naming requirements, authorisation
#'   and undocumented overwriting require external evidence from the caller.
#'   XML members over 20 MB or total expanded ZIP sizes over 100 MB are rejected.
#' @export
inspect_workbook <- function(path, header_rows = 1) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    stop("path must be one nonempty local path", call. = FALSE)
  headers <- unlist(header_rows, use.names = TRUE)
  if (!is.numeric(headers) || !length(headers) || anyNA(headers) ||
      any(!is.finite(headers) | headers < 1 | headers > 1048576 | headers != floor(headers)) ||
      (is.null(names(headers)) && length(headers) != 1L) ||
      (!is.null(names(headers)) && (any(!nzchar(names(headers))) || anyDuplicated(names(headers)))))
    stop("header_rows must be a positive integer or uniquely named sheet mapping", call. = FALSE)
  applicability <- list(structure = "saved_xml", semantic_types = "caller_profile_required",
    requirements = "caller_profile_required", access_authorisation = "external_evidence_required",
    change_authority = "external_evidence_required", encryption = "not_established")
  findings <- tibble::tibble(sheet = character(), code = character(), row = integer(), column = integer())
  result <- list(status = "error", error = NULL, applicability = applicability,
                 facts = list(), findings = findings)
  tryCatch({
    if (!file.exists(path) || dir.exists(path) || file.access(path, 4) != 0) stop("unreadable_file")
    con <- file(path, "rb")
    magic <- tryCatch(readBin(con, "raw", 8), finally = close(con))
    if (identical(magic, as.raw(c(208, 207, 17, 224, 161, 177, 26, 225))))
      stop("compound_file_encryption_unknown")
    if (length(magic) < 4 || !identical(magic[1:4], as.raw(c(80, 75, 3, 4)))) stop("not_zip_xlsx")
    read <- wb_reader(path)
    workbook <- read("xl/workbook.xml")
    if (xml2::xml_name(xml2::xml_root(workbook)) != "workbook") stop("invalid_workbook")
    rels <- wb_relationships(read, "xl/_rels/workbook.xml.rels")
    sheets <- wb_nodes(workbook, "./sheets/sheet")
    sheet_names <- wb_attr(sheets, "name")
    if (!length(sheets) || any(!nzchar(sheet_names)) || anyDuplicated(sheet_names)) stop("invalid_sheets")
    core <- read("docProps/core.xml", TRUE)
    metadata <- lapply(c("creator", "lastModifiedBy", "created", "modified"), function(tag)
      xml2::xml_text(wb_nodes(core, paste0(".//*[local-name()='", tag, "']"))))
    names(metadata) <- c("creator", "lastModifiedBy", "created", "modified")
    shared <- vapply(wb_nodes(read("xl/sharedStrings.xml", TRUE), ".//si"), function(si)
      paste(xml2::xml_text(wb_nodes(si, ".//t")), collapse = ""), character(1))
    styles <- wb_styles(read)
    facts <- stats::setNames(vector("list", length(sheets)), sheet_names)
    for (i in seq_along(sheets)) {
      name <- sheet_names[i]
      header <- if (is.null(names(headers))) headers else unname(headers[name])
      visibility <- wb_attr(sheets[i], "state", "visible")
      if (is.na(header)) {
        facts[[i]] <- list(status = "not_applicable", reason = "header_row_not_supplied", visibility = visibility)
        next
      }
      id <- wb_id(sheets[i])
      target <- wb_target("xl/", wb_relation(rels, id, "worksheet"))
      fact <- wb_sheet(read, target, header, shared, styles,
        wb_attr(wb_nodes(workbook, "./workbookPr"), "date1904", "false"))
      fact$visibility <- visibility
      if (visibility != "visible") fact$findings <- dplyr::bind_rows(fact$findings,
        tibble::tibble(code = "hidden_sheet", row = NA_integer_, column = NA_integer_))
      findings <- dplyr::bind_rows(findings, tibble::as_tibble(c(list(sheet = rep(name, nrow(fact$findings))), fact$findings)))
      fact$findings <- NULL
      facts[[i]] <- fact
    }
    result$status <- if (any(vapply(facts, function(x) x$status == "not_applicable", logical(1)))) "partial" else "inspected"
    result$applicability$encryption <- "zip_xml_readable_not_an_access_control_assessment"
    result$facts <- list(metadata = metadata, date1904 = wb_attr(wb_nodes(workbook, "./workbookPr"), "date1904"),
                         styles = styles, sheets = facts)
    result$findings <- findings
    result
  }, error = function(e) {
    safe <- c("unreadable_file", "not_zip_xlsx", "compound_file_encryption_unknown",
      "unsafe_or_invalid_zip", "zip_size_limit", "missing_part", "unsafe_xml", "invalid_part",
      "escaping_part", "invalid_part_size", "external_relationship", "invalid_cells", "invalid_shared_string",
      "invalid_worksheet", "invalid_workbook", "invalid_sheets", "invalid_namespace", "invalid_relationship")
    result$error <- if (conditionMessage(e) %in% safe) conditionMessage(e) else "invalid_or_unreadable_xlsx"
    result
  })
}
