# Read bounded ZIP members directly; never extract paths supplied by a workbook.
wb_target <- function(base, target) {
  if (is.na(target) || grepl("[\\\\:]", target)) stop("invalid_part")
  parts <- strsplit(paste0(if (startsWith(target, "/")) "" else base, target), "/")[[1]]
  out <- character()
  for (part in parts) {
    if (part %in% c("", ".")) next
    if (part == "..") {
      if (!length(out)) stop("escaping_part")
      out <- utils::head(out, -1L)
    } else out <- c(out, part)
  }
  paste(out, collapse = "/")
}
wb_reader <- function(path) {
  entries <- suppressWarnings(utils::unzip(path, list = TRUE))
  names <- entries$Name
  if (!length(names) || anyDuplicated(names) || any(grepl("(^/|\\\\|:|(^|/)\\.\\.(/|$))", names)))
    stop("unsafe_or_invalid_zip")
  if (any(entries$Length > 2e7) || sum(entries$Length) > 1e8) stop("zip_size_limit")
  function(part, optional = FALSE) {
    if (!part %in% names) {
      if (optional) return(NULL)
      stop("missing_part")
    }
    con <- unz(path, part, open = "rb")
    on.exit(close(con))
    bytes <- readBin(con, "raw", n = entries$Length[match(part, names)] + 1)
    if (length(bytes) != entries$Length[match(part, names)]) stop("invalid_part_size")
    text <- rawToChar(bytes)
    if (grepl("<!DOCTYPE|<!ENTITY", text, ignore.case = TRUE)) stop("unsafe_xml")
    doc <- suppressWarnings(xml2::read_xml(bytes, options = "NONET"))
    name <- xml2::xml_name(xml2::xml_root(doc))
    uri <- xml2::xml_find_chr(xml2::xml_root(doc), "namespace-uri(.)")
    if ((name %in% c("workbook", "worksheet", "styleSheet", "sst", "table") &&
         !uri %in% c("http://schemas.openxmlformats.org/spreadsheetml/2006/main",
                     "http://purl.oclc.org/ooxml/spreadsheetml/main")) ||
        (name == "Relationships" && uri != "http://schemas.openxmlformats.org/package/2006/relationships")) stop("invalid_namespace")
    doc
  }
}
wb_nodes <- function(doc, xpath) {
  if (is.null(doc)) return(xml2::xml_find_all(xml2::read_xml("<empty/>"), "./absent"))
  uri <- xml2::xml_find_chr(xml2::xml_root(doc), "namespace-uri(.)")
  if (nzchar(uri)) xpath <- gsub("(?<=/)([A-Za-z][A-Za-z0-9_]*)", "m:\\1", xpath, perl = TRUE)
  xml2::xml_find_all(doc, xpath, ns = if (nzchar(uri)) c(m = uri) else character())
}
wb_attr <- function(nodes, name, default = "") {
  value <- xml2::xml_attr(nodes, name)
  value[is.na(value)] <- default
  value
}
wb_relationships <- function(read, part) {
  doc <- read(part, TRUE)
  if (!is.null(doc) && xml2::xml_name(xml2::xml_root(doc)) != "Relationships") stop("invalid_relationship")
  rel <- wb_nodes(doc, "./Relationship")
  if (any(wb_attr(rel, "TargetMode") == "External")) stop("external_relationship")
  ids <- wb_attr(rel, "Id")
  if (any(!nzchar(ids)) || anyDuplicated(ids) || any(!nzchar(wb_attr(rel, "Type")))) stop("invalid_relationship")
  structure(stats::setNames(wb_attr(rel, "Target"), ids), types = stats::setNames(wb_attr(rel, "Type"), ids))
}
wb_id <- function(node) {
  uri <- if (xml2::xml_find_chr(xml2::xml_root(node), "namespace-uri(.)") ==
    "http://purl.oclc.org/ooxml/spreadsheetml/main") "http://purl.oclc.org/ooxml/officeDocument/relationships" else
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
  unname(xml2::xml_attr(node, "r:id", ns = c(r = uri)))
}
wb_relation <- function(rels, id, kind) {
  types <- paste0(c("http://schemas.openxmlformats.org/officeDocument/2006/relationships/",
                    "http://purl.oclc.org/ooxml/officeDocument/relationships/"), kind)
  if (length(id) != 1L || is.na(id) || !id %in% names(rels) ||
      !unname(attr(rels, "types")[id]) %in% types) stop("invalid_relationship")
  unname(rels[id])
}
wb_styles <- function(read) {
  doc <- read("xl/styles.xml", TRUE)
  custom <- wb_nodes(doc, ".//numFmt")
  codes <- stats::setNames(wb_attr(custom, "formatCode"), wb_attr(custom, "numFmtId"))
  xfs <- wb_nodes(doc, ".//cellXfs/xf")
  ids <- wb_attr(xfs, "numFmtId", "0")
  format <- unname(codes[ids])
  # Formatting is evidence of presentation, not of a question's semantic type.
  stripped <- gsub('"[^"]*"|\\\\.|\\[[^]]*\\]', "", format)
  data.frame(style = seq_along(ids) - 1L, num_fmt_id = ids, format = format,
             date_format = ids %in% as.character(c(14:22, 27:36, 45:47, 50:58)) |
               (!is.na(stripped) & grepl("[ymdhis]", stripped, ignore.case = TRUE)))
}

wb_filter_criteria <- function(filter) {
  criteria <- wb_nodes(filter, "./filterColumn/*")
  any(vapply(criteria, function(x) {
    kind <- xml2::xml_name(x)
    if (kind == "filters") return(length(wb_nodes(x, "./filter|./dateGroupItem")) > 0L ||
                                   wb_attr(x, "blank") %in% c("1", "true"))
    if (kind == "customFilters") return(length(wb_nodes(x, "./customFilter")) > 0L)
    if (kind == "dynamicFilter") return(!wb_attr(x, "type") %in% c("", "null"))
    kind %in% c("top10", "colorFilter", "iconFilter")
  }, logical(1)))
}
