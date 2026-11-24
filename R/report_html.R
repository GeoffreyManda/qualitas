# The HTML form of a report: one page, no scripts, plain black type.

# A report as a self-contained HTML page in plain black type.
report_html <- function(sections, file) {
  tables <- purrr::map_chr(sections$tables, \(part) paste0(
    "<h2>", html_escape(part$heading), "</h2>\n", html_table(part$table),
    if (is.null(part$note)) "" else paste0("<p class=\"note\">", html_escape(part$note), "</p>")))
  page <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<title>", html_escape(sections$title), "</title><style>",
    "body{font-family:'Times New Roman',serif;color:#000;max-width:70rem;margin:2rem auto;",
    "padding:0 1rem;line-height:1.4}table{border-collapse:collapse;margin:0.5rem 0;",
    "font-size:0.9rem}th,td{border:1px solid #000;padding:6px 8px;text-align:left;",
    "vertical-align:top}.note{font-size:0.9rem}</style></head><body>\n",
    "<h1>", html_escape(sections$title), "</h1>\n<p>", html_escape(sections$opening), "</p>\n",
    paste(tables, collapse = "\n"), "\n<h2>Method</h2>\n<p>", html_escape(sections$method),
    "</p>\n</body></html>\n")
  writeLines(page, file, useBytes = TRUE)
}

# A data frame as an HTML table.
html_table <- function(table) {
  if (nrow(table) == 0) return("<p>None.</p>")
  cells <- purrr::map(table, \(x) html_escape(dplyr::coalesce(as_text(x), "")))
  rows <- do.call(paste0, purrr::map(cells, \(x) paste0("<td>", x, "</td>")))
  paste0("<table><thead><tr>", paste0("<th>", html_escape(names(table)), "</th>", collapse = ""),
         "</tr></thead><tbody>", paste0("<tr>", rows, "</tr>", collapse = "\n"),
         "</tbody></table>")
}

# Text with the characters HTML reserves written as entities.
html_escape <- function(text) {
  text |>
    stringr::str_replace_all("&", "&amp;") |>
    stringr::str_replace_all("<", "&lt;") |>
    stringr::str_replace_all(">", "&gt;") |>
    stringr::str_replace_all("\"", "&quot;")
}
