# The Word form of a report, written with officer and flextable: black
# Times New Roman type, headings set in bold rather than taken from the
# template's coloured heading styles, and plain tables with thin black rules.

# A report as a Word document.
report_docx <- function(sections, file) {
  rlang::check_installed(c("officer", "flextable"), reason = "to write a Word report.")
  document <- officer::read_docx() |>
    officer::body_add_fpar(docx_text(sections$title, size = 16, bold = TRUE)) |>
    officer::body_add_fpar(docx_text(sections$opening))
  document <- purrr::reduce(sections$tables, docx_section, .init = document)
  document |>
    officer::body_add_fpar(docx_text("Method", size = 12, bold = TRUE)) |>
    officer::body_add_fpar(docx_text(sections$method)) |>
    print(target = file)
}

# One headed table of a report, added to the document.
docx_section <- function(document, part) {
  document <- document |>
    officer::body_add_fpar(docx_text(part$heading, size = 12, bold = TRUE)) |>
    flextable::body_add_flextable(docx_table(part$table))
  if (is.null(part$note)) return(document)
  officer::body_add_fpar(document, docx_text(part$note, size = 9))
}

# A paragraph of black Times New Roman text.
docx_text <- function(text, size = 10.5, bold = FALSE) {
  officer::fpar(officer::ftext(text, officer::fp_text(font.family = "Times New Roman",
                                                      font.size = size, bold = bold,
                                                      color = "black")))
}

# A data frame as a plain table: 9 point type, thin black rules, padding of
# 6 points above and below and 8 at the sides.
docx_table <- function(table) {
  shown <- if (nrow(table) == 0) tibble::tibble(note = "None.") else table
  rule <- officer::fp_border(color = "black", width = 0.5)
  flextable::flextable(as.data.frame(dplyr::mutate(shown, dplyr::across(dplyr::everything(),
                                                                        as_text)))) |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::color(color = "black", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::border_outer(border = rule, part = "all") |>
    flextable::border_inner(border = rule, part = "all") |>
    flextable::padding(padding.top = 6, padding.bottom = 6, padding.left = 8,
                       padding.right = 8, part = "all") |>
    flextable::autofit()
}
