# Original synthetic XML fixtures; no client data or retained workbook binaries.
wb_fixture <- function(sheet = '<sheetData/>', target = 'worksheets/sheet1.xml',
                       extra = list(), state = 'visible', second = FALSE, strict = FALSE) {
  root <- tempfile('qualitas-workbook-')
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  path <- tempfile(fileext = '.xlsx')
  withr::defer(unlink(path), envir = parent.frame())
  main <- if (strict) 'http://purl.oclc.org/ooxml/spreadsheetml/main' else
    'http://schemas.openxmlformats.org/spreadsheetml/2006/main'
  office <- if (strict) 'http://purl.oclc.org/ooxml/officeDocument/relationships' else
    'http://schemas.openxmlformats.org/officeDocument/2006/relationships'
  package <- 'http://schemas.openxmlformats.org/package/2006/relationships'
  parts <- list(
    'xl/workbook.xml' = paste0('<workbook xmlns="', main, '" xmlns:r="', office,
      '"><workbookPr date1904="1"/><sheets><sheet name="Data" sheetId="1" state="', state,
      '" r:id="rId1"/>', if (second) '<sheet name="Empty" sheetId="2" r:id="rId2"/>', '</sheets></workbook>'),
    'xl/_rels/workbook.xml.rels' = paste0('<Relationships xmlns="', package,
      '"><Relationship Id="rId1" Type="', office, '/worksheet" Target="', target,
      '"/>', if (second) paste0('<Relationship Id="rId2" Type="', office,
      '/worksheet" Target="worksheets/sheet2.xml"/>'),
      '<Relationship Id="styles" Type="', office, '/styles" Target="styles.xml"/>',
      if ('xl/sharedStrings.xml' %in% names(extra)) paste0('<Relationship Id="strings" Type="', office, '/sharedStrings" Target="sharedStrings.xml"/>'), '</Relationships>'),
    'xl/worksheets/sheet1.xml' = paste0('<worksheet xmlns="', main, '">', sheet, '</worksheet>'),
    'xl/styles.xml' = paste0('<styleSheet xmlns="', main, '"><numFmts><numFmt numFmtId="164" formatCode="yyyy-mm-dd"/></numFmts><cellXfs><xf numFmtId="0"/><xf numFmtId="14"/><xf numFmtId="164"/></cellXfs></styleSheet>'),
    'docProps/core.xml' = '<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" xmlns:dc="http://purl.org/dc/elements/1.1/"><dc:creator>Synthetic author</dc:creator><cp:lastModifiedBy>Synthetic saver</cp:lastModifiedBy></cp:coreProperties>',
    '_rels/.rels' = paste0('<Relationships xmlns="', package, '"><Relationship Id="r1" Type="',
      office, '/officeDocument" Target="xl/workbook.xml"/><Relationship Id="core" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/></Relationships>'))
  if (second) parts[['xl/worksheets/sheet2.xml']] <- paste0('<worksheet xmlns="', main, '"><sheetData/></worksheet>')
  parts[names(extra)] <- extra
  types <- vapply(names(parts), function(name) {
    kind <- if (name == 'xl/workbook.xml') 'sheet.main' else if (grepl('^xl/worksheets/', name))
      'worksheet' else if (name == 'xl/styles.xml') 'styles' else if (name == 'xl/sharedStrings.xml')
      'sharedStrings' else if (grepl('^xl/tables/', name)) 'table' else ''
    if (name == 'docProps/core.xml') return('<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>')
    if (!nzchar(kind)) return('')
    paste0('<Override PartName="/', name, '" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.', kind, '+xml"/>')
  }, character(1))
  parts[['[Content_Types].xml']] <- paste0('<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/>', paste(types, collapse = ''), '</Types>')
  for (name in names(parts)) {
    file <- file.path(root, name)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeChar(parts[[name]], file, eos = NULL, useBytes = TRUE)
  }
  old <- setwd(root)
  on.exit(setwd(old))
  utils::zip(path, files = names(parts), flags = '-q')
  path
}
wb_inline <- function(ref, value) paste0('<c r="', ref, '" t="inlineStr"><is><t xml:space="preserve">', value, '</t></is></c>')

test_that('physical workbook facts retain coordinates and separate protection', {
  sheet <- paste0('<cols><col min="2" max="3" hidden="1"/></cols><sheetData>',
    '<row r="1">', wb_inline('A1', ' id '), wb_inline('B1', 'id'), wb_inline('D1', 'when'), '</row>',
    '<row r="2" hidden="1"><c r="A2"><v>1</v></c><c r="C2"><v>2</v></c><c r="D2" s="1"><v>45000</v></c></row>',
    '<row r="3"><c r="A3" s="0"/></row>',
    '<row r="4"><c r="A4"><v>3</v></c><c r="D4" s="2"><v>45001</v></c></row>',
    '<row r="8">', wb_inline('A8', 'Total'), '<c r="B8"><f>SUM(A2:A4)</f><v>4</v></c></row>',
    '</sheetData><mergeCells><mergeCell ref="A1:B1"/><mergeCell ref="A4:B4"/></mergeCells>',
    '<autoFilter ref="A1:D4"><filterColumn colId="0"><filters><filter val="SECRET"/></filters></filterColumn></autoFilter><sheetProtection sheet="1"/>')
  out <- inspect_workbook(wb_fixture(sheet, state = 'veryHidden', second = TRUE))
  expect_identical(out$status, 'inspected')
  s <- out$facts$sheets$Data
  expect_identical(s$visibility, 'veryHidden')
  expect_identical(s$hidden_rows, 2L)
  expect_identical(s$hidden_columns$max, '3')
  expect_identical(s$merged_data_ranges, 'A4:B4')
  expect_true(all(c(3L, 5L, 6L, 7L) %in% s$blank_rows))
  expect_identical(s$candidate_summary_rows, 8L)
  expect_true(s$saved_filter_criteria)
  expect_true(s$sheet_protection)
  expect_identical(out$facts$metadata$lastModifiedBy, 'Synthetic saver')
  expect_identical(out$facts$date1904, '1')
  expect_true(s$headers$duplicate[2])
  expect_true(s$headers$blank[3])
  expect_true(s$headers$padded[1])
  expect_true(all(s$cells$date_format[s$cells$ref %in% c('D2', 'D4')]))
  expect_true('data_outside_named_header' %in% out$findings$code)
  expect_false('value' %in% names(s$cells))
  expect_false(any(grepl('SECRET|Synthetic|Total|45000', unlist(out$findings))))
  expect_identical(out$facts$sheets$Empty$status, 'empty')
  expect_identical(out$applicability$semantic_types, 'caller_profile_required')
  expect_identical(out$applicability$access_authorisation, 'external_evidence_required')
})

test_that('filter ranges and empty filter columns are not saved criteria', {
  for (filter in c('<autoFilter ref="A1:B2"/>', '<autoFilter ref="A1:B2"><filterColumn colId="0"/></autoFilter>', '<autoFilter ref="A1:B2"><filterColumn colId="0"><filters/></filterColumn></autoFilter>')) {
    out <- inspect_workbook(wb_fixture(paste0('<sheetData/>', filter)))
    expect_identical(out$status, 'inspected')
    expect_false(out$facts$sheets$Data$saved_filter_criteria)
    expect_false('saved_filter_criteria' %in% out$findings$code)
  }
})

test_that('named headers preserve original duplicate names and mark omitted sheets', {
  sheet <- paste0('<sheetData><row r="2">', wb_inline('A2', 'x'), wb_inline('B2', 'x'), '</row></sheetData>')
  path <- wb_fixture(sheet, second = TRUE)
  out <- inspect_workbook(path, c(Data = 2))
  expect_identical(out$status, 'partial')
  expect_identical(out$facts$sheets$Data$headers$original, c('x', 'x'))
  expect_identical(out$facts$sheets$Empty$status, 'not_applicable')
  expect_identical(inspect_workbook(path, list(Data = 2))$status, 'partial')
  for (header in list(0, NA_real_, 1.5, c(1, 2), c(Data = 1, Data = 2), '1', Inf))
    expect_error(inspect_workbook(path, header), 'header_rows')
})

test_that('unsafe relationships, XML, and non-workbooks never pass or leak XML', {
  for (target in c('../../outside.xml', '/../../outside.xml', 'C:/outside.xml', 'https://example.test/a')) {
    out <- inspect_workbook(wb_fixture(target = target))
    expect_identical(out$status, 'error')
    expect_true(out$error %in% c('escaping_part', 'invalid_part'))
  }
  for (xml in c('<worksheet><PRIVATE', '<!DOCTYPE worksheet [<!ENTITY x SYSTEM "file:///etc/passwd">]><worksheet>&x;</worksheet>')) {
    out <- inspect_workbook(wb_fixture(extra = list('xl/worksheets/sheet1.xml' = xml)))
    expect_identical(out$status, 'error')
    expect_false(grepl('PRIVATE|passwd', out$error))
  }
  expect_identical(inspect_workbook(tempfile())$error, 'unreadable_file')
  path <- tempfile()
  on.exit(unlink(path))
  writeBin(as.raw(c(208, 207, 17, 224, 161, 177, 26, 225)), path)
  expect_identical(inspect_workbook(path)$error, 'compound_file_encryption_unknown')
  writeLines('not a workbook', path)
  expect_identical(inspect_workbook(path)$error, 'not_zip_xlsx')
  out <- inspect_workbook(wb_fixture(extra = list('xl/worksheets/sheet1.xml' = '<other/>')))
  expect_identical(out$error, 'invalid_worksheet')
})

test_that('archive paths are rejected without extraction', {
  expect_identical(wb_target('xl/', '../safe.xml'), 'safe.xml')
  expect_error(wb_target('xl/', '../../escape.xml'), 'escaping_part')
  # Patch an original stored ZIP's equal-length member name to plant traversal.
  path <- wb_fixture(extra = list('evilname.xml' = '<fake/>'))
  bytes <- readBin(path, 'raw', n = file.info(path)$size)
  from <- charToRaw('evilname.xml')
  to <- charToRaw('../eviln.xml')
  at <- which(vapply(seq_len(length(bytes) - length(from) + 1L), function(i)
    identical(bytes[i + seq_along(from) - 1L], from), logical(1)))
  for (i in at) bytes[i + seq_along(to) - 1L] <- to
  writeBin(bytes, path)
  expect_identical(inspect_workbook(path)$error, 'unsafe_or_invalid_zip')
})

test_that('shared strings, ISO dates, formulas and mixed storage are facts', {
  sheet <- '<sheetData><row r="1"><c r="A1" t="s"><v>0</v></c></row><row r="2"><c r="A2"><v>3</v></c></row><row r="3"><c r="A3" t="s"><v>1</v></c></row><row r="4"><c r="A4" t="d"><v>2020-01-01</v></c></row><row r="5"><c r="A5"><f>NOW()</f></c></row></sheetData>'
  out <- inspect_workbook(wb_fixture(sheet, extra = list('xl/sharedStrings.xml' = '<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><si><t>number</t></si><si><r><t>12</t></r><r><t>3</t></r></si></sst>')))
  expect_identical(out$status, 'inspected')
  expect_identical(out$facts$sheets$Data$headers$original, 'number')
  expect_identical(out$facts$sheets$Data$cells$storage, c('s', 'n', 's', 'd', 'n'))
  expect_true(out$facts$sheets$Data$cells$numeric_text[3])
  expect_false(out$facts$sheets$Data$cells$numeric_text[2])
  expect_equal(sum(out$facts$sheets$Data$storage_counts$cells), 4)
  expect_true(out$facts$sheets$Data$cells$formula[5])
  expect_length(out$facts$sheets$Data$blank_rows, 0)
})

test_that('table saved criteria are inspected', {
  sheet <- '<sheetData/><tableParts><tablePart xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" r:id="t1"/></tableParts>'
  out <- inspect_workbook(wb_fixture(sheet, extra = list(
    'xl/worksheets/_rels/sheet1.xml.rels' = '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="t1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/table" Target="../tables/table1.xml"/></Relationships>',
    'xl/tables/table1.xml' = '<table xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><autoFilter ref="A1:A3"><filterColumn colId="0"><customFilters><customFilter operator="greaterThan" val="1"/></customFilters></filterColumn></autoFilter></table>')))
  expect_identical(out$status, 'inspected')
  expect_true(out$facts$sheets$Data$saved_filter_criteria)
})


test_that('missing and external parts and invalid coordinates return explicit errors', {
  out <- inspect_workbook(wb_fixture(target = 'worksheets/missing.xml'))
  expect_identical(out$error, 'missing_part')
  out <- inspect_workbook(wb_fixture(extra = list(
    'xl/_rels/workbook.xml.rels' = '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="https://example.test/a" TargetMode="External"/></Relationships>')))
  expect_identical(out$error, 'external_relationship')
  for (ref in c('XFE1', 'A1048577', 'A0', 'private-cell')) {
    out <- inspect_workbook(wb_fixture(paste0('<sheetData><row r="1"><c r="', ref, '"><v>1</v></c></row></sheetData>')))
    expect_identical(out$error, 'invalid_cells')
  }
  out <- inspect_workbook(wb_fixture('<sheetData><row r="1"><c r="A1" t="s"><v>99</v></c></row></sheetData>'))
  expect_identical(out$error, 'invalid_shared_string')
  out <- inspect_workbook(wb_fixture('<sheetData><row r="1"><c r="A1"/><c r="A1"/></row></sheetData>'))
  expect_identical(out$error, 'invalid_cells')
})

test_that('fingerprints detect formula text, shared formula attributes and date facts', {
  sheet <- function(cell) paste0('<sheetData><row r="1">', wb_inline('A1', 'header'),
    '</row><row r="2">', cell, '</row></sheetData>')
  inspect <- function(cell, ...) inspect_workbook(wb_fixture(sheet(cell), ...))
  fingerprint <- function(x) x$facts$sheets$Data$cell_fingerprint
  a <- inspect('<c r="A2"><f>1+1</f><v>2</v></c>')
  b <- inspect('<c r="A2"><f>999-997</f><v>2</v></c>')
  expect_identical(a$status, 'inspected'); expect_identical(b$status, 'inspected')
  expect_false(identical(fingerprint(a), fingerprint(b)))
  expect_identical(fingerprint(a), fingerprint(inspect('<c r="A2"><f>1+1</f><v>2</v></c>')))
  a <- inspect('<c r="A2"><f t="shared" si="0" ref="A2:A3">1+1</f><v>2</v></c>')
  b <- inspect('<c r="A2"><f t="shared" si="1" ref="A2:A3">1+1</f><v>2</v></c>')
  expect_false(identical(fingerprint(a), fingerprint(b)))
  a <- inspect('<c r="A2" s="0"><v>45000</v></c>')
  b <- inspect('<c r="A2" s="1"><v>45000</v></c>')
  expect_false(identical(fingerprint(a), fingerprint(b)))
  expect_identical(a$facts$sheets$Data$cells$date_format[2], FALSE)
  expect_identical(b$facts$sheets$Data$cells$date_format[2], TRUE)
  styles <- '<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><cellXfs><xf numFmtId="14"/></cellXfs></styleSheet>'
  b <- inspect('<c r="A2" s="0"><v>45000</v></c>', extra = list('xl/styles.xml' = styles))
  expect_false(identical(fingerprint(a), fingerprint(b)))
  epoch <- '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><workbookPr date1904="0"/><sheets><sheet name="Data" sheetId="1" r:id="rId1"/></sheets></workbook>'
  b <- inspect('<c r="A2" s="0"><v>45000</v></c>', extra = list('xl/workbook.xml' = epoch))
  expect_false(identical(fingerprint(a), fingerprint(b)))
  expect_false(any(c('value', 'formula_text', 'formula_xml') %in% names(a$facts$sheets$Data$cells)))
})

test_that('transitional and strict namespaces work and spoofed namespaces do not', {
  for (strict in c(FALSE, TRUE)) {
    out <- inspect_workbook(wb_fixture(paste0('<sheetData><row r="1">', wb_inline('A1', 'id'),
      '</row><row r="2"><c r="A2"><v>1</v></c></row></sheetData>'), strict = strict))
    expect_identical(out$status, 'inspected')
    expect_identical(out$facts$sheets$Data$headers$original, 'id')
    expect_equal(out$facts$sheets$Data$cells$row, c(1L, 2L))
  }
  for (uri in c('urn:not-ooxml', '')) {
    xml <- paste0('<workbook xmlns="', uri, '" xmlns:r="urn:not-relationships"><sheets><sheet name="Data" r:id="rId1"/></sheets></workbook>')
    out <- inspect_workbook(wb_fixture(extra = list('xl/workbook.xml' = xml)))
    expect_identical(out$error, 'invalid_namespace')
    out <- inspect_workbook(wb_fixture(extra = list('xl/worksheets/sheet1.xml' = paste0('<worksheet xmlns="', uri, '"><sheetData/></worksheet>'))))
    expect_identical(out$error, 'invalid_namespace')
  }
  xml <- '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="urn:fake"><sheets><sheet name="Data" r:id="rId1"/></sheets></workbook>'
  expect_identical(inspect_workbook(wb_fixture(extra = list('xl/workbook.xml' = xml)))$error, 'invalid_relationship')
})

test_that('relationship IDs are unique and referenced relationship types are exact', {
  relation <- function(body) paste0('<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">', body, '</Relationships>')
  office <- 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/'
  for (body in c(
    paste0('<Relationship Id="rId1" Type="', office, 'worksheet" Target="worksheets/sheet1.xml"/><Relationship Id="rId1" Type="', office, 'worksheet" Target="worksheets/missing.xml"/>'),
    paste0('<Relationship Id="rId1" Type="', office, 'styles" Target="worksheets/sheet1.xml"/>'),
    '<Relationship Id="rId1" Target="worksheets/sheet1.xml"/>')) {
    out <- inspect_workbook(wb_fixture(extra = list('xl/_rels/workbook.xml.rels' = relation(body))))
    expect_identical(out$error, 'invalid_relationship')
  }
  for (xml in c('<Relationships><Relationship Id="rId1" Type="worksheet" Target="worksheets/sheet1.xml"/></Relationships>',
                '<Other xmlns="http://schemas.openxmlformats.org/package/2006/relationships"/>')) {
    out <- inspect_workbook(wb_fixture(extra = list('xl/_rels/workbook.xml.rels' = xml)))
    expect_identical(out$status, 'error')
    expect_true(out$error %in% c('invalid_namespace', 'invalid_relationship'))
  }
  sheet <- '<sheetData/><tableParts><tablePart xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" r:id="t1"/></tableParts>'
  out <- inspect_workbook(wb_fixture(sheet, extra = list(
    'xl/worksheets/_rels/sheet1.xml.rels' = relation(paste0('<Relationship Id="t1" Type="', office, 'worksheet" Target="../tables/table1.xml"/>')),
    'xl/tables/table1.xml' = '<table xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"/>')))
  expect_identical(out$error, 'invalid_relationship')
})

test_that('shared-string indices never truncate fractional or malformed input', {
  shared <- list('xl/sharedStrings.xml' = '<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><si><t>HEADER</t></si></sst>')
  for (value in c('0.9', '-0.1', '1e0', '-1', 'x', '2147483647', '')) {
    sheet <- paste0('<sheetData><row r="1"><c r="A1" t="s"><v>', value, '</v></c></row></sheetData>')
    expect_identical(inspect_workbook(wb_fixture(sheet, extra = shared))$error, 'invalid_shared_string')
  }
  sheet <- '<sheetData><row r="1"><c r="A1" t="s"><v>0</v></c></row></sheetData>'
  out <- inspect_workbook(wb_fixture(sheet, extra = shared))
  expect_identical(out$status, 'inspected')
  expect_identical(out$facts$sheets$Data$headers$original, 'HEADER')
})

test_that('sheet protection follows the sheet boolean, including its default', {
  for (attribute in c('sheet="0"', 'sheet="false"', '')) {
    out <- inspect_workbook(wb_fixture(paste0('<sheetData/><sheetProtection ', attribute, '/>')))
    expect_identical(out$status, 'inspected')
    expect_false(out$facts$sheets$Data$sheet_protection)
  }
  for (attribute in c('sheet="1"', 'sheet="true"')) {
    out <- inspect_workbook(wb_fixture(paste0('<sheetData/><sheetProtection ', attribute, '/>')))
    expect_true(out$facts$sheets$Data$sheet_protection)
    expect_identical(out$applicability$encryption, 'zip_xml_readable_not_an_access_control_assessment')
  }
  expect_identical(inspect_workbook(wb_fixture('<sheetData/><sheetProtection sheet="maybe"/>'))$error,
                   'invalid_worksheet')
})

test_that('blank rows run from the declared header through the last populated data row', {
  sheet <- paste0('<sheetData><row r="1">', wb_inline('A1', 'id'),
    '</row><row r="3"><c r="A3"><v>1</v></c></row></sheetData>')
  out <- inspect_workbook(wb_fixture(sheet))
  expect_identical(out$facts$sheets$Data$blank_rows, 2L)
  expect_identical(out$findings$row[out$findings$code == 'physical_blank_row'], 2L)
  expect_length(inspect_workbook(wb_fixture('<sheetData/>'))$facts$sheets$Data$blank_rows, 0L)
  sheet <- paste0('<sheetData><row r="2">', wb_inline('A2', 'id'),
    '</row><row r="4"><c r="A4"><v>1</v></c></row><row r="5"/></sheetData>')
  out <- inspect_workbook(wb_fixture(sheet), header_rows = 2)
  expect_identical(out$facts$sheets$Data$blank_rows, 3L)
})
