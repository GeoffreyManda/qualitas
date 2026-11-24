# Shapes common in survey-platform exports: headers that join a variable
# name and a label, timestamps written month first with AM and PM, and
# interview length known only from start and end times. All values here are
# made up.

test_that("dates are read in ISO, month-first and day-first forms", {
  expect_equal(format(as_time(c("10/30/2024 10:45:34 PM", "10/30/2024 12:05:00 AM"))),
               c("2024-10-30 22:45:34", "2024-10-30 00:05:00"))
  expect_equal(format(as_day(c("30/10/2024", "01/02/2024"))), c("2024-10-30", "2024-02-01"))
  expect_equal(format(as_day(c("2024-10-30", "bad"))), c("2024-10-30", NA))
  expect_equal(infer_type(c("10/30/2024 10:45:34 PM", "11/01/2024 9:00:00 AM")), "date")
  # Long free text is not a date, and does not stop the parser.
  expect_true(is.na(as_day(strrep("observed finding ", 120))))
})

test_that("roles are found in the words of compound headers", {
  headers <- c("pk_form | Id", "createdat_form | Created At", "endat | EndAt",
               "AssignedTo | Assigned To", "fk_district_form | District",
               "lastupdatedat_form | Last Updated At", "1.1 Name of the project")
  expect_equal(infer_role(headers),
               c("id", "start", "end", "enumerator", "group", "date", ""))
})

test_that("interview length comes from start and end times", {
  data <- data.frame(
    created = sprintf("10/%02d/2024 9:00:00 AM", 1:20),
    ended = c(sprintf("10/%02d/2024 9:45:00 AM", 1:18), "10/19/2024 9:02:00 AM",
              "10/20/2024 8:50:00 AM"),
    answer = "yes")
  spec <- spec_complete(tibble::tibble(column = c("created", "ended", "answer"),
                                       type = c("datetime", "datetime", "text"),
                                       role = c("start", "end", "")))
  found <- check_short_duration(data, spec, qualitas_options())$issues
  expect_equal(found$row, c(19, 20))
  expect_match(found$message[2], "ends before it starts")
})

test_that("names that point to a person are personal data; project names are not", {
  data <- data.frame(a = 1, b = 1, c = 1, d = 1, e = 1)
  names(data) <- c("Respondent name", "Mobile number", "1. Project name",
                   "3. Contracting company name", "enumeratorname | Enumerator name")
  found <- check_pii_columns(data, infer_spec(data), qualitas_options())$issues
  expect_setequal(found$column, c("Respondent name", "Mobile number",
                                  "enumeratorname | Enumerator name"))
})

test_that("dates and numbers are read value by value", {
  expect_equal(as.character(as_day(c("31/02/2026", "2026-01-05", "2026-02-31"))),
               c(NA, "2026-01-05", NA))
  expect_equal(as_number(c(" 3 ", "1e3", "abc", "")), c(3, 1000, NA, NA))
  spec <- spec_complete(tibble::tibble(column = "visit", type = "date"))
  data <- data.frame(visit = c("bad", "2026-01-05", "2026-01-06"))
  expect_equal(check_type_conformance(data, spec, qualitas_options())$issues$row, 1)
})
