# The steps around an assessment: specifications, the cleaning log, drift
# between deliveries, linked tables, reports and exported results.

test_that("a specification survives writing and reading", {
  spec <- messy_survey_spec()
  path <- withr::local_tempfile(fileext = ".csv")
  write_spec(spec, path)
  expect_equal(read_spec(path), spec, ignore_attr = TRUE)
})

test_that("the catalogue names an exported function for every check", {
  catalogue <- checks_catalogue()
  expect_false(anyDuplicated(catalogue$check_id) > 0)
  expect_true(all(catalogue$`function` %in% getNamespaceExports("qualitas")))
  expect_true(all(catalogue$severity %in% names(severity_weights())))
})

test_that("a cleaning log changes and removes only what it says", {
  data <- data.frame(id = 1:3, age = c(30, 300, 31))
  spec <- spec_complete(tibble::tibble(column = c("id", "age"), type = "integer",
                                       max = c(NA, "120")))
  log <- cleaning_log(assess(data, spec = spec, checks = "VA02", name = "toy"))
  expect_equal(log$row, 2)
  log$action <- "change"
  log$new_value <- "30"
  expect_message(apply_log(data, log), "Changed 1 value")
  cleaned <- suppressMessages(apply_log(data, log))
  expect_equal(cleaned$age, c(30, 30, 31))
  log$action <- "remove"
  expect_equal(nrow(suppressMessages(apply_log(data, log))), 2)
  log$action <- "fix"
  expect_error(apply_log(data, log), "not keep, change or remove")
})

test_that("drift between deliveries is flagged", {
  old <- data.frame(age = 20:40, sex = rep(c("f", "m", "f"), 7), gone = 1)
  new <- data.frame(age = 40:60, sex = rep("f", 21), added = 1)
  drift <- compare_data(old, new)
  expect_equal(drift$status[match(c("gone", "added"), drift$column)], c("removed", "added"))
  expect_true(all(drift$flagged[match(c("age", "sex"), drift$column)]))
  steady <- compare_data(old, old)
  expect_false(any(steady$flagged))
})

test_that("linked tables are assessed with their keys", {
  households <- data.frame(hh_id = c("h1", "h2"))
  members <- data.frame(member_id = 1:3, hh_id = c("h1", "h2", "h9"))
  links <- data.frame(from = "members", column = "hh_id", table = "households", key = "hh_id")
  expect_no_warning(
    results <- assess_tables(list(households = households, members = members),
                             relations = links, checks = "IN01"))
  expect_s3_class(results, "qualitas_assessments")
  expect_equal(issues(results$members)$row, 3)
  expect_equal(nrow(issues(results$households)), 0)
  expect_equal(nrow(scores(results$households)), 0)
})

test_that("reports and results are written", {
  result <- assess(messy_survey(), spec = messy_survey_spec(), name = "survey")
  html <- withr::local_tempfile(fileext = ".html")
  suppressMessages(report(result, html))
  page <- paste(readLines(html), collapse = "\n")
  expect_match(page, "Data quality of survey")
  expect_match(page, "Scores by dimension")
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  docx <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(report(result, docx))
  expect_gt(file.size(docx), 5000)
  dir <- withr::local_tempdir()
  paths <- write_results(result, dir)
  expect_true(all(file.exists(paths)))
  json <- jsonlite::read_json(paths[4])
  expect_equal(json$records, 302)
})

test_that("printing shows the overall score", {
  result <- assess(data.frame(id = c(1, 1, 2), age = c(30, NA, 300)), name = "toy")
  said <- utils::capture.output(utils::capture.output(print(result)), type = "message")
  expect_true(any(grepl("Overall score", said)))
  expect_named(summary(result), c("overall", "scores", "top_checks", "by_severity"))
})
