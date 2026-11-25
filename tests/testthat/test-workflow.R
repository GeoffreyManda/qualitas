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

test_that("manifest checks detect missing, changed and unsafe files", {
  root <- withr::local_tempdir()
  writeLines("synthetic", file.path(root, "data.csv"))
  writeLines("definition", file.path(root, "dictionary.csv"))
  files <- c("data.csv", "dictionary.csv")
  hashes <- vapply(file.path(root, files), digest::digest, character(1),
                   file = TRUE, algo = "sha256", USE.NAMES = FALSE)
  manifest <- data.frame(path = files, sha256 = hashes, sender = "synthetic IA",
    sent_at = "2026-11-25T10:00:00Z", version = "1.0", role = c("data", "dictionary"))
  expect_equal(nrow(assess_delivery(manifest, root)), 0L)
  writeLines("changed", file.path(root, files[1]))
  expect_equal(assess_delivery(manifest, root)$requirement, "delivery_hash")
  manifest$path[1] <- "../outside.csv"
  expect_true("delivery_path" %in% assess_delivery(manifest, root)$requirement)
  manifest$path[1] <- "missing.csv"
  expect_true("delivery_missing" %in% assess_delivery(manifest, root)$requirement)
  expect_true("delivery_role" %in% assess_delivery(manifest[1, ], root)$requirement)
})

test_that("reconciliation distinguishes zero totals, invalid counts and thresholds", {
  counts <- data.frame(indicator_id = letters[1:6], period = "2026-Q3",
    reported = c(10, 0, 0, 10, NA, 2.5), observed = c(9, 0, 1, 11, 2, 3))
  result <- reconcile_counts(counts)
  expect_identical(result$status,
    c("issues found", "passed", "issues found", "issues found", "error", "error"))
  expect_true(is.na(result$ratio[2]))
  expect_equal(reconcile_counts(counts, 0.1)$status[c(1, 4)], c("passed", "passed"))
  expect_error(reconcile_counts(counts, -1), "nonnegative")
  expect_error(reconcile_counts(transform(counts, reported = as.character(reported))), "numeric")
})

test_that("review decisions bind to source and preserve all machine flags", {
  data <- data.frame(id = c(1, 1))
  x <- assess(data, checks = "UN01")
  hash <- paste(rep("a", 64), collapse = "")
  initial <- review_findings(x, hash)
  expect_equal(initial$decision, "pending")
  decisions <- data.frame(finding_id = initial$finding_id, decision = "explained",
    reviewer = "Reviewer", reviewed_at = "2026-11-25", reason = "Documented test record",
    evidence = "synthetic-protocol section 2")
  reviewed <- review_findings(x, hash, decisions)
  expect_equal(nrow(reviewed), nrow(issues(x)))
  expect_equal(reviewed$decision, "explained")
  expect_equal(measures(x)$flagged, 1L)
  expect_error(review_findings(x, hash, transform(decisions, evidence = "")), "evidence")
  expect_error(review_findings(x, paste(rep("b", 64), collapse = ""), decisions), "snapshot")
  expect_equal(nrow(review_findings(assess(data[FALSE, , drop = FALSE], checks = "UN01"), hash)), 0L)
})

test_that("delivery aliases timestamps and sizes remain independent failures", {
  root <- withr::local_tempdir()
  writeLines("sample", file.path(root, "data.csv"))
  m <- data.frame(path = c("data.csv", "./data.csv"),
    sha256 = digest::digest(file = file.path(root, "data.csv"), algo = "sha256"),
    sender = "IA", sent_at = "not-a-time", version = "1", role = c("data", "dictionary"))
  result <- assess_delivery(m, root)
  expect_equal(sum(result$requirement == "delivery_alias"), 2L)
  expect_equal(sum(result$requirement == "delivery_timestamp"), 2L)
  m <- m[1, , drop = FALSE]
  m$sent_at <- "2026-02-30T10:00:00Z"
  expect_true("delivery_timestamp" %in% assess_delivery(m, root)$requirement)
  m$sent_at <- "2026-11-25T10:00:00+03:00"
  m$byte_size <- 0
  expect_true("delivery_size" %in% assess_delivery(m, root)$requirement)
})

test_that("decisions cannot transfer across datasets contracts or invalid dates", {
  x <- assess(data.frame(id = c(1, 1)), checks = "UN01", name = "sheet-A")
  hash <- strrep("a", 64)
  id <- review_findings(x, hash)$finding_id
  decision <- data.frame(finding_id = id, decision = "confirmed", reviewer = "R",
    reviewed_at = "2026-11-25", reason = "Independent check", evidence = "protocol section 2")
  expect_error(review_findings(x, hash, decision, dataset_locator = "sheet-B"), "snapshot")
  x$options$as_of <- as.Date("2027-01-01")
  expect_error(review_findings(x, hash, decision), "snapshot")
  decision$finding_id <- review_findings(x, hash)$finding_id
  decision$reviewed_at <- "not-a-date"
  expect_error(review_findings(x, hash, decision), "dates")
})

test_that("requirements require exact scope hash and valid reviewed evidence", {
  req <- data.frame(requirement_id = "restore", scope_id = "system-A:Q3", source = "agreement 2",
    owner = "IA")
  expect_equal(assess_requirements(req)$status, "missing evidence")
  path <- withr::local_tempfile();writeLines("Synthetic restoration test passed", path)
  ev <- data.frame(requirement_id = "restore", scope_id = "system-A:Q3", path = path,
    sha256 = digest::digest(file = path, algo = "sha256"), outcome = "passed", reviewer = "R",
    reviewed_at = "2026-11-25", reason = "Restored values compared", valid_until = "2026-12-31")
  expect_equal(assess_requirements(req, ev, as.Date("2026-11-25"))$status,
    "met from reviewed evidence")
  expect_equal(assess_requirements(req, ev, as.Date("2027-01-01"))$status, "expired evidence")
  expect_equal(assess_requirements(req, transform(ev, scope_id = "system-B:Q3"))$status,
    "missing evidence")
  writeLines("changed", path)
  expect_equal(assess_requirements(req, ev)$status, "unverified evidence")
  expect_error(assess_requirements(req, transform(ev, reviewed_at = "wrong")), "valid")
})

test_that("delivery naming and identical-content warnings use the declared profile", {
  root <- withr::local_tempdir()
  for (name in c("wrong.csv", "copy.csv")) writeLines("same", file.path(root, name))
  m <- data.frame(path = c("wrong.csv", "copy.csv"), sender = "IA", sent_at = "2026-11-25T10:00:00Z",
    version = "1", role = c("data", "dictionary"),
    sha256 = digest::digest(file = file.path(root, "wrong.csv"), algo = "sha256"))
  out <- assess_delivery(m, root, filename_pattern = "^IA_[0-9]{4}_")
  expect_equal(sum(out$requirement == "delivery_name"), 2L)
  expect_equal(sum(out$requirement == "delivery_duplicate_content"), 2L)
})

test_that("evidence review cutoff includes the whole assessment UTC day", {
  path <- withr::local_tempfile()
  writeLines("Original synthetic evidence", path)
  req <- data.frame(requirement_id = c("before", "on", "after", "offset-before", "offset-after"),
    scope_id = "scope", source = "agreement", owner = "owner")
  ev <- data.frame(requirement_id = req$requirement_id, scope_id = "scope", path = path,
    sha256 = digest::digest(file = path, algo = "sha256"), outcome = "passed", reviewer = "reviewer",
    reviewed_at = c("2026-11-24", "2026-11-25T23:59:59Z", "2026-11-26T00:00:00Z",
      "2026-11-26T01:00:00+02:00", "2026-11-25T23:00:00-02:00"), reason = "reviewed")
  out <- assess_requirements(req, ev, as.Date("2026-11-25"))
  expect_identical(out$requirement_id, req$requirement_id)
  expect_identical(out$status, c("met from reviewed evidence", "met from reviewed evidence",
    "not yet reviewed", "met from reviewed evidence", "not yet reviewed"))
  expect_identical(assess_requirements(req, ev, "2026-11-25")$status, out$status)
  expect_identical(assess_requirements(req[1, ], transform(ev[1, ], reviewed_at = "2099-01-01"),
    as.Date("2026-11-25"))$status, "not yet reviewed")
  expect_identical(assess_requirements(req[1, ], transform(ev[1, ], reviewed_at = "2026-11-25"),
    as.Date("2026-11-25"))$status, "met from reviewed evidence")
  expect_identical(assess_requirements(req[1, ], ev[0, ], as.Date("2026-11-25"))$status, "missing evidence")
  expect_equal(nrow(assess_requirements(req[0, ], ev[0, ], as.Date("2026-11-25"))), 0L)
  for (cutoff in list(as.POSIXct("2026-11-25 12:00:00", tz = "UTC"), "2026-11-25T12:00:00Z",
                      as.Date(NA), as.Date(Inf, origin = "1970-01-01"), "2026-02-30"))
    expect_error(assess_requirements(req, ev, cutoff), "date|Date")
  expect_identical(assess_requirements(req[1, ], transform(ev[1, ], valid_until = "2026-11-24"),
    as.Date("2026-11-25"))$status, "expired evidence")
  expect_identical(assess_requirements(req[1, ], transform(ev[1, ], outcome = "failed"),
    as.Date("2026-11-25"))$status, "failed")
})
