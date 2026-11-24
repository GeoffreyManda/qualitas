# Assessments of real data sets the package was not built on: the Mayo
# Clinic primary biliary cholangitis trial (survival::pbc) and the NOAA
# Atlantic storm tracks (dplyr::storms). Each test pins a behaviour that the
# first blind runs showed to be wrong.

test_that("names ending in time are not read as dates", {
  skip_if_not_installed("survival")
  spec <- infer_spec(survival::pbc)
  expect_equal(spec$role[spec$column %in% c("time", "protime")], c("", ""))
})

test_that("missing values in the trial match a direct count", {
  skip_if_not_installed("survival")
  result <- suppressMessages(assess(survival::pbc, checks = "CO01", name = "pbc"))
  expect_equal(nrow(issues(result)), sum(is.na(survival::pbc)))
})

test_that("last digits are read at the precision most values carry", {
  skip_if_not_installed("survival")
  data <- survival::pbc
  found <- check_digit_preference(data, infer_spec(data), qualitas_options())$issues
  # alk.phos is whole numbers with a few decimals; age is computed to four
  # decimals. AST ends in .x0 or .x5 in 287 of 312 values (score 61.5);
  # cholesterol prefers even digits (score 13.2), within the band SMART
  # calls acceptable.
  expect_false(any(c("alk.phos", "age", "chol") %in% found$column))
  expect_true("ast" %in% found$column)
})

test_that("skewed laboratory values are judged on the log scale", {
  skip_if_not_installed("survival")
  data <- survival::pbc
  found <- check_outliers(data, infer_spec(data), qualitas_options())$issues
  expect_false(any(c("bili", "time") %in% found$column))
  chol <- log(data$chol[!is.na(data$chol)])
  expect_equal(sort(found$row[found$column == "chol"]),
               which(!is.na(data$chol))[abs(modified_z(chol)) > 3.5])
})

test_that("a plausible number is not taken for a missing-value code", {
  data <- data.frame(longitude = c(-99, -98.5, -97, -96.2, -95, -94, -93.1, -92, -91, -90),
                     age = c(-99, 30, 31, 35, 40, 41, 45, 50, 52, 60))
  found <- check_disguised_missing(data, infer_spec(data), qualitas_options())$issues
  expect_equal(found$column, "age")
})

test_that("a long tail on one side does not hide or invent outliers", {
  data <- dplyr::storms
  found <- check_outliers(data, infer_spec(data), qualitas_options())$issues
  pressure <- as.numeric(found$value[found$column == "pressure"])
  expect_true(all(pressure < stats::median(data$pressure)))
  expect_lt(length(pressure), 0.02 * nrow(data))
})

test_that("the benchmark's false alarms stay fixed", {
  # A repeated identifier groups records in panel data; it is not a key.
  panel <- data.frame(school_id = rep(1:50, each = 4), year = rep(2001:2004, 50),
                      score = seq_len(200))
  expect_false(infer_spec(panel)$unique[1])
  # Values recorded in fives follow a convention.
  winds <- data.frame(wind = rep(seq(10, 160, by = 5), length.out = 400))
  expect_equal(check_digit_preference(winds, infer_spec(winds), qualitas_options())$checked, 0)
  # Benford's law is tested only on declared amounts.
  spend <- data.frame(paid = round(exp(seq(1, 12, length.out = 1500)), 2))
  expect_equal(check_benford(spend, infer_spec(spend), qualitas_options())$checked, 0)
  declared <- spec_complete(tibble::tibble(column = "paid", type = "number", role = "amount"))
  expect_equal(check_benford(spend, declared, qualitas_options())$checked, 1)
  # An order identifier is not a telephone number.
  orders <- data.frame(id = sprintf("2015-%06d", 1:30), item = "pizza")
  expect_equal(nrow(check_pii_values(orders, infer_spec(orders), qualitas_options())$issues), 0)
  # A policy's duration in years is not an interview's length.
  expect_equal(infer_role(c("duration", "interview_duration", "duration_min")),
               c("", "duration", "duration"))
})

test_that("text that is not valid UTF-8 is reported, not fatal", {
  # bayesrules::book_banning holds titles in Latin-1, which stopped the first
  # benchmark run.
  latin <- c("caf\xe9 society", "plain title", "na\xefve reader", "plain title")
  data <- data.frame(title = latin, pages = c(120, 200, 340, 200))
  result <- suppressMessages(assess(data, name = "titles"))
  expect_false(any(measures(result)$status == "error"))
  found <- issues(result)
  expect_equal(found$row[found$check_id == "ST06"], c(1, 3))
  expect_equal(as_text(latin)[1], "caf\u00e9 society")
})
