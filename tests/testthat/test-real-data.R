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
  # decimals; cholesterol ends in an even digit 193 times in 284.
  expect_false(any(c("alk.phos", "age") %in% found$column))
  expect_true("chol" %in% found$column)
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
