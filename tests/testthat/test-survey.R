# The example survey holds defects planted at known rows (helper-survey.R);
# each check must find its defects and no others.

survey_result <- function() {
  # CO02 checks missing required ages; this rule tests adults with supplied ages.
  assess(messy_survey(), spec = messy_survey_spec(),
         rules = c(consented = "consent == \"yes\"", adult = "is.na(age_years) | age_years == \"\" | as.numeric(age_years) >= 18"),
         options = qualitas_options(as_of = as.Date("2026-11-24")), name = "survey")
}

found_rows <- function(result, check, in_column = NULL) {
  kept <- issues(result) |>
    dplyr::filter(.data$check_id == check, is.null(in_column) | .data$column %in% in_column)
  sort(unique(kept$row))
}

test_that("every check runs without an error", {
  result <- survey_result()
  expect_false(any(measures(result)$status == "error"))
  expect_equal(nrow(measures(result)), nrow(checks_catalogue()))
})

test_that("repeated records and keys are found", {
  result <- survey_result()
  expect_equal(found_rows(result, "UN01"), 301)
  expect_equal(found_rows(result, "UN02"), c(10, 150, 301, 302))
})

test_that("values outside the specification are found", {
  result <- survey_result()
  expect_equal(found_rows(result, "VA01"), c(30, 40, 41))
  expect_equal(found_rows(result, "VA02"), c(50, 51, 60))
  expect_equal(found_rows(result, "VA04"), 20)
  expect_equal(found_rows(result, "ST05"), c(21, 70))
})

test_that("missing values, codes and rules are found", {
  result <- survey_result()
  expect_equal(found_rows(result, "CO03"), 50)
  expect_equal(found_rows(result, "CO02"), c(52, 111))
  expect_equal(found_rows(result, "CS01"), c(50, 110, 111))
})

test_that("spelling variants and stray spaces are found", {
  result <- survey_result()
  expect_equal(found_rows(result, "CS02", "district"), c(30, 31))
  expect_equal(found_rows(result, "CS03"), 31)
})

test_that("outliers and signs of made-up data are found", {
  result <- survey_result()
  expect_true(all(c(51, 60) %in% found_rows(result, "PL01")))
  expect_equal(found_rows(result, "FA04"), 80:87)
  expect_equal(found_rows(result, "FA05"), 90:99)
  found <- issues(result)
  # Heaped ages score 28.4; one enumerator's whole-number weights score
  # 16.9, below the problematic band of 20.
  expect_equal(found$column[found$check_id == "FA01"], "age_years")
  expect_equal(found$column[found$check_id == "FA02"], "age_years")
  expect_equal(found$value[found$check_id == "FA06"], "E6")
})

test_that("personal data are found", {
  result <- survey_result()
  found <- issues(result)
  expect_true(120 %in% found$row[found$check_id == "PI01" & found$column == "notes"])
  expect_equal(found$column[found$check_id == "PI02"], "respondent_phone")
})

test_that("measures and scores agree with the issues", {
  result <- survey_result()
  duplicates <- dplyr::filter(measures(result), .data$check_id == "UN02")
  expect_equal(duplicates$flagged, 4)
  expect_equal(duplicates$score, 1 - 4 / 302)
  expect_true(all(scores(result)$score >= 0 & scores(result)$score <= 1))
  fabrication <- dplyr::filter(measures(result), .data$dimension == "fabrication",
                               !is.na(.data$score))
  expect_equal(scores(result)$lowest[scores(result)$dimension == "fabrication"],
               min(fabrication$score))
  expect_equal(overall_score(result), mean(scores(result)$score))
})


test_that("explicit adult applicability retains completeness and unknown errors", {
  data <- data.frame(age_years = c(NA_character_, "", "17", "20"))
  spec <- infer_spec(data)
  spec$type <- "integer"; spec$required <- TRUE
  rule <- c(adult = 'is.na(age_years) | age_years == "" | as.numeric(age_years) >= 18')
  result <- assess(data, spec = spec, rules = rule, checks = c("CO02", "CS01"))
  expect_false(any(measures(result)$status == "error"))
  expect_identical(found_rows(result, "CO02"), c(1L, 2L))
  expect_identical(found_rows(result, "CS01"), 3L)
  expect_equal(measures(result)$checked[measures(result)$check_id == "CS01"], 4L)
  unknown <- assess(data, spec = spec, rules = c(adult = "as.numeric(age_years) >= 18"), checks = "CS01")
  expect_identical(measures(unknown)$status, "error")
  expect_true(is.na(measures(unknown)$score))
  expect_match(measures(unknown)$error, "unresolved NA")
  expect_identical(found_rows(unknown, "CS01"), 3L)
  data$age_years[4] <- "unknown"
  expect_warning(malformed <- assess(data, spec = spec, rules = rule, checks = "CS01"), "NAs introduced")
  expect_identical(measures(malformed)$status, "error")
  expect_true(is.na(measures(malformed)$score))
  expect_identical(found_rows(malformed, "CS01"), 3L)
})
