# Single checks on small data, including the cases where a check must stay
# silent.

run <- function(check, data, spec = infer_spec(data), ...) {
  check(data, spec, qualitas_options(...))
}

test_that("structure checks find empty, constant and badly named columns", {
  data <- data.frame(a = c(NA, NA, NA), b = c(1, 1, 1), c = c(1, NA, NA))
  expect_equal(run(check_empty_columns, data)$issues$column, "a")
  expect_equal(run(check_constant_columns, data)$issues$column, "b")
  # A label such as "1.1 Name of respondent" is a valid header; repeated,
  # padded and multi-line names are not.
  named <- data.frame(x = 1, x = 2, `1.1 Name` = 3, ` padded` = 4, `two\nlines` = 5,
                      check.names = FALSE)
  expect_setequal(run(check_column_names, named)$issues$column, c("x", " padded", "two\nlines"))
})

test_that("codes for missing values are not flagged when they are allowed", {
  spec <- spec_complete(tibble::tibble(column = "answer", type = "category",
                                       allowed = "yes;no;unknown"))
  data <- data.frame(answer = c("yes", "unknown", "-99"))
  expect_equal(run(check_disguised_missing, data, spec)$issues$row, 3)
})

test_that("multiple-choice answers are checked answer by answer", {
  spec <- spec_complete(tibble::tibble(column = "foods", type = "text",
                                       allowed = "rice;beans;milk"))
  data <- data.frame(foods = c("rice beans", "milk", "rice bread"))
  expect_equal(run(check_allowed_values, data, spec)$issues$row, 3)
})

test_that("near duplicates are found and exact duplicates left to UN01", {
  base <- data.frame(a = 1:6, b = letters[1:6], c = 6:1, d = LETTERS[1:6], e = c(1, 1, 2, 2, 3, 3))
  data <- rbind(base, base[2, ], transform(base[3, ], e = 9))
  found <- run(check_near_duplicates, data, near_duplicate_share = 0.8)$issues$row
  expect_true(all(c(3, 8) %in% found))
  expect_false(7 %in% found)
})

test_that("rules are evaluated and a broken rule is reported, not raised", {
  data <- data.frame(start = as.Date(c("2026-01-01", "2026-03-01")),
                     end = as.Date(c("2026-02-01", "2026-02-01")))
  rules <- read_rules(c(ordered = "end >= start", broken = "no_such_column > 1"))
  found <- check_rules(data, infer_spec(data), qualitas_options(rules = rules))$issues
  expect_equal(found$row[!is.na(found$row)], 2)
  expect_match(found$message[is.na(found$row)], "could not be evaluated")
})

test_that("outliers are found within groups", {
  data <- data.frame(district = rep(c("a", "b"), each = 12),
                     weight = c(seq(10, 21), seq(50, 61)))
  data$weight[3] <- 55
  spec <- infer_spec(data)
  expect_equal(run(check_group_outliers, data, spec)$issues$row, 3)
  expect_length(run(check_outliers, data, spec)$issues$row, 0)
})

test_that("foreign keys that match no record are found", {
  visits <- data.frame(child = c("a", "b", "z", NA))
  children <- data.frame(id = c("a", "b"))
  links <- data.frame(column = "child", table = "children", key = "id")
  result <- check_foreign_keys(visits, infer_spec(visits),
                               qualitas_options(tables = list(children = children),
                                                relations = links))
  expect_equal(result$issues$row, 3)
})

test_that("stale data and empty periods are found", {
  data <- data.frame(visit_date = as.Date(c("2026-01-10", "2026-02-10", "2026-04-10")))
  spec <- infer_spec(data)
  stale <- run(check_currentness, data, spec, as_of = as.Date("2026-11-01"), max_age_days = 90)
  expect_equal(stale$issues$column, "visit_date")
  gaps <- run(check_period_gaps, data, spec, period = "month")
  expect_equal(gaps$issues$value, "2026-03-01")
})

test_that("uniform last digits and unheaped ages are not flagged", {
  withr::local_seed(1)
  data <- data.frame(age = sample(18:80, 400, replace = TRUE),
                     weight = round(stats::runif(400, 40, 90), 1))
  spec <- infer_spec(data)
  expect_length(run(check_age_heaping, data, spec)$issues$column, 0)
  expect_length(run(check_digit_preference, data, spec)$issues$column, 0)
})

test_that("an inferred specification is not judged for its documentation", {
  data <- data.frame(x = 1:3)
  expect_equal(run(check_documentation, data)$checked, 0)
  spec <- spec_complete(tibble::tibble(column = "x", type = "integer"))
  expect_equal(run(check_documentation, data, spec)$issues$column, "x")
})
