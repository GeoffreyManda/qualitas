# Each check counts the same unit when it counts what it examined and what it
# flagged, and a check that could not run in full has no score.

test_that("a rule that cannot be evaluated gives the rule check no score", {
  data <- data.frame(age = c(20, 15, 40))
  result <- assess(data, rules = c(adult = "age >= 18", broken = "no_such_column > 1"))
  rule <- dplyr::filter(measures(result), .data$check_id == "CS01")
  expect_equal(rule$status, "error")
  expect_true(is.na(rule$score))
  expect_equal(issues(result)$row[issues(result)$check_id == "CS01"], 2)
})

test_that("enumerator effects count enumerator and measure pairs", {
  withr::local_seed(1)
  data <- data.frame(enumerator = rep(c("a", "b", "c", "d"), each = 12),
                     weight = stats::rnorm(48, 60, 5), height = stats::rnorm(48, 160, 8))
  data$weight[data$enumerator == "d"] <- data$weight[data$enumerator == "d"] + 20
  spec <- infer_spec(data)
  spec$role[spec$column == "enumerator"] <- "enumerator"
  result <- assess(data, spec = spec)
  effect <- dplyr::filter(measures(result), .data$check_id == "FA06")
  expect_equal(effect$unit, "group")
  # Weight and height for four enumerators; the share of missing values is
  # zero throughout, so it gives no comparable pair.
  expect_equal(effect$checked, 8L)
  found <- dplyr::filter(issues(result), .data$check_id == "FA06")
  expect_equal(effect$flagged, dplyr::n_distinct(paste(found$column, found$value)))
  expect_true("d" %in% found$value[found$column == "weight"])
})

test_that("text in a numeric column is counted in cells", {
  data <- data.frame(score = c(as.character(1:9), "ten", "eleven"))
  spec <- infer_spec(data)
  spec$type <- "text"
  result <- assess(data, spec = spec, options = qualitas_options(mixed_type_share = 0.8))
  mixed <- dplyr::filter(measures(result), .data$check_id == "CS04")
  expect_equal(mixed$unit, "cell")
  expect_equal(mixed$checked, 11L)
  expect_equal(mixed$flagged, 2L)
})
