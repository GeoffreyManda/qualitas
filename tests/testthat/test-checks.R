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

test_that("a rule that cannot be evaluated makes the rule check an error", {
  data <- data.frame(start = as.Date(c("2026-01-01", "2026-03-01")),
                     end = as.Date(c("2026-02-01", "2026-02-01")))
  rules <- read_rules(c(ordered = "end >= start", broken = "no_such_column > 1"))
  result <- check_rules(data, infer_spec(data), qualitas_options(rules = rules))
  expect_equal(result$issues$row, 2)
  expect_match(result$error, "rule broken could not be evaluated")
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

test_that("dictionary and indicator metadata retain missing rows and structural defects", {
  dict <- data.frame(element_id = "site", table = "visits", column = "site_id",
    definition = "Site identifier", type = "text", unit = "not applicable", owner = "IA",
    version = "1.0")
  expect_equal(nrow(validate_dictionary(dict)), 0L)
  expect_true("definition" %in% validate_dictionary(transform(dict, definition = " "))$requirement)
  expect_true("dictionary_mapping" %in% validate_dictionary(rbind(dict, dict))$requirement)
  reused <- rbind(dict, transform(dict, table = "sites"))
  expect_equal(nrow(validate_dictionary(reused)), 0L)
  reused$definition[2] <- "A different concept"
  expect_true("element_id" %in% validate_dictionary(reused)$requirement)
  expect_true("type" %in% validate_dictionary(transform(dict, type = "mystery"))$requirement)
  expect_true("unit" %in% validate_dictionary(dict[setdiff(names(dict), "unit")])$requirement)
  indicator <- data.frame(indicator_id = "coverage", definition = "Visit coverage",
    population = "scheduled visits", unit = "percent", numerator = "completed visits",
    denominator = "scheduled visits", period = "quarter", disaggregation = "agency",
    source = "approved plan", owner = "IA", version = "1.0")
  expect_equal(nrow(validate_indicators(indicator)), 0L)
  expect_equal(validate_indicators(transform(indicator, population = ""))$requirement, "population")
  expect_true("indicator_id" %in% validate_indicators(rbind(indicator, indicator))$requirement)
})

test_that("composite keys preserve tuples, missingness, types and parent uniqueness", {
  parent <- data.frame(ia = c("a", "b"), site = c("1", "1"))
  child <- data.frame(ia = c("a", "b", "c", NA), site = c("1", "1", "1", "1"))
  links <- data.frame(from = "visits", table = "sites")
  links$columns <- list(c("ia", "site"))
  links$keys <- list(c("ia", "site"))
  tables <- list(sites = parent, visits = child)
  result <- assess_keys(tables, list(sites = c("ia", "site")), links)
  expect_setequal(result$requirement, c("foreign_missing", "foreign_unmatched"))
  expect_equal(result$row[result$requirement == "foreign_unmatched"], 3L)
  tables$sites <- rbind(parent, parent[1, ])
  expect_true("parent_duplicate" %in% assess_keys(tables, list(), links)$requirement)
  tables$sites <- transform(parent, site = as.integer(site))
  expect_true("foreign_type" %in% assess_keys(tables, list(), links)$requirement)
  links$required <- FALSE
  expect_false("foreign_missing" %in% assess_keys(tables, list(), links)$requirement)
  expect_error(assess_keys(tables, list(sites = "absent")), "existing")
  empty <- list(sites = parent[FALSE, ])
  expect_equal(nrow(assess_keys(empty, list(sites = c("ia", "site")))), 0L)
  collision <- data.frame(a = c("a\u001fb", "a"), b = c("c", "b\u001fc"))
  expect_equal(nrow(assess_keys(list(x = collision), list(x = c("a", "b")))), 0L)
})

test_that("coordinate screening checks missing, finite and declared limits", {
  data <- data.frame(lat = c(0, 15, NA, 91, 15), lon = c(0, 44, 44, 44, Inf))
  options <- qualitas_options(latitude = "lat", longitude = "lon")
  result <- assess(data, options = options, checks = "PL04")
  expect_equal(measures(result)$checked, 5L)
  expect_equal(measures(result)$flagged, 3L)
  expect_equal(issues(result)$row, c(3L, 4L, 5L))
  expect_equal(measures(result)$score, 2 / 5)
  options$coordinate_bounds <- c(12, 19, 42, 55)
  expect_equal(check_coordinates(data, infer_spec(data), options)$issues$row, c(1L, 3L, 4L, 5L))
  options$longitude <- "absent"
  expect_match(check_coordinates(data, infer_spec(data), options)$error, "existing")
  expect_equal(check_coordinates(data, infer_spec(data), qualitas_options())$checked, 0L)
})

test_that("expected answers preserve unknown applicability and skip violations", {
  data <- data.frame(answer = c("yes", NA, "99", "yes", NA))
  result <- assess_answers(data, "answer", c(TRUE, TRUE, TRUE, FALSE, NA), "99")
  expect_equal(result$row[result$requirement == "expected_missing"], c(2L, 3L))
  expect_equal(result$row[result$requirement == "skipped_answer"], 4L)
  expect_equal(result$row[result$requirement == "applicability_unknown"], 5L)
  expect_error(assess_answers(data, "answer", TRUE), "each row")
})

test_that("choice constraints preserve labels, exclusivity and empty tokens", {
  data <- data.frame(answer = c("a;b", "a;a", "none;a", "z", "a;", "", NA))
  result <- assess_choices(data, "answer", c("a", "b", "none"), multiple = TRUE,
                            exclusive = "none", maximum = 2)
  expect_equal(result$row[result$requirement == "choice_duplicate"], 2L)
  expect_equal(result$row[result$requirement == "choice_exclusive"], 3L)
  expect_equal(result$row[result$requirement == "choice_unknown"], c(4L, 5L))
  expect_false(any(result$row %in% c(1L, 6L, 7L)))
  expect_equal(assess_choices(data[1, , drop = FALSE], "answer", c("a", "b"))$requirement,
                "choice_unknown")
  expect_equal(nrow(assess_choices(data[FALSE, , drop = FALSE], "answer", c("a", "b"))), 0L)
})

test_that("primary-key findings stack across tables and internal names cannot match orphans", {
  tables <- list(a = data.frame(id = c(1, 1)), b = data.frame(id = c(1, 1, 1)))
  result <- assess_keys(tables, list(a = "id", b = "id"))
  expect_equal(sum(result$requirement == "primary_duplicate"), 5L)
  expect_setequal(result$table, c("a", "b"))
  missing <- assess_keys(list(a = data.frame(id = c(NA, NA))), list(a = "id"))
  expect_equal(sum(missing$requirement == "primary_missing"), 2L)
  relation <- data.frame(from = "child", table = "parent")
  relation$columns <- relation$keys <- list(".qualitas_row")
  result <- assess_keys(list(parent = data.frame(.qualitas_row = 1L),
                            child = data.frame(.qualitas_row = 999L)), list(), relation)
  expect_equal(result$requirement, "foreign_unmatched")
  expect_equal(result$row, 1L)
})

test_that("metadata and coordinate selectors reject misleading types", {
  d <- data.frame(element_id = "i", table = "t", column = "c", type = "text",
                   unit = "not applicable", owner = "IA", version = "1")
  d$definition <- I(list(NULL))
  expect_equal(validate_dictionary(d)$requirement, "definition")
  d$definition <- "A definition"
  d$owner <- FALSE
  expect_equal(validate_dictionary(d)$requirement, "owner")
  data <- data.frame(other = 15, `1` = 100, `2` = 44, check.names = FALSE)
  result <- check_coordinates(data, NULL, qualitas_options(latitude = 1, longitude = 2))
  expect_false(is.na(result$error))
  expect_true("choice_cardinality" %in% assess_choices(data.frame(x = "yes"), "x", "yes",
    minimum = 0, maximum = 0)$requirement)
  expect_error(assess_choices(data.frame(x = "a;b"), "x", "a;b", multiple = TRUE), "separator")
})

test_that("questionnaire contracts check routes translations and closed bands", {
  q <- data.frame(question_id = c("q1", "q1", "q2", "q2"),
    language = c("en", "ar", "en", "ar"), order = c(1, 1, 2, 2),
    type = c("single", "single", "number", "number"),
    text = "Synthetic question", source = "form v1", route_to = c("q2", "q2", "", ""),
    unit = c("not applicable", "not applicable", "metre", "metre"))
  ch <- data.frame(question_id = "q1", language = c("en", "ar"), code = "1", label = "answer")
  expect_equal(nrow(assess_questionnaire(q, ch)), 0L)
  bad <- q;bad$route_to[1] <- "q99"
  expect_true("route_target" %in% assess_questionnaire(bad, ch)$requirement)
  expect_true("question_translation" %in% assess_questionnaire(q[-1, ], ch)$requirement)
  ch$code[2] <- "2"
  expect_true("choice_translation" %in% assess_questionnaire(q, ch)$requirement)
  expect_true("question_unit" %in% assess_questionnaire(transform(q, unit = ""), ch)$requirement)
  bands <- data.frame(question_id = "q1", language = "en", code = c("a", "b"),
    label = "band", lower = c(0, 10), upper = c(10, 20))
  expect_true("answer_band" %in% assess_questionnaire(q, bands)$requirement)
})

test_that("precision and missing patterns use declared measurement and applicability", {
  d <- data.frame(x = c(1.1, 1.25, NA, Inf))
  expect_equal(assess_precision(d, "x", 0.1)$row, c(2L, 4L))
  expect_error(assess_precision(d, "x", 0), "positive")
  d <- data.frame(a = c(NA, NA, 1), b = c(NA, NA, 1))
  expect_equal(assess_missing_patterns(d, matrix(TRUE, 3, 2))$row, c(1L, 2L))
  expect_equal(nrow(assess_missing_patterns(d, matrix(FALSE, 3, 2))), 0L)
  expect_error(assess_missing_patterns(d, matrix(NA, 3, 2)), "complete")
})

test_that("choice definitions require an existing question-language pair", {
  q <- data.frame(question_id = c("q1", "q2"), language = c("en", "fr"),
    order = 1, type = "text", text = "Synthetic question", source = "form v1")
  choices <- data.frame(question_id = c("q1", "absent", "q1", "q2"),
    language = c("en", "en", "fr", "fr"), code = "a", label = "Synthetic choice")
  out <- assess_questionnaire(q, choices, languages = character())
  expect_identical(out$row[out$requirement == "choice_question"], c(2L, 3L))
  expect_false(any(out$row[out$requirement == "choice_question"] %in% c(1L, 4L)))
  expect_equal(nrow(assess_questionnaire(q[0, ], choices[0, ])), 0L)
  out <- assess_questionnaire(q[0, ], choices)
  expect_identical(out$row[out$requirement == "choice_question"], 1:4)
})

test_that("a single answer-band endpoint is a defect even when unknown or empty", {
  q <- data.frame(question_id = "q1", language = "en", order = 1,
    type = "single", text = "Choose", source = "form v1")
  choices <- data.frame(question_id = "q1", language = "en", code = c("a", "b"), label = "Band")
  for (endpoint in c("lower", "upper")) {
    bands <- choices; bands[[endpoint]] <- c(0, NA_real_)
    out <- assess_questionnaire(q, bands)
    expect_identical(out$row[out$requirement == "answer_band"], 1:2)
    out <- assess_questionnaire(q, bands[0, ])
    expect_true("answer_band" %in% out$requirement)
    expect_true(all(is.na(out$row[out$requirement == "answer_band"])))
  }
  expect_equal(nrow(assess_questionnaire(q, choices)), 0L)
  bands <- transform(choices, lower = c(0, NA_real_), upper = c(1, 2))
  expect_identical(assess_questionnaire(q, bands)$row, 2L)
})

test_that("precision separates unverifiable magnitudes without losing original rows", {
  d <- data.frame(x = c(NA, 0, 1.5, 1e15 + 0.5, Inf, -Inf, NaN, 2))
  out <- assess_precision(d, "x", 1)
  expect_identical(out$row, c(3L, 4L, 5L, 6L))
  expect_identical(out$requirement, c("measurement_resolution", "resolution_unverifiable",
    "measurement_resolution", "measurement_resolution"))
  expect_equal(nrow(assess_precision(data.frame(x = c(0, 0.1, 0.2, 0.3, -0.1, NA)), "x", 0.1)), 0L)
  expect_equal(nrow(assess_precision(data.frame(x = numeric()), "x", 0.1)), 0L)
  expect_equal(assess_precision(data.frame(x = c(NA, 0, 1e-200, -1e-200)), "x", 1e200)$row, c(3L, 4L))
  out <- assess_precision(data.frame(x = c(0, 1e200)), "x", 1e-200)
  expect_identical(out$row, 2L)
  expect_identical(out$requirement, "resolution_unverifiable")
  expect_equal(nrow(assess_precision(data.frame(x = c(0, 1e-200, 2e-200)), "x", 1e-200)), 0L)
  expect_error(assess_precision(data.frame(x = 1), "x", Inf), "finite")
  expect_error(assess_precision(data.frame(x = 1), "x", NA_real_), "finite")
  expect_error(assess_precision(data.frame(x = 1 + 1i), "x", 1), "numeric")
})
