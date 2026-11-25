# A made-up household survey of 300 interviews with known defects, used by
# the tests and written to inst/extdata/messy_survey.csv by
# write_messy_survey(). Every defect is planted at a known row, so a test can
# say exactly what each check must find.

messy_survey <- function() {
  withr::local_seed(20261124)
  n <- 300
  enumerator <- rep(paste0("E", 1:6), length.out = n)
  age <- sample(18:70, n, replace = TRUE)
  heaped <- enumerator == "E4" | stats::runif(n) < 0.3
  weight <- round(stats::rnorm(n, 65, 9), 1)
  survey <- tibble::tibble(
    interview_id = sprintf("S%03d", seq_len(n)),
    interview_date = format(as.Date("2026-01-05") + sample(0:82, n, replace = TRUE)),
    enumerator = enumerator,
    district = sample(c("North", "South", "East", "West"), n, replace = TRUE),
    sex = sample(c("female", "male"), n, replace = TRUE),
    age_years = as.character(dplyr::if_else(heaped, round(age / 5) * 5, age)),
    weight_kg = dplyr::if_else(enumerator == "E3", round(weight), weight) +
      dplyr::if_else(enumerator == "E6", 12, 0),
    household_size = as.character(sample(1:12, n, replace = TRUE)),
    income = round(exp(stats::rnorm(n, 9, 1.4))),
    duration_min = round(stats::rnorm(n, 45, 7)),
    q1 = sample(1:5, n, TRUE), q2 = sample(1:5, n, TRUE), q3 = sample(1:5, n, TRUE),
    q4 = sample(1:5, n, TRUE), q5 = sample(1:5, n, TRUE), q6 = sample(1:5, n, TRUE),
    consent = "yes",
    respondent_phone = sprintf("+967 7%08d", 1e7 + sample.int(8e7, n)),
    notes = ""
  )
  plant_defects(survey)
}

# The defects, each at a fixed row.
plant_defects <- function(survey) {
  survey |>
    dplyr::mutate(
      interview_date = replace(interview_date, c(20, 21), c("2027-01-01", "31/02/2026")),
      district = replace(district, c(30, 31), c("north", "South ")),
      sex = replace(sex, c(40, 41), c("Male", "F")),
      age_years = replace(age_years, c(50, 51, 52), c("-99", "230", "")),
      weight_kg = replace(weight_kg, 60, 650),
      household_size = replace(household_size, 70, "twelve"),
      duration_min = replace(duration_min, 80:87, c(4, 5, 6, 5, 4, 7, 6, 5)),
      dplyr::across(q1:q6, \(q) replace(q, 90:99, 3)),
      consent = replace(consent, c(110, 111), c("no", "")),
      notes = replace(notes, 120, "call back at someone@example.org")
    ) |>
    dplyr::bind_rows(survey[10, ]) |>
    dplyr::bind_rows(dplyr::mutate(survey[150, ], weight_kg = 70.2))
}

# The specification of the survey, as a user would write it.
messy_survey_spec <- function() {
  spec_complete(tibble::tribble(
    ~column, ~type, ~role, ~required, ~unique, ~min, ~max, ~allowed, ~description,
    "interview_id", "text", "id", TRUE, TRUE, NA, NA, NA, "Interview identifier",
    "interview_date", "date", "date", TRUE, FALSE, "2026-01-01", NA, NA, "Date of interview",
    "enumerator", "category", "enumerator", TRUE, FALSE, NA, NA, NA, "Enumerator code",
    "district", "category", "group", TRUE, FALSE, NA, NA, "North;South;East;West", "District",
    "sex", "category", "", TRUE, FALSE, NA, NA, "female;male", "Sex of respondent",
    "age_years", "integer", "age", TRUE, FALSE, "18", "100", NA, "Age in completed years",
    "weight_kg", "number", "", FALSE, FALSE, "30", "200", NA, "Weight in kilograms",
    "household_size", "integer", "", TRUE, FALSE, "1", "30", NA, "Members of the household",
    "income", "number", "", FALSE, FALSE, "0", NA, NA, "Monthly income in rials",
    "duration_min", "number", "duration", TRUE, FALSE, "0", NA, NA, "Interview length, minutes",
    "q1", "integer", "", TRUE, FALSE, "1", "5", NA, "Satisfaction item 1",
    "q2", "integer", "", TRUE, FALSE, "1", "5", NA, "Satisfaction item 2",
    "q3", "integer", "", TRUE, FALSE, "1", "5", NA, "Satisfaction item 3",
    "q4", "integer", "", TRUE, FALSE, "1", "5", NA, "Satisfaction item 4",
    "q5", "integer", "", TRUE, FALSE, "1", "5", NA, "Satisfaction item 5",
    "q6", "integer", "", TRUE, FALSE, "1", "5", NA, "Satisfaction item 6",
    "consent", "category", "", TRUE, FALSE, NA, NA, "yes;no", "Consent given",
    "respondent_phone", "text", "", FALSE, FALSE, NA, NA, NA, "Telephone of respondent",
    "notes", "text", "", FALSE, FALSE, NA, NA, NA, "Enumerator notes"
  ))
}

# Writes the survey, its specification and a rule to inst/extdata.
write_messy_survey <- function(dir = "inst/extdata") {
  readr::write_csv(messy_survey(), file.path(dir, "messy_survey.csv"), na = "")
  write_spec(messy_survey_spec(), file.path(dir, "messy_survey_spec.csv"))
  readr::write_csv(tibble::tibble(rule_id = c("consented", "adult"),
                                  expression = c('!is.na(consent) & consent == "yes"',
                                                 'is.na(age_years) | age_years == "" | as.numeric(age_years) >= 18'),
                                  description = c("only consenting respondents are interviewed",
                                                  "respondents are adults")),
                   file.path(dir, "messy_survey_rules.csv"))
}
