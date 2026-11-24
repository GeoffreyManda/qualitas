---
name: qualitas
description: Assess whether a data set is fit for use with the R package qualitas, which runs 35 checks in eleven dimensions against a specification and reports issues, measures and scores. Use when asked to check, audit, profile or clean a data set.
---

# Data quality assessment with qualitas

Use qualitas when a person asks whether a data set is fit for use, or asks you to check, audit or clean one.

## Steps

1. Read the data so that codes for missing values stay visible: `readr::read_csv(path, na = "")`.
2. Find the specification. Ask for the codebook first; if none exists, run `infer_spec(data)`, write it with `write_spec()`, then correct the types, roles, ranges and allowed values from the questionnaire.
3. Write the rules the records must keep as R expressions, one per row of a CSV file with the columns `rule_id`, `expression` and `description`.
4. Run the assessment: `result <- assess(data, spec = "spec.csv", rules = "rules.csv", options = qualitas_options(as_of = Sys.Date()))`.
5. Read `scores(result)` first, then `measures(result)` for the checks that found issues, then `issues(result)` for the records.
6. Write the report with `report(result, "quality.html")`, or `"quality.docx"` for Word, and the results with `write_results(result, "results")`.
7. To clean the data, write `cleaning_log(result)` to a CSV file, let a person decide the action for each row, then run `apply_log(data, log)`. Change no value without a recorded decision.

## Reading the results

- A measure gives the units checked, the units flagged and the score 1 - flagged / checked; report all three, with the unit.
- A check whose status is `not applicable` examined nothing. Say so, and do not count it as passed.
- A check whose status is `error` could not run; report its message.
- Treat the fabrication checks, FA01 to FA06, as signs to follow up with the field team.
- Results against an inferred specification are provisional until a person has reviewed the specification.
- Remove or mask personal data found by PI01 or PI02 before results are shared.

## Settings that often need changing

- `as_of`: the date the data are judged against, used for future dates and currentness.
- `max_age_days` and `period`: set them to check timeliness.
- `outlier_z` (3.5), `group_min_n` (10) and `min_duration`: adjust them to the data at hand.
- `missing_codes`: add the codes the questionnaire uses for answers such as "don't know" or "refused".

## Linked tables and repeated deliveries

Use `assess_tables()` with a `relations` table (`from`, `column`, `table`, `key`) for linked tables, and `compare_data(old, new)` for a new delivery of data already assessed.
