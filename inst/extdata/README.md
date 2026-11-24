# Synthetic survey fixtures

`messy_survey.csv` contains generated interviews with planted defects. No interviews were collected and no respondent or client records are included.

The generator is `messy_survey()` in `tests/testthat/helper-survey.R`. It uses seed 20261124, creates 300 synthetic observations and appends two records to exercise duplicate detection. It generates identifier, district, demographic, measurement, interview-duration and item-response fields. Telephone-like strings are generated solely to exercise privacy checks. The free-text email uses the reserved example.org domain.

`messy_survey_spec.csv` describes the expected column types, roles and constraints. `messy_survey_rules.csv` supplies the example record-level rules. The specification and rules belong with this fixture and do not prescribe requirements for another survey.

The fixtures are distributed under the package's MIT licence.
