# qualitas

qualitas assesses the quality of a data set in R. You describe what the data should hold, or let the package infer it; `assess()` runs 38 checks and returns every problem with its row, column and value, a measure for each check and a score for each of eleven dimensions. Each check is mapped to ISO/IEC 25012, to the harmonised framework of Kahn et al. (2016) and to dataquieR (Schmidt et al. 2021). A cleaning log records what is done about each problem. Reports can be written in four formats: HTML, Word, CSV and JSON.

## Installation

```r
# install.packages("remotes")
remotes::install_github("GeoffreyManda/qualitas")
```

## A first assessment

The package includes a made-up household survey of 302 interviews with defects planted at known rows, its specification and two rules.

```r
library(qualitas)

survey <- read.csv(system.file("extdata", "messy_survey.csv", package = "qualitas"))
spec <- system.file("extdata", "messy_survey_spec.csv", package = "qualitas")
rules <- system.file("extdata", "messy_survey_rules.csv", package = "qualitas")

result <- assess(survey, spec = spec, rules = rules,
                 options = qualitas_options(as_of = as.Date("2026-11-24")))
scores(result)
#>         dimension checks checks_with_issues score lowest
#> 1     fabrication      5                  5 0.789  0.000
#> 2 confidentiality      2                  2 0.945  0.944
#> 3    plausibility      2                  2 0.992  0.991
#> 4    completeness      4                  3 0.992  0.947
#> 5      uniqueness      3                  2 0.993  0.987
#> 6     consistency      4                  3 0.996  0.990
#> 7        validity      4                  4 0.998  0.997
#> 8       structure      6                  0 1.000  1.000
#> 9   documentation      1                  0 1.000  1.000
```

`issues(result)` lists the 677 problems found, one row each, and `measures(result)` gives one row per check with the units it examined, the units it flagged and the share that passed.

For check UN02 the share is 0.987: 4 of the 302 records hold a repeated interview identifier.

Fabrication scores lowest. Ages heap on numbers ending in 0 or 5 (Whipple index 258), the last digits of ages are not uniform, eight interviews lasted under eight minutes, ten records give the same answer to all six satisfaction items, and one enumerator's weights run 12 kg above the other enumerators' weights.

The Benford test does not apply here, because no column is declared an amount.

## Dimensions

| Dimension | Checks | What they find |
|---|---|---|
| structure | ST01 to ST07 | empty, constant or badly named columns; columns missing from the data or the specification; values that break the declared type; text that is invalid UTF-8; records whose values sit under the wrong headers |
| completeness | CO01 to CO04 | missing values, missing required values, codes such as -99 that stand for a missing value, records missing most values |
| uniqueness | UN01 to UN03 | repeated records, repeated keys, records that match another on nearly all values |
| validity | VA01 to VA04 | values outside the allowed list, outside the range, not matching a pattern, or dated after the assessment |
| consistency | CS01 to CS04 | broken rules, labels written in more than one way, stray spaces, text in a numeric column |
| plausibility | PL01 to PL03 | outliers in a column or within a group, dates before the date they must follow |
| integrity | IN01 | keys that match no record of a linked table |
| timeliness | TI01 to TI02 | data older than allowed, periods without records |
| fabrication | FA01 to FA07 | last-digit preference, age heaping, departures from Benford's law in declared amounts, interviews too short or too long, straightlining, enumerators whose records differ, interviews outside the working hours |
| confidentiality | PI01 to PI02 | e-mail addresses and telephone numbers in values; column names that suggest personal data |
| documentation | DO01 | columns the specification does not describe |

`checks_catalogue()` lists every check with its unit, severity, mapping and source.

## Specifications

A specification has one row per column: its type (`integer`, `number`, `date`, `datetime`, `logical`, `category`, `text`), its role (`id`, `date`, `enumerator`, `group`, `age`, `duration`, `start`, `end`, `amount`), whether it is required and unique, its minimum and maximum, its allowed values separated by `;`, a pattern, its own missing-value codes, a description and the column whose date it must follow.

```r
spec <- infer_spec(survey)    # a first draft from the data
write_spec(spec, "spec.csv")  # review and complete it in a spreadsheet
result <- assess(survey, spec = "spec.csv")
```

Review an inferred specification before you rely on it, because it records the data as found. Without a specification, `assess()` infers one.

## Rules

A rule is an R expression that holds for a valid record. Rules can be given directly or kept in a CSV file with the columns `rule_id`, `expression` and `description`:

```r
assess(survey, spec = spec, rules = c(adult = "age_years >= 18",
                                      consented = "consent == 'yes'"))
```

## Cleaning log

```r
log <- cleaning_log(result)          # one row per problem that points to a record
readr::write_csv(log, "cleaning-log.csv")
# set action to keep, change or remove; give new_value and reason
cleaned <- apply_log(survey, "cleaning-log.csv")
```

`apply_log()` makes only the changes and removals the log records, and keeps the applied log as the `cleaning` attribute of the cleaned data.

## Two deliveries of the same data

```r
compare_data(last_month, this_month)
```

For each column, `compare_data()` reports whether it was added, removed or changed type, the change in its share of missing values, and how far its values moved: the shift of the median in median absolute deviations for numbers, the population stability index for categories.

## Linked tables

```r
links <- data.frame(from = "members", column = "hh_id", table = "households", key = "hh_id")
assess_tables(list(households = households, members = members), relations = links)
```

## Reports and results

```r
report(result, "quality.html")   # or "quality.docx", with officer and flextable installed
write_results(result, "results") # issues, measures and scores as CSV, everything as JSON
```

From a shell, `inst/bin/qualitas.R` assesses a CSV file and exits with status 1 when a critical check finds a problem, so a calling script can stop:

```sh
Rscript "$(Rscript -e 'cat(system.file("bin", "qualitas.R", package = "qualitas"))')" \
  data.csv --spec spec.csv --report quality.html --results results
```

## Settings

`qualitas_options()` holds every threshold, each with its default and source in its help page. Change any by name, for example `qualitas_options(outlier_z = 3, max_age_days = 30, period = "month")`.

## How measures and scores are computed

Following Pipino, Lee and Wang (2002), each check states its result as a ratio. It counts the units it examined, which may be cells, records, columns or the whole table, the units it flagged, and the score 1 - flagged / checked. A dimension's score is the mean of its checks' scores, weighted 4, 2 and 1 for critical, major and minor checks; the overall score is the mean of the dimension scores. A check that examined nothing does not count.

Outliers use the modified z-score of Iglewicz and Hoaglin (1993) with the limit 3.5, the limit used in the WHO Data Quality Review (2017). The digit preference score follows the SMART methodology, the Whipple index the United Nations grading, the Benford test the mean absolute deviation limits of Nigrini (2012), and near duplicates Kuriakose and Robbins (2016).

## Benchmark on public data

`inst/benchmark/run-benchmark.R` assesses a random sample of public data sets from the Rdatasets archive, one per source package, and records for each the time taken, the checks that could not run and what every check flagged.

## Use with an AI agent

`inst/skill/SKILL.md` is a skill file that tells an agent how to run an assessment with qualitas and report it.

## Citation

```r
citation("qualitas")
```

MIT licence. Copyright 2026 Geoffrey Manda.

## Worked guide

After installing a build that includes vignettes, open the complete synthetic workflow:

```r
vignette("qualitas", package = "qualitas")
```

The guide covers the observation unit, a reviewed specification, findings and denominators, rules, a cleaning log, reassessment, delivery comparison and report export. Its source is [vignettes/qualitas.Rmd](vignettes/qualitas.Rmd). For a GitHub installation with the guide, use `remotes::install_github("GeoffreyManda/qualitas", build_vignettes = TRUE)` with `knitr`, `rmarkdown` and Pandoc installed.

## Preparing a source release

Build the source package and check the resulting archive before submission:

```sh
R CMD build .
R CMD check --as-cran qualitas_0.1.0.tar.gz
```

A full manual check requires a TeX installation. Submission notes are maintained in `cran-comments.md`; they state the actual check environments and any outstanding results. The package has not yet been accepted by CRAN.
