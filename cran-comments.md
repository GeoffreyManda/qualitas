## First submission

qualitas provides data-quality assessment against reviewed specifications and
record-level rules, with row-level findings, explicit denominators, review logs,
and HTML or Word reports. The package contains no compiled code.

## Example data and licensing

The bundled survey is synthetic. Its generator and planted-defect definitions
are included in tests/testthat/helper-survey.R. It contains no collected
respondent or client data. Tests that use survival::pbc and dplyr::storms use
those packages' public datasets through their declared dependencies.

The package is MIT licensed. The designated maintainer and copyright holder
are identified in DESCRIPTION and LICENSE. Method references and check mappings
are included in the check catalogue and vignette.

## Check results

Checked on macOS arm64 with R 4.5.3 (aarch64-apple-darwin20), using:

R CMD check --as-cran --no-manual

0 errors | 0 warnings | 1 note

The note identifies the maintainer and states that this is a new submission.
Help examples, the test suite, vignette building and installed vignette checks
passed. No missing documentation entries, Rd cross-reference failures, usage
mismatches or code/documentation mismatches were reported.

The system-clock check was disabled for this local run. The PDF manual was not
checked because pdflatex is not installed. A full manual check and checks of
this documentation revision on Windows and Linux remain to be completed before
submission. Previous multi-platform checks cover an earlier package revision.

## Downstream dependencies

This is the first CRAN submission. No reverse-dependency result is claimed.
