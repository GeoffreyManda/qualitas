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

Checked on 24 November 2026 with R CMD check --as-cran, including the
PDF manual, examples, tests and executable vignette.

| Environment | Errors | Warnings | Notes |
| --- | ---: | ---: | ---: |
| Local macOS arm64, R 4.5.3 | 0 | 0 | 2 |
| GitHub macOS release, R 4.6.2 | 0 | 0 | 1 |
| GitHub Windows release, R 4.6.2 UCRT | 0 | 0 | 0 |
| GitHub Ubuntu release, R 4.6.2 | 0 | 0 | 1 |
| GitHub Ubuntu R-devel | 0 | 0 | 1 |
| GitHub Ubuntu previous release, R 4.5.3 | 0 | 0 | 1 |

The local incoming-feasibility note identifies the maintainer and states
“New submission”. The other local note concerns an outdated HTML Tidy.
The macOS and Ubuntu CI notes concern unavailable HTML Tidy; HTML validation
passed on Windows. PDF manual checks passed in every environment. All five
CI jobs passed 98 test expectations, with no test warnings or skipped tests.
The system-clock check was disabled. CI uses the r-lib action defaults for
incoming checks; the local run also performed CRAN incoming feasibility.

Completed workflow:
https://github.com/GeoffreyManda/qualitas/actions/runs/45393674740

Checked repository revision: 7aec770a818ddc2a584792199ac9ba0016b06f5e.

## Review artifacts

The indexed reference manual has 25 pages: outputs/qualitas-manual.pdf.
The source archive is outputs/qualitas_0.1.0.tar.gz, built using R CMD build.
It includes the installed worked vignette. Local plans, agent configuration,
generated outputs and manual-build working directories are excluded.

Source archive SHA256:
ef6b8c3df13d4ce02a9d004564368f18b55060e9d44d6dc7537dea13949b4823

The initial check exposed an unavailable test helper and a warning when
summarising an assessment with no applicable checks. Both were corrected;
the linked-table regression now verifies that this case emits no warning
and returns no dimension scores. CI now installs Courier font metrics and
makeindex explicitly, with a bounded installer and TeX Live's own extraction
tools on Windows.

The package is prepared for maintainer review. It has not been submitted.
CRAN acceptance remains subject to CRAN's review and checks. Submission
requirements were checked against https://cran.r-project.org/web/packages/policies.html.

## Downstream dependencies

This is the first CRAN submission. No reverse-dependency result is claimed.

## Submission initiated, 24 November 2026

Following the maintainer's instruction to submit, the checked archive above
was uploaded through https://cran.r-project.org/submit.html and the Submit
package action completed. The upload review identified qualitas 0.1.0 and
Geoffrey Manda as maintainer. Upload record: 363994.

CRAN's confirmation page states that a confirmation email has been sent to
the maintainer, geofmanda@gmail.com. The submission is not complete until
the maintainer follows the confirmation link. CRAN review has not yet been
confirmed. This updates the earlier pre-submission status above.

## Maintainer confirmation completed, 24 November 2026

At the maintainer's explicit request, the CRAN confirmation email for
qualitas 0.1.0 was opened in geofmanda@gmail.com and its confirmation link
followed. The form acknowledged the CRAN policies and the completed
R-devel --as-cran check. The final Upload the Package to CRAN action
returned: "The package has been uploaded successfully to CRAN submission
team." Submission is now confirmed and awaiting CRAN processing/review.
This supersedes the pending-confirmation status above; acceptance is not
yet established.

## Corrected journal snapshot, 24 November 2026

Version 0.1.1 incorporates the committed FA06, CS04 and CS01 repairs and additional regression coverage. The source archive passed local R CMD check --as-cran with manual and vignettes enabled: 0 errors, 0 warnings, 2 notes (New submission and outdated HTML Tidy). All 129 test expectations passed. A stray pre-existing texput.log is excluded from the source build. This is the corrected JSS submission snapshot; the pending CRAN 0.1.0 archive remains unchanged and no replacement has been submitted to CRAN.

## GitHub development release 0.2.0, 25 November 2026

The GitHub release adds workbook structure inspection, explicit answer and metadata contracts, record reconciliation, geographic screens, questionnaire checks and scoped requirement evidence. Synthetic regression fixtures cover unsafe XLSX structures, ambiguous keys, unresolved rules, missing evidence and zero denominators. These mechanisms do not establish the truth of source declarations or organizational compliance. The original CRAN 0.1.0 submission remains unchanged. Version 0.2.0 will not be submitted while that review is pending; the maintainer has requested waiting for CRAN's reply. Final check results are recorded below after completion.

Final 0.2.0 local R CMD check --as-cran, including installation, tests, examples, executable vignette and PDF reference manual: 0 errors, 0 warnings, 2 notes. The notes concern new-submission status and an outdated local HTML Tidy binary. The namespace note found during integration was repaired and the complete check repeated successfully. The source archive contains no client data, manuscript, internal plans or agent configuration.
