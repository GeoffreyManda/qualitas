# qualitas 0.1.0

First release.

* `assess()` runs 38 checks in eleven dimensions against a data specification.
* A specification can be written by hand, read from a CSV or YAML file, or inferred with `infer_spec()`.
* Each check is mapped to ISO/IEC 25012, Kahn et al. (2016) and dataquieR; it reports the units it examined, the units it flagged and the share that passed.
* Dimension scores weight checks by severity.
* `read_rules()` reads rules that records must keep.
* `cleaning_log()` and `apply_log()` record and apply cleaning decisions.
* `compare_data()` finds drift between two deliveries of the same data.
* `assess_tables()` assesses linked tables and the keys between them.
* `report()` writes an HTML page or a Word document; `write_results()` writes CSV and JSON files.
* `inst/bin/qualitas.R` assesses a data file from the command line.
* `inst/benchmark/run-benchmark.R` assesses a random sample of public data sets from the Rdatasets archive.
* `inst/skill/SKILL.md` tells an AI agent how to run an assessment.
