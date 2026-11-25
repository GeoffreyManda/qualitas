# qualitas 0.2.0

* `inspect_workbook()` inspects original XLSX structure and storage, including headers, hidden data, merged ranges, filters, formulas and workbook metadata, without extracting untrusted ZIP paths.
* `compare_records()` reconciles complete unique composite keys and reports added, removed and changed records, with optional documented correction authority.
* `assess_dependencies()` and `assess_distributions()` check functional dependencies and categorical group differences with explicit denominators and limits.
* `assess_geography()` screens geographic pairs, polygon membership, distance from site centres and repeated locations using explicit CRS and boundary provenance.
* `assess_questionnaire()` checks structured question identity, languages, choices, routing targets, numeric bands, units and event reference periods.
* `assess_requirements()` links scoped requirements to hashed reviewed evidence and keeps missing, unverified, expired, failed and explained evidence distinct.
* `assess_precision()` checks declared measurement resolution; `assess_missing_patterns()` uses explicit expected-answer masks.

* Expected-answer masks distinguish missing expected answers, skipped answers and unresolved applicability. Choice contracts check allowed codes, duplicate selections, cardinality and exclusive options.
* Shared dictionaries and indicator definitions can be checked for required metadata and duplicate identities.
* Composite primary and foreign keys are checked without joining text values or coercing types. Missing references and ambiguous parent keys remain separate findings.
* Delivery manifests are checked against SHA-256 hashes, required document roles and safe relative paths.
* Reported counts can be reconciled with matched delivered counts, including zero denominators and invalid counts.
* PL04 screens coordinate pairs against declared geographic bounds. This screen does not establish that a field visit occurred.
* Review decisions bind to a source snapshot and retain the original flags and scores.

# qualitas 0.1.1

* FA06 counts an enumerator and a measure as one unit, both for the pairs it compares and for the pairs it flags; pairs that cannot be compared are not counted.
* CS04 counts cells, both for the filled values it examines and for the text values it flags.
* A rule that cannot be evaluated sets CS01 to `error` instead of being listed as a broken rule, and a check that could not run in full has no score.

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
