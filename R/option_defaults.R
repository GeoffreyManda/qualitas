# The default settings, documented in qualitas_options().
option_defaults <- function() {
  list(
    as_of = Sys.Date(),
    missing_codes = c("NA", "N/A", "n/a", "na", "NULL", "null", "-", "--", ".", "?",
                      "none", "None", "unknown", "Unknown", "-99", "-999", "9999", "-9"),
    record_missing_share = 0.5, outlier_z = 3.5, group_min_n = 10L,
    near_duplicate_share = 0.95, digit_p = 0.001, digit_min_n = 50L, digit_min_iqr = 20,
    digit_score_max = 20,
    digit_coarse_share = 0.95,
    digit_max_places = 2L, log_skew_min = 0.2,
    benford_mad = 0.015, benford_min_n = 1000L, whipple_max = 110,
    mixed_type_share = 0.8, min_duration = NA_real_, short_duration_share = 1 / 3,
    longest_interview = NA_real_, working_hours = NA_character_,
    alignment_share = 0.9, alignment_record_share = 0.5, alignment_min_cells = 3L,
    alignment_min_columns = 5L,
    straightline_min_items = 5L, enumerator_difference = 0.5,
    max_age_days = NA_real_, period = NA_character_,
    email_pattern = "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}",
    phone_pattern = "(?<![\\d.])\\+?(?:\\d[\\s().-]?){8,14}\\d(?![\\d.])",
    pii_name_pattern = paste0(
      "(^|[^[:alpha:]])((first|last|full|family|given|sur|respondent|beneficiary|",
      "interviewee|enumerator|person|contact|head|owner|mother|father)[ _]?name|",
      "phone|telephone|mobile|whatsapp|tel|e-?mail|address|national[ _]?id|nid|",
      "passport|dob|birth[ _]?date|date[ _]of[ _]birth|gps|latitude|longitude|",
      "lat|lon|long|lng)([^[:alpha:]]|$)|^name$|",
      # The Arabic words for name, telephone, mobile and address.
      "\u0627\u0633\u0645|\u0647\u0627\u062a\u0641|\u062c\u0648\u0627\u0644|",
      "\u0627\u0644\u0639\u0646\u0648\u0627\u0646"),
    drift_missing = 0.1, drift_shift = 0.5, drift_psi = 0.25,
    rules = NULL,
    tables = list(), relations = NULL
  )
}
