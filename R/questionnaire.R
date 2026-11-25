#' Check a structured questionnaire definition
#'
#' Checks declared question identity, ordering, routing, units and translated
#' code sets. This cannot assess wording, interpretation or translation quality.
#' Those decisions require a reviewed evidence record in [assess_requirements()].
#' @param questions Table with `question_id`, `language`, `order`, `type`,
#'   `text`, `source`; optional `route_to` is a semicolon-separated target list,
#'   `unit` is required for `number`, and `period` for `event_count`.
#' @param choices Optional table with `question_id`, `language`, `code`, `label`;
#'   numeric `lower` and `upper` define mutually exclusive closed answer bands.
#' @param languages Languages required for every question; NULL uses observed languages.
#' @return Defect table with requirement, row and message; no response records.
#' @export
#' @examples
#' q <- data.frame(question_id = "q1", language = "en", order = 1,
#'   type = "text", text = "Describe the visit", source = "form v1")
#' assess_questionnaire(q)
assess_questionnaire <- function(questions, choices = NULL, languages = NULL) {
  fields <- c("question_id", "language", "type", "text", "source")
  found <- required_metadata(questions, fields)
  if (!all(c(fields, "order") %in% names(questions))) {
    return(dplyr::bind_rows(found, contract_defects("question_order", NA_integer_,
                                                   "question order column is required")))
  }
  if (nrow(found)) return(found)
  q <- questions
  if (!all(q$type %in% c("text", "single", "multiple", "number", "integer", "event_count",
                          "date", "datetime", "logical"))) cli::cli_abort("Unknown question type.")
  if (!is.numeric(q$order) || any(!is.finite(q$order) | q$order < 1 | q$order != floor(q$order))) {
    return(contract_defects("question_order", NA_integer_, "orders must be positive integers"))
  }
  languages <- languages %||% unique(q$language)
  keys <- q[c("question_id", "language")]
  repeat_id <- duplicated(keys) | duplicated(keys, fromLast = TRUE)
  found <- dplyr::bind_rows(found, contract_defects("question_identity", which(repeat_id),
                                                  "question identifier repeats in language"))
  checks <- purrr::map(seq_len(nrow(q)), \(i) {
    routes <- if ("route_to" %in% names(q)) split_list(q$route_to[i]) else character()
    peers <- q$question_id[q$language == q$language[i]]
    bad <- c(route_target = any(!routes %in% peers),
      question_unit = q$type[i] == "number" &&
        (!"unit" %in% names(q) || is_blank(q$unit[i])),
      question_period = q$type[i] == "event_count" &&
        (!"period" %in% names(q) || is_blank(q$period[i])))
    rules <- names(bad)[bad]
    contract_defects(rules, rep(i, length(rules)), gsub("_", " ", rules))
  })
  missing <- purrr::map(unique(q$question_id), \(id) {
    absent <- setdiff(languages, q$language[q$question_id == id])
    contract_defects("question_translation", rep(NA_integer_, length(absent)),
                      paste("question", id, "has no declaration in", absent))
  })
  orders <- duplicated(q[c("language", "order")])
  found <- dplyr::bind_rows(found, purrr::list_rbind(checks), purrr::list_rbind(missing),
    contract_defects("question_order", which(orders), "question order repeats in language"))
  if (is.null(choices)) {
    needed <- which(q$type %in% c("single", "multiple"))
    return(dplyr::bind_rows(found, contract_defects("question_choices", needed,
                                                   "choice definition is absent")))
  }
  malformed <- required_metadata(choices, c("question_id", "language", "code", "label"))
  if (nrow(malformed)) return(dplyr::bind_rows(found, malformed))
  orphan <- !vapply(seq_len(nrow(choices)), function(i) any(
    q$question_id == choices$question_id[i] & q$language == choices$language[i]), logical(1))
  found <- dplyr::bind_rows(found, contract_defects("choice_question", which(orphan),
    "choice question and language pair is not declared"))
  repeated <- duplicated(choices[c("question_id", "language", "code")])
  found <- dplyr::bind_rows(found, contract_defects("choice_code", which(repeated),
                                                  "choice code repeats in a question"))
  for (id in unique(q$question_id[q$type %in% c("single", "multiple")])) {
    sets <- lapply(languages, \(lang) sort(choices$code[choices$question_id == id &
                                                      choices$language == lang]))
    if (any(lengths(sets) == 0) || any(!vapply(sets, identical, logical(1), sets[[1]]))) {
      found <- dplyr::bind_rows(found, contract_defects("choice_translation", NA_integer_,
                                                       paste("code sets differ or are absent:", id)))
    }
  }
  if (xor("lower" %in% names(choices), "upper" %in% names(choices)))
    return(dplyr::bind_rows(found, contract_defects("answer_band",
      if (nrow(choices)) seq_len(nrow(choices)) else NA_integer_, "answer bands require both lower and upper columns")))
  if (all(c("lower", "upper") %in% names(choices))) {
    if (!is.numeric(choices$lower) || !is.numeric(choices$upper)) {
      return(dplyr::bind_rows(found, contract_defects("answer_band", NA_integer_,
                                                      "answer bounds must be numeric")))
    }
    for (i in seq_len(nrow(choices))) {
      peers <- which(choices$question_id == choices$question_id[i] &
                       choices$language == choices$language[i] & seq_len(nrow(choices)) < i)
      bad <- is.na(choices$lower[i]) || is.na(choices$upper[i]) ||
        choices$lower[i] > choices$upper[i] || any(choices$lower[i] <= choices$upper[peers] &
          choices$upper[i] >= choices$lower[peers], na.rm = TRUE)
      if (bad) found <- dplyr::bind_rows(found, contract_defects("answer_band", i,
        "numeric band is missing reversed or overlaps a closed band"))
    }
  }
  found
}
