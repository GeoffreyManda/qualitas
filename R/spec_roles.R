# The role of each column, guessed from the words of its name. Exports often
# join a variable name and a label, as in "pk_form | Id" or
# "createdat_form | Created At"; splitting a name into words finds the role
# in either part.

# The roles of columns from their names: an identifier, the enumerator, the
# start or end of the interview, its length, an age, a date or a grouping
# place; "" for any other column.
infer_role <- function(names) {
  words <- name_words(names)
  dplyr::case_when(
    has_word(words, "id|pk|uuid|key|code") | grepl("^(id|pk|uuid)_", tolower(names)) ~ "id",
    has_word(words, paste("enumerator|enumeratorname|interviewer|surveyor|collector|",
                          "assignedto|assigned to|submitted by|submittedby|username", sep = "")) ~
      "enumerator",
    has_word(words, "createdat|created at|starttime|start time|start|started") ~ "start",
    has_word(words, "endat|end at|endtime|end time|end|completed at|submission time") ~ "end",
    has_word(words, paste("interview duration|interview length|interview time|",
                          "duration min|durationmin|length min|minutes", sep = "")) ~ "duration",
    has_word(words, "age|age years") ~ "age",
    grepl("date", words) ~ "date",
    has_word(words, paste("region|district|subdistrict|sub district|province|state|governorate|",
                          "site|facility|village|locality|county|ward", sep = "")) ~ "group",
    .default = ""
  )
}

# Each name in lower case with every run of other characters made one space,
# padded with a space at each end.
name_words <- function(names) {
  paste0(" ", trimws(gsub("[^[:alnum:]]+", " ", tolower(names))), " ")
}

# Whether the words of each name hold one of the given words or phrases.
has_word <- function(words, phrases) grepl(paste0(" (", phrases, ") "), words)
