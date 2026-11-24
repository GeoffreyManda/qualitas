# Reading dates and date-times written in the common formats: ISO
# (2024-10-30 22:45:34), month first (10/30/2024 10:45:34 PM, as Excel and
# many survey exports write) and day first (30/10/2024 22:45). A column is
# read with the format that reads most of its values, so a day above 12
# settles whether the month or the day comes first; ties go to the format
# listed first, ISO before month-first before day-first.
moment_formats <- c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d %H:%M",
                    "%Y-%m-%d", "%m/%d/%Y %I:%M:%S %p", "%m/%d/%Y %I:%M %p",
                    "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y",
                    "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y", "%d.%m.%Y",
                    "%d-%m-%Y")

# A column's values as date-times (UTC); values that do not read are missing.
as_time <- function(x) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x))
  if (inherits(x, "Date")) return(as.POSIXct(x))
  # A date is short; long free text is not read, since the parser stops on
  # strings of more than about a thousand characters.
  text <- trimws(as_text(x))
  text <- dplyr::if_else(nchar(text) <= 40, text, NA_character_)
  # AM and PM are read in the C locale, where they are always AM and PM.
  locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", locale), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  parsed <- purrr::map(moment_formats, \(f) as.POSIXct(text, format = f, tz = "UTC"))
  parsed[[which.max(purrr::map_int(parsed, \(p) sum(!is.na(p))))]]
}

# A column's values as dates; values that do not read as a date are missing.
as_day <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(as_time(x))
}

# Whether every text value reads as a date, written with separators.
reads_as_dates <- function(text) {
  length(text) > 0 && all(grepl("[-/.]", text)) && all(!is.na(as_time(text)))
}
