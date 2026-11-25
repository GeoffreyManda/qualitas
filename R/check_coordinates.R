#' Screen latitude and longitude pairs
#'
#' Both coordinates must be present, numeric, finite and within the declared
#' rectangle. This is a bounding-box screen and cannot establish a visit or
#' membership of a country or administrative polygon. Zero is a valid coordinate.
#' @inheritParams structure_checks
#' @return A [check_result()], counting records.
#' @export
#' @examples
#' data <- data.frame(lat = c(15, 95), lon = c(44, 45))
#' check_coordinates(data, infer_spec(data), qualitas_options(
#'   latitude = "lat", longitude = "lon"))
check_coordinates <- function(data, spec, options) {
  lat <- options$latitude
  lon <- options$longitude
  if (is.null(lat) && is.null(lon)) return(check_result())
  if (!is.character(lat) || !is.character(lon) ||
      length(lat) != 1 || length(lon) != 1 || is.na(lat) || is.na(lon) ||
      !all(c(lat, lon) %in% names(data)) || identical(lat, lon)) {
    return(check_result(error = "Latitude and longitude must name distinct existing columns."))
  }
  bounds <- options$coordinate_bounds %||% c(-90, 90, -180, 180)
  if (!is.numeric(bounds) || length(bounds) != 4 || any(!is.finite(bounds)) ||
      bounds[1] > bounds[2] || bounds[3] > bounds[4] ||
      bounds[1] < -90 || bounds[2] > 90 || bounds[3] < -180 || bounds[4] > 180) {
    return(check_result(error = "Bounds must be ordered latitude then longitude limits."))
  }
  y <- as_number(data[[lat]])
  x <- as_number(data[[lon]])
  bad <- !is.finite(y) | !is.finite(x) | y < bounds[1] | y > bounds[2] |
    x < bounds[3] | x > bounds[4]
  check_result(tibble::tibble(column = paste(lat, lon, sep = ";"), row = which(bad),
    value = NA_character_, message = rep("coordinate pair is incomplete or outside bounds",
                                         sum(bad))), checked = nrow(data))
}
