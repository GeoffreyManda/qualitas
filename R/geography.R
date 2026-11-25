#' Screen geographic pairs, polygon membership and repeated locations
#'
#' Coordinates are WGS84 decimal degrees in longitude/latitude order. Numeric
#' text is parsed strictly; zero is valid. Polygon edges count as inside.
#' Distances use a spherical Earth (radius 6371008.8 m). Site centres are
#' coordinate-wise medians; antimeridian-spanning groups are not assessed.
#' Repeated locations count neighbours on the same Date, including the row.
#' These configurable screens cannot establish a visit or fabrication.
#' @param data Atomic data frame.
#' @param longitude,latitude Single column names, numeric or character.
#' @param site Optional site column name; missing sites are not grouped.
#' @param day Optional Date column name; times must be converted explicitly.
#' @param boundaries Optional sf/sfc POLYGON or MULTIPOLYGON features. With no
#'   assignment, their union is tested; no bounding rectangle substitutes for it.
#' @param assignment Optional column name of integer feature indices (admin areas).
#' @param boundary_source Nonblank boundary source/version reference.
#' @param crs Must explicitly be 4326 (input WGS84); polygon CRS must be known.
#' @param axis Must explicitly be `longitude_latitude`.
#' @param distance_m Site-median screening limit in metres, inclusive exceedance.
#' @param cluster_m Same-day neighbour radius in metres, inclusive.
#' @param cluster_n Minimum same-day neighbour count, including self, at least 2.
#' @param max_pairs Maximum comparisons for same-day or polygon screening.
#' @param boundary_tolerance_m Polygon-edge distance tolerance in metres (1e-6).
#'   Points within this distance count as inside, including numerical edge error.
#' @return List of row-indexed results and settings. Untested screens are NA;
#'   malformed input errors, and empty input is not_applicable. Boundary source,
#'   CRS and axis metadata are retained. Raw coordinates are not returned.
#' @name assess_geography
#' @export
assess_geography <- function(data, longitude, latitude, crs, axis, site = NULL, day = NULL,
                             boundaries = NULL, assignment = NULL, boundary_source = NULL,
                             distance_m = 10000, cluster_m = 10, cluster_n = 3L, max_pairs = 1e6, boundary_tolerance_m = 1e-6) {
  advanced_frame(data, c(longitude, latitude, site, day, assignment))
  if (length(longitude) != 1L || length(latitude) != 1L ||
      !identical(crs, 4326) || !identical(axis, "longitude_latitude")) stop("Declare CRS 4326 and longitude_latitude axis.")
  limits <- list(distance_m, cluster_m, cluster_n, max_pairs, boundary_tolerance_m)
  if (!all(vapply(limits, function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0, logical(1))) ||
      cluster_n < 2 || cluster_n != floor(cluster_n)) stop("Invalid screening limits.")
  if (length(site) > 1L || length(day) > 1L || length(assignment) > 1L ||
      (!is.null(day) && !inherits(data[[day]], "Date"))) stop("Use single site, assignment and Date columns.")
  parse <- function(x) {
    if (!(is.numeric(x) && !is.object(x)) && !is.character(x)) stop("Coordinates must be numeric or decimal text.")
    if (is.character(x)) {
      valid <- !is.na(x) & grepl("^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$", trimws(x))
      x <- suppressWarnings(as.numeric(ifelse(valid, trimws(x), NA_character_)))
    }; x
  }
  x <- parse(data[[longitude]]); y <- parse(data[[latitude]])
  missing <- function(v) is.na(v) | (is.character(v) & trimws(v) == "")
  mx <- missing(data[[longitude]]); my <- missing(data[[latitude]])
  good <- is.finite(x) & is.finite(y) & abs(x) <= 180 & abs(y) <= 90
  result <- tibble::tibble(row = seq_len(nrow(data)), pair = ifelse(mx & my, "missing",
    ifelse(xor(mx, my), "incomplete", ifelse(good, "valid", "invalid"))),
    inside = NA, distance_m = NA_real_, distant = NA, neighbours = NA_integer_, clustered = NA)
  polygon_crs <- NULL
  if (!is.null(assignment) && is.null(boundaries)) stop("Assignment needs boundaries.")
  if (!is.null(boundaries)) {
    if (!requireNamespace("sf", quietly = TRUE)) stop("Polygon checks require optional package sf.")
    if (length(boundary_source) != 1L || !is.character(boundary_source) || is.na(boundary_source) || !nzchar(trimws(boundary_source))) stop("Supply boundary source/version.")
    if (!inherits(boundaries, "sf") && !inherits(boundaries, "sfc")) stop("Supply sf polygon geometry.")
    polygon_crs <- sf::st_crs(boundaries)
    if (is.na(polygon_crs) || !length(sf::st_geometry(boundaries)) ||
        !all(sf::st_geometry_type(boundaries) %in% c("POLYGON", "MULTIPOLYGON")) ||
        any(sf::st_is_empty(boundaries)) || !all(sf::st_is_valid(boundaries))) stop("Need valid nonempty polygons with known CRS.")
    if (!is.null(assignment) && (!is.integer(data[[assignment]]) ||
        any(!is.na(data[[assignment]]) & (data[[assignment]] < 1L | data[[assignment]] > length(sf::st_geometry(boundaries)))))) stop("Assignment must contain integer feature indices or NA.")
    ids <- which(good)
    if (length(ids)) {
      points <- sf::st_as_sf(data.frame(x = x[ids], y = y[ids]), coords = c("x", "y"), crs = 4326)
      if (length(ids) * as.double(length(sf::st_geometry(boundaries))) > max_pairs) stop("Polygon comparisons exceed max_pairs.")
      hits <- sf::st_intersects(sf::st_transform(points, polygon_crs), boundaries, model = "closed")
      edge <- sf::st_distance(points, sf::st_transform(sf::st_boundary(sf::st_geometry(boundaries)), 4326))
      hits <- lapply(seq_along(hits), function(i) union(hits[[i]], which(as.numeric(edge[i, ]) <= boundary_tolerance_m)))
      result$inside[ids] <- if (is.null(assignment)) lengths(hits) > 0L else
        vapply(seq_along(ids), function(i) { k <- data[[assignment]][ids[i]]
          if (is.na(k)) NA else k %in% hits[[i]] }, logical(1))
    }
  }
  if (!is.null(site)) for (s in unique(data[[site]][!missing(data[[site]])])) {
    ids <- which(good & !missing(data[[site]]) & data[[site]] == s)
    if (length(ids) < 2L || diff(range(x[ids])) > 180) next
    result$distance_m[ids] <- geographic_metres(x[ids], y[ids], stats::median(x[ids]), stats::median(y[ids]))
    result$distant[ids] <- result$distance_m[ids] >= distance_m
  }
  if (!is.null(day)) {
    sizes <- table(data[[day]][good]); if (sum(as.double(sizes)^2) > max_pairs) stop("Same-day comparisons exceed max_pairs.")
    for (d in unique(data[[day]][!is.na(data[[day]])])) {
      ids <- which(good & !is.na(data[[day]]) & data[[day]] == d)
      for (i in ids) result$neighbours[i] <- sum(geographic_metres(x[i], y[i], x[ids], y[ids]) <= cluster_m)
      result$clustered[ids] <- result$neighbours[ids] >= cluster_n
    }
  }
  list(status = if (nrow(data)) "assessed" else "not_applicable", results = result,
       settings = list(crs = crs, axis = axis, boundary_crs = polygon_crs, boundary_source = boundary_source,
         distance_m = distance_m, cluster_m = cluster_m, cluster_n = cluster_n, max_pairs = max_pairs, boundary_tolerance_m = boundary_tolerance_m))
}
geographic_metres <- function(x, y, x0, y0) {
  rad <- pi / 180; a <- sin((y - y0) * rad / 2)^2 + cos(y * rad) * cos(y0 * rad) * sin((x - x0) * rad / 2)^2
  2 * 6371008.8 * asin(sqrt(pmin(1, pmax(0, a))))
}
