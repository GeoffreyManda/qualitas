test_that("record reconciliation uses typed tuples and retains row grain", {
  a <- data.frame(k1 = c("a|b", "a", "gone"), k2 = c("c", "b|c", "x"), v = c(1L, 2L, 3L))
  b <- a[c(2, 1), ]; b$v[1] <- 9L
  b <- rbind(b, data.frame(k1 = "added", k2 = "x", v = 0L))
  x <- compare_records(a, b, c("k1", "k2"))
  expect_identical(x$status, "assessed")
  expect_equal(x$matched, 2L); expect_equal(x$cells_compared, 2L)
  expect_equal(x$added, 3L); expect_equal(x$removed, 3L)
  expect_equal(x$changes$old_row, 2L); expect_equal(x$changes$new_row, 1L)
  expect_identical(x$changes$column, "v")
  expect_false(any(c("old_value", "new_value", "k1", "k2") %in% names(x$changes)))
  expect_identical(compare_records(a, a, c("k1", "k2"))$changes$column, character())
  b$v <- as.double(b$v)
  expect_identical(compare_records(a, b, c("k1", "k2"))$status, "blocked")
})

test_that("key defects precede matching, including blanks after normalisation", {
  a <- data.frame(id = c("a", "a", NA, " "), v = 1:4)
  x <- compare_records(a, a, "id")
  expect_identical(x$status, "blocked"); expect_equal(x$matched, 0L)
  expect_true(all(c("incomplete", "duplicate") %in% x$key_defects$issue))
  expect_equal(sum(x$key_defects$issue == "incomplete"), 4L)
  a <- data.frame(id = c("A", "a"), v = 1:2)
  expect_identical(compare_records(a, a, "id", list(id = tolower))$status, "blocked")
  b <- a; b$id <- 1:2
  expect_identical(compare_records(a, b, "id")$schema$incompatible, "id")
  expect_error(compare_records(a, a, "id", list(id = function(x) rep(1L, length(x)))), "type or length")
  expect_error(compare_records(a, a, "absent"), "existing")
  expect_error(compare_records(a, a, "id", sensitive = NA), "logical")
})

test_that("normalisation is explicit and authorized evidence never hides changes", {
  a <- data.frame(id = 1:2, value = c(" Yes ", NA))
  b <- data.frame(id = 1:2, value = c("Yes", "filled"))
  evidence <- data.frame(old_row = 2L, new_row = 2L, column = "value", evidence = "ticket-7")
  x <- compare_records(a, b, "id", list(value = trimws), evidence)
  expect_equal(nrow(x$changes), 1L); expect_true(x$changes$authorized)
  expect_identical(x$changes$evidence, "ticket-7"); expect_identical(x$evidence, evidence)
  expect_false("old_value" %in% names(x$changes))
  y <- compare_records(a, b, "id", sensitive = TRUE)
  expect_identical(y$changes$old_value[[1]], " Yes ")
  expect_equal(nrow(compare_records(a, b, "id")$changes), 2L)
  evidence$evidence <- " "
  expect_error(compare_records(a, b, "id", authorized_changes = evidence), "Incomplete")
  evidence$evidence <- "ticket-7"; evidence$old_row <- "2"
  expect_error(compare_records(a, b, "id", authorized_changes = evidence), "schema")
  expect_identical(compare_records(a[0, ], a[0, ], "id")$status, "not_applicable")
  b$extra <- "x"; expect_identical(compare_records(a, b, "id")$schema$added, "extra")
  a$value <- factor(a$value); b$value <- factor(b$value)
  expect_identical(compare_records(a, b, "id")$status, "blocked")
})

test_that("dependencies respect tuples, incomplete records and exact names", {
  d <- data.frame(a = c("x|y", "x", "x|y", "x", NA, "solo"),
    b = c("z", "y|z", "z", "y|z", "q", "s"), district = c("N", "S", "N", "T", "X", NA))
  x <- assess_dependencies(d, c("a", "b"), "district")
  expect_equal(x$excluded, 2L); expect_equal(nrow(x$results), 2L)
  expect_equal(sum(x$results$flagged), 1L)
  expect_identical(x$status, "assessed")
  d$district <- "N"
  expect_false(any(assess_dependencies(d, c("a", "b"), "district")$results$flagged, na.rm = TRUE))
  expect_identical(assess_dependencies(d[0, ], c("a", "b"), "district")$status, "not_applicable")
  d$district <- NA_character_
  expect_identical(assess_dependencies(d, c("a", "b"), "district")$status, "not_applicable")
})

test_that("categorical comparisons expose both denominators and sparse nonapplicability", {
  d <- data.frame(g = rep(c("one", "two"), c(30, 40)),
    v = c(rep(c("Y", "N"), c(24, 6)), rep(c("Y", "N"), c(20, 20))))
  x <- assess_distributions(d, "g", "v")
  expect_true(all(x$results$flagged)); expect_equal(x$results$difference, rep(0.3, 4))
  expect_equal(x$results$n_group, c(30, 30, 40, 40))
  expect_equal(x$results$n_rest, c(40, 40, 30, 30))
  expect_true(all(x$results$threshold == 0.3)); expect_true(all(x$results$tolerance == 1e-12))
  expect_false(any(assess_distributions(d, "g", "v", difference = 0.31)$results$flagged))
  sparse <- assess_distributions(d[-1, ], "g", "v")
  expect_identical(sparse$status, "not_applicable"); expect_true(all(is.na(sparse$results$flagged)))
  expect_identical(assess_distributions(d[0, ], "g", "v")$status, "not_applicable")
  d$v <- NA_character_
  expect_identical(assess_distributions(d, "g", "v")$status, "not_applicable")
  d$v <- 1L; expect_error(assess_distributions(d, "g", "v"), "categorical")
  expect_error(assess_distributions(d, "g", "g"), "unique")
  d$v <- "Y"; expect_error(assess_distributions(d, "g", "v", min_n = 0), "Invalid")
  expect_error(assess_distributions(d, "g", "v", tolerance = 0.1), "Invalid")
})

geo_screen <- function(d, ...) assess_geography(d, "x", "y", crs = 4326, axis = "longitude_latitude", ...)
test_that("coordinate parsing distinguishes invalid and incomplete pairs with legitimate zero", {
  d <- data.frame(x = c("0", "bad", "1", NA, "181", "Inf", " 1e-2 "),
                  y = c("0", "1", NA, NA, "0", "0", "2"))
  x <- geo_screen(d)
  expect_identical(x$results$pair, c("valid", "invalid", "incomplete", "missing", "invalid", "invalid", "valid"))
  expect_true(all(is.na(x$results$inside))); expect_true(all(is.na(x$results$distant)))
  expect_identical(geo_screen(d[0, ])$status, "not_applicable")
  expect_error(assess_geography(d, "x", "y", 3857, "longitude_latitude"), "Declare CRS")
  expect_error(assess_geography(d, "x", "y", 4326, "latitude_longitude"), "Declare CRS")
  d$x <- factor(d$x); expect_error(geo_screen(d), "numeric or decimal")
})

test_that("distance and same-day screening use metres and configurable limits", {
  d <- data.frame(x = c(0, 0, 0.001), y = 0, site = "a", day = as.Date("2026-01-01"))
  x <- geo_screen(d, site = "site", day = "day", distance_m = 100, cluster_m = 1, cluster_n = 2L)
  expect_equal(x$results$distance_m, c(0, 0, 111.19508), tolerance = 1e-5)
  expect_identical(x$results$distant, c(FALSE, FALSE, TRUE))
  expect_identical(x$results$neighbours, c(2L, 2L, 1L))
  expect_identical(x$results$clustered, c(TRUE, TRUE, FALSE))
  y <- geo_screen(d, site = "site", day = "day", distance_m = 200, cluster_m = 200, cluster_n = 3L)
  expect_false(any(y$results$distant)); expect_true(all(y$results$clustered))
  d$day[3] <- d$day[3] + 1
  expect_equal(geo_screen(d, day = "day", cluster_m = 200)$results$neighbours, c(2L, 2L, 1L))
  expect_error(geo_screen(d, day = "day", max_pairs = 1), "max_pairs")
  d$day <- as.character(d$day); expect_error(geo_screen(d, day = "day"), "Date")
  d$site <- NA_character_
  expect_true(all(is.na(geo_screen(d, site = "site")$results$distant)))
  expect_error(geo_screen(d, distance_m = -1), "Invalid")
  d$x <- c(-179, 179, 179)
  d$site <- "a"
  expect_true(all(is.na(geo_screen(d, site = "site")$results$distant)))
})

test_that("polygon membership includes edges, holes, assignments and projected CRS", {
  skip_if_not_installed("sf")
  outer <- matrix(c(-2,-2, 2,-2, 2,2, -2,2, -2,-2), ncol = 2, byrow = TRUE)
  hole <- matrix(c(-0.5,-0.5, -0.5,0.5, 0.5,0.5, 0.5,-0.5, -0.5,-0.5), ncol = 2, byrow = TRUE)
  polygon <- sf::st_sfc(sf::st_polygon(list(outer, hole)), crs = 4326)
  # Planar projection makes the independently specified square edges unambiguous.
  polygon <- sf::st_transform(polygon, 3857)
  d <- data.frame(x = c(1, 0, 2, 3, NA), y = 0)
  x <- geo_screen(d, boundaries = polygon, boundary_source = "synthetic-square-v1")
  expect_identical(x$results$inside, c(TRUE, FALSE, TRUE, FALSE, NA))
  expect_equal(x$settings$boundary_crs$epsg, 3857)
  expect_identical(x$settings$boundary_source, "synthetic-square-v1")
  d$area <- c(1L, 1L, NA_integer_, 1L, 1L)
  expect_true(is.na(geo_screen(d, boundaries = polygon, assignment = "area", boundary_source = "v1")$results$inside[3]))
  d$area[1] <- 2L
  expect_error(geo_screen(d, boundaries = polygon, assignment = "area", boundary_source = "v1"), "indices")
  expect_error(geo_screen(d, boundaries = polygon), "source/version")
  expect_error(geo_screen(d, boundaries = sf::st_set_crs(polygon, NA), boundary_source = "v1"), "known CRS")
  expect_error(geo_screen(d, boundaries = sf::st_sfc(sf::st_point(c(0,0)), crs = 4326), boundary_source = "v1"), "polygons")
  expect_error(geo_screen(d, assignment = "area"), "needs boundaries")
})


test_that("categorical exclusions, factor blanks and resource limits stay explicit", {
  d <- data.frame(g = factor(c("a", "a", "b", "b", NA, "a")),
                  v = factor(c("yes", "yes", "no", "no", "no", " ")))
  x <- assess_distributions(d, "g", "v", min_n = 2)
  expect_equal(x$excluded, 2L)
  expect_equal(x$results$n_group, rep(2L, 4))
  expect_true(all(x$results$flagged))
  expect_error(assess_distributions(d, "g", "v", max_cells = 3), "max_cells")
  one <- d[1:2, ]
  expect_identical(assess_distributions(one, "g", "v", min_n = 1)$status, "not_applicable")
  expect_true(all(is.na(assess_distributions(one, "g", "v", min_n = 1)$results$flagged)))
})

test_that("schema, empty deliveries and time-zone changes cannot silently agree", {
  a <- data.frame(id = 1L, value = as.POSIXct("2026-01-01", tz = "UTC"))
  b <- a; attr(b$value, "tzone") <- "Europe/London"
  expect_identical(compare_records(a, b, "id")$status, "blocked")
  expect_equal(compare_records(a[0, ], a, "id")$added, 1L)
  expect_equal(compare_records(a, a[0, ], "id")$removed, 1L)
  b <- a[c("id")]
  expect_identical(compare_records(a, b, "id")$schema$removed, "value")
  names(b) <- "value"
  expect_error(compare_records(a, b, "id"), "existing")
})

test_that("geographic polygon edges use closed membership in WGS84", {
  skip_if_not_installed("sf")
  polygon <- sf::st_sfc(sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0),
      ncol = 2, byrow = TRUE))), crs = 4326)
  d <- data.frame(x = c(0, 0.5, 2), y = c(0, 0, 0))
  expect_identical(geo_screen(d, boundaries = polygon, boundary_source = "synthetic-v1")$results$inside,
                   c(TRUE, TRUE, FALSE))
  expect_true(all(is.na(geo_screen(d, boundaries = polygon, boundary_source = "synthetic-v1")$results$clustered)))
})


test_that("polygon tolerance and resource limits are explicit", {
  skip_if_not_installed("sf")
  polygon <- sf::st_sfc(sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0),
      ncol = 2, byrow = TRUE))), crs = 4326)
  d <- data.frame(x = -0.00001, y = 0.5)
  expect_false(geo_screen(d, boundaries = polygon, boundary_source = "v1")$results$inside)
  near <- geo_screen(d, boundaries = polygon, boundary_source = "v1", boundary_tolerance_m = 2)
  expect_true(near$results$inside)
  expect_equal(near$settings$boundary_tolerance_m, 2)
  expect_error(geo_screen(d, boundaries = polygon, boundary_source = "v1", max_pairs = 0), "max_pairs")
})

test_that("rule results must be logical vectors with exactly one entry per record", {
  d <- data.frame(x = c(1, NA, -1))
  for (expr in c("logical(0)", "1:3", "c(TRUE, TRUE)", "TRUE", "NA",
                 "list(TRUE, TRUE, TRUE)", "matrix(TRUE, 3, 1)")) {
    out <- assess(d, spec = infer_spec(d), rules = c(r = expr), checks = "CS01")
    expect_identical(out$measures$status, "error")
    expect_true(is.na(out$measures$score))
    expect_match(out$measures$error, "logical vector.*one value per record")
  }
  out <- assess(d, spec = infer_spec(d), rules = c(r = "c(TRUE, TRUE, TRUE)"), checks = "CS01")
  expect_identical(out$measures$status, "passed")
})

test_that("unresolved rules retain known false findings without a passing score", {
  d <- data.frame(x = c(1, NA, -1))
  out <- assess(d, spec = infer_spec(d), rules = c(r = "x > 0"), checks = "CS01")
  expect_identical(out$measures$status, "error")
  expect_true(is.na(out$measures$score))
  expect_match(out$measures$error, "unresolved NA")
  expect_identical(out$issues$row, 3L)
  expect_equal(out$measures$flagged, 1L)
  out <- assess(d, spec = infer_spec(d), rules = c(r = "rep(NA, 3)"), checks = "CS01")
  expect_identical(out$measures$status, "error")
  expect_true(is.na(out$measures$score))
  expect_match(out$measures$error, "unresolved NA")
  empty <- d[0, , drop = FALSE]
  out <- assess(empty, spec = infer_spec(empty), rules = c(r = "logical(0)"), checks = "CS01")
  expect_identical(out$measures$status, "not applicable")
  expect_true(is.na(out$measures$error)); expect_true(is.na(out$measures$score))
  expect_equal(out$measures$checked, 0L)
})

test_that("duplicate metadata names cannot hide a contradictory manifest hash", {
  root <- tempfile('qualitas-delivery-')
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  writeLines("synthetic", file.path(root, "data.csv"))
  h <- digest::digest(file = file.path(root, "data.csv"), algo = "sha256")
  m <- data.frame(path = "data.csv", sha256 = h, sender = "sender",
    sent_at = "2026-11-25T10:00:00Z", version = "1", role = "data")
  expect_equal(nrow(assess_delivery(m, root, required_roles = "data")), 0L)
  m$contradictory <- strrep("0", 64); names(m)[7] <- "sha256"
  expect_error(assess_delivery(m, root, required_roles = "data"), "column names must be unique")
  expect_error(required_metadata(m[0, ], "sha256"), "column names must be unique")
})
