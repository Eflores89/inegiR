denue_business_fixture <- function() {
  data.frame(
    id = c("1", "2"), name = c("Shop", "Office"),
    legal_name = c("", "Office SA"), activity = c("Retail", "Services"),
    economic_class = c("0 a 5 personas", "11 a 30 personas"),
    roads = c("CALLE", "AVENIDA"), street = c("Uno", "Dos"),
    exterior_num = c("1", "2"), interior_num = c("", ""),
    colonia = c("Centro", "Centro"), postal_code = c("64000", "64000"),
    location = c("Monterrey", "Monterrey"), tel = c("", "123"),
    email = c("", "office@example.org"), website = c("", "example.org"),
    type = c("Fijo", "Semifijo"), longitud = c("-100", "-100"),
    latitud = c("25", "25"), stringsAsFactors = FALSE
  )
}

test_that("DENUE grid accepts doubles, integers and multiclass numeric corners", {
  local_mocked_bindings(
    make_grid = function(lat1, lat2, lon1, lon2, space_lat, space_lon) {
      data.frame(latitud = c(lat1, lat2), longitud = c(lon1, lon2))
    },
    inegi_denue = function(latitud, longitud, token, meters, keyword) {
      denue_business_fixture()
    }
  )
  corners <- list(lat1 = 25, lat2 = 26, lon1 = -100, lon2 = -101)
  expect_identical(do.call(inegi_denue_grid, c(corners, list(token = "synthetic-token"))),
    denue_business_fixture())
  expect_equal(nrow(do.call(inegi_denue_grid,
    c(corners, list(token = "synthetic-token", uniqueonly = FALSE)))), 4L)

  for (corner in names(corners)) {
    for (value in list(as.integer(corners[[corner]]),
      structure(corners[[corner]], class = c("coordinate", "numeric")))) {
      input <- corners
      input[[corner]] <- value
      result <- NULL
      expect_no_error(result <- do.call(inegi_denue_grid,
        c(input, list(token = "synthetic-token"))))
      expect_identical(result, denue_business_fixture(),
        info = paste(corner, paste(class(value), collapse = "/")))
    }
  }
})

test_that("DENUE stats accept numeric coordinate vectors and compute statistics", {
  local_mocked_bindings(inegi_denue = function(latitud, longitud, token, meters, keyword) {
    denue_business_fixture()
  })
  coordinates <- list(latitud_vector = c(25, 26), longitud_vector = c(-100, -101))
  expected <- data.frame(businesses = c(2, 2), unique_streets = c(2, 2),
    unique_activities = c(2, 2), are_fixed = c(1, 1), have_legalname = c(1, 1),
    have_phone = c(1, 1), have_website = c(1, 1), in_avenue = c(1, 1),
    estimated_employees = c(23, 23), employee_stdev = rep(17 / sqrt(2), 2),
    businesses_per_street = c(1, 1), businesses_per_meter = c(0.008, 0.008),
    employees_per_business = c(11.5, 11.5), employees_per_meter = c(0.092, 0.092))
  expect_equal(do.call(inegi_denue_stats, c(coordinates, list(token = "synthetic-token"))),
    expected)

  for (coordinate in names(coordinates)) {
    for (value in list(as.integer(coordinates[[coordinate]]),
      structure(coordinates[[coordinate]], class = c("coordinate", "numeric")))) {
      input <- coordinates
      input[[coordinate]] <- value
      result <- NULL
      expect_no_error(result <- do.call(inegi_denue_stats,
        c(input, list(token = "synthetic-token"))))
      expect_equal(result, expected,
        info = paste(coordinate, paste(class(value), collapse = "/")))
    }
  }
})

test_that("DENUE coordinate errors are quiet and omit the token-bearing call", {
  local_mocked_bindings(
    make_grid = function(...) stop("grid must not run"),
    inegi_denue = function(...) stop("network must not run")
  )
  cases <- list(
    list(fun = inegi_denue_grid,
      args = list(lat1 = 25, lat2 = 26, lon1 = -100, lon2 = -101)),
    list(fun = inegi_denue_stats,
      args = list(latitud_vector = c(25, 26), longitud_vector = c(-100, -101)))
  )
  for (case in cases) {
    for (coordinate in names(case$args)) {
      input <- case$args
      input[[coordinate]] <- as.character(input[[coordinate]])
      literal_call <- as.call(c(list(case$fun), input,
        list(token = "synthetic-literal-denue-token")))
      output <- capture.output(error <- tryCatch(eval(literal_call), error = identity))
      expect_s3_class(error, "error")
      expect_match(conditionMessage(error), "numeric")
      expect_length(output, 0L)
      expect_null(conditionCall(error))
      expect_false(grepl("synthetic-literal-denue-token",
        paste(capture.output(dput(unclass(error))), collapse = "\n"), fixed = TRUE))
    }
  }
})
