test_that("supported legacy dates, shortcuts, values and notes remain compatible", {
  annual <- legacy_fixture_data(fixture_series(c("2023", "2024/01"), "1"))
  expect_identical(annual$date, c("2023", "2024"))
  expect_identical(annual$date_shortcut, c("Year", "Year"))
  expect_identical(annual$values, c(100.25, 101.5))
  expect_identical(annual$notes, c("", ""))
  quarterly <- legacy_fixture_data(fixture_series(c("2024/01", "2024/04"), "6"))
  expect_identical(quarterly$date, as.Date(c("2024-01-01", "2024-10-01")))
  expect_identical(quarterly$date_shortcut, c("Q1", "Q4"))
  monthly <- legacy_fixture_data(fixture_series())
  expect_identical(monthly$date, as.Date(c("2024-01-01", "2024-02-01")))
  expect_identical(monthly$date_shortcut, c("M1", "M2"))
  expect_warning(biweekly <- legacy_fixture_data(
    fixture_series(c("2024/01", "2024/01"), "9")), "Biweekly")
  expect_identical(biweekly$date, as.Date(c("2024-01-01", "2024-01-01")))
  expect_identical(biweekly$date_shortcut, 1:2)
})

test_that("frequency 10 is not accidentally treated as annual", {
  expect_warning(result <- legacy_fixture_data(fixture_series(frequency = "10")),
                 "not supported")
  expect_identical(result$date, c("2024/01", "2024/02"))
})

test_that("unsupported legacy frequencies return raw periods with defined shortcuts", {
  for (frequency in c("4", "5", "7", "10", "11", "12", "13", "18", "99")) {
    fixture <- fixture_series(c("2024/01", "2024/02"), frequency)
    expect_warning(result <- legacy_fixture_data(fixture), "not supported")
    expect_identical(result$date, c("2024/01", "2024/02"), info = frequency)
    expect_identical(result$date_shortcut, c("2024/01", "2024/02"), info = frequency)
  }
})

test_that("canonical periods use validated interval starts and exact frequency codes", {
  annual <- inegi_period(c("2024", "2023/01"), "1")
  expect_identical(annual$period, c("2024", "2023/01"))
  expect_identical(annual$date, as.Date(c("2024-01-01", "2023-01-01")))
  expect_identical(annual$date_shortcut, c("Year", "Year"))
  expect_identical(inegi_period("2024", "2")$date, as.Date("2024-01-01"))
  expect_identical(inegi_period("2024", "3")$date, as.Date("2024-01-01"))
  quarterly <- inegi_period(c("2024/01", "2024/04"), "6")
  expect_identical(quarterly$date, as.Date(c("2024-01-01", "2024-10-01")))
  expect_identical(quarterly$date_shortcut, c("Q1", "Q4"))
  monthly <- inegi_period(c("2024/02", "2024/12"), "8")
  expect_identical(monthly$date, as.Date(c("2024-02-01", "2024-12-01")))
  expect_identical(monthly$date_shortcut, c("M2", "M12"))
  for (frequency in c("10", "18", "99", NA_character_)) {
    unknown <- inegi_period("2024/01", frequency)
    expect_identical(unknown$period, "2024/01")
    expect_identical(unknown$date, as.Date(NA_character_))
    expect_identical(unknown$date_shortcut, "2024/01")
  }
})

test_that("ambiguous biweekly periods stay undated and calendar mistakes are rejected", {
  biweekly <- inegi_period(c("2024/01", "2024/01"), "9")
  expect_true(all(is.na(biweekly$date)))
  expect_identical(biweekly$period, c("2024/01", "2024/01"))
  expect_identical(biweekly$date_shortcut, 1:2)
  for (case in list(list("1", c("0000", "2024/13", "2024abc")),
                    list("6", c("2024/00", "2024/05", "2024/1")),
                    list("8", c("2024/00", "2024/13", "2024/02/30", NA_character_)))) {
    expect_true(all(is.na(inegi_period(case[[2]], case[[1]])$date)))
  }
  empty <- inegi_period(character(), "8")
  expect_identical(nrow(empty), 0L)
  expect_s3_class(empty$date, "Date")
})

test_that("unsupported periods give an actionable tibbletime error", {
  expect_error(suppressWarnings(legacy_fixture_data(fixture_series(frequency = "7"),
                                                     as_tt = TRUE)),
               "as_tt.*Date|Date.*as_tt")
})
