test_that("make calendar data", {
  expect_equal({
    df <- calender_data(as.Date("2021-03-01"))
    c(nrow(df), ncol(df), sum(df$week), sum(df$size))
  }, c(39, 6, 387, 40))
  expect_equal({
    df <- calender_data(as.Date("2021-03-01"),
                        start_monday = FALSE)
    c(nrow(df), ncol(df), sum(df$week), sum(df$size))
  }, c(39, 6, 391, 40))
  expect_equal({
    df <- calender_data(as.Date("2020-12-01"))
    c(nrow(df), ncol(df), sum(df$week), sum(df$size))
  }, c(39, 6, 1951, 40))
  expect_equal({
    df <- calender_data(as.Date("2021-01-01"))
    c(nrow(df), ncol(df), sum(df$week), sum(df$size))
  }, c(39, 6, 52, 40))
  expect_equal({
    df <- calender_data("March")
    c(nrow(df), ncol(df), sum(df$size))
  }, c(39, 6, 40))
  expect_equal({
    df <- calender_data(as.Date("2021-03-01"),
                        locale = "en")
    levels(df$day)
  }, c("Monday", "Tuesday", "Wednesday",
       "Thursday", "Friday", "Saturday",
       "Sunday"))
  expect_equal({
    df <- calender_data(as.Date("2021-03-01"),
                        start_monday = FALSE,
                        locale = "en")
    levels(df$day)
  }, c("Sunday", "Monday", "Tuesday",
       "Wednesday", "Thursday", "Friday",
       "Saturday"))
})

test_that("errors", {
  expect_error(
    calender_data("test"),
    "\"month\" was not given as a proper date or name of a month"
  )
})
