test_that("make_wallpapr", {
  expect_equal({
    out <- make_wallpapr(
      system.file("extdata", "mull.jpg", package = "wallpapr"),
      month = as.Date("2021-03-01"),
      return_plot = TRUE
    )
    df <- out$data
    c(nrow(df), ncol(df), sum(df$week), sum(df$size),
      class(out), attr(out, "dims"))
  }, list(39L, 6L, 387, 40, "wallpapr", "gg", "ggplot", dpi = 900,
          width = 1613L, height = 907L))
})
