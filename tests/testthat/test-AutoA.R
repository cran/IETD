context("test-AutoA")

test_that("Whether MaxLag mataches with the maximun leg time considered in the autocorrelogram", {
  Time_series=five_minute_time_series
  MaxLag=30
  df<-AutoA(Time_series,MaxLag)$Values
  expect_equal(df[length(df[,1]),1],MaxLag)})
