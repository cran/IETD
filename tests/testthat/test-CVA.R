context("test-CVA")

test_that("Whether MaxIETD mataches with the maximun number of hours considered in the Figure of IETD vs CV", {
  Time_series=five_minute_time_series
  MaxIETD=30
  df<-CVA(Time_series,MaxIETD)$Values
  expect_equal(length(df[,2]),MaxIETD)})
