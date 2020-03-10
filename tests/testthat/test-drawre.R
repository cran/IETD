context("test-drawre")

test_that("Whether the number of events in the Rainfall_Characteristics file (dataframe) mataches with the ones included
           in the sublist Rainfall_Events", {
  Time_series=five_minute_time_series
  RC=drawre(Time_series,IETD=3,Thres=0.5)$Rainfall_Characteristics
  RE=drawre(Time_series,IETD=3,Thres=0.5)$Rainfall_Events
  dl=length(RC[,2])-length(RE)
  expect_equal(dl,0)})



test_that("Whether the dry periods between the extracted storms are longer than IETD", {
            Time_series=five_minute_time_series
            RC=drawre(Time_series,IETD=3,Thres=0.5)$Rainfall_Characteristics
            dryP<-min(RC$Starting[-1]-RC$End[-length(RC$End)])
            expect_gt(dryP,3)
           })
