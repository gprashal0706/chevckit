library(lubridate)

test_that("cross validation works", {
  # check unordered dates
  dts <- ymd_hms("2019-10-01 00:00:00") + minutes(1)*c(1,3:400,2)
  expect_error(cv_ts_folds(dts, ymd("2019-10-02"), 1, 3),
               "Datetime values must be ordered.")
  
  # check two weeks of minutely-hourly data
  dts <- ymd("2019-10-01") + minutes(1)*0:(1440*14-1)
  cv_folds <- cv_ts_folds(dts, ymd("2019-10-07"), 2, 3, 1)
  expect_equal(dts[cv_folds[[1]][["train"]]],
               ymd("2019-10-01") + minutes(1)*0:(1440*6-1))
  expect_equal(dts[cv_folds[[1]][["test"]]],
               ymd("2019-10-07") + minutes(1)*0:(1440*2-1))
  expect_equal(dts[cv_folds[[2]][["train"]]],
               ymd("2019-10-01") + minutes(1)*0:(1440*7-1))
  expect_equal(dts[cv_folds[[2]][["test"]]],
               ymd("2019-10-08") + minutes(1)*0:(1440*2-1))
  expect_equal(dts[cv_folds[[3]][["train"]]],
               ymd("2019-10-01") + minutes(1)*0:(1440*8-1))
  expect_equal(dts[cv_folds[[3]][["test"]]],
               ymd("2019-10-09") + minutes(1)*0:(1440*2-1))
  
  # check two weeks of half-hourly data with two day jumps
  dts <- ymd("2019-10-01") + minutes(1)*0:(1440*14-1)
  cv_folds <- cv_ts_folds(dts, ymd("2019-01-07"), 2, 3, 2)
  expect_equal(dts[cv_folds[[1]][["train"]]],
               ymd("2019-10-01") + minutes(1)*0:(1440*6-1))
  expect_equal(dts[cv_folds[[1]][["test"]]],
               ymd("2019-10-07") + minutes(1)*0:(1440*2-1))
  expect_equal(dts[cv_folds[[2]][["train"]]],
               ymd("2019-10-01") + minutes(1)*0:(1440*8-1))
  expect_equal(dts[cv_folds[[2]][["test"]]],
               ymd("2019-10-09") + minutes(1)*0:(1440*2-1))
  expect_equal(dts[cv_folds[[3]][["train"]]],
               ymd("2019-10-01") + minutes(1)*0:(1440*10-1))
  expect_equal(dts[cv_folds[[3]][["test"]]],
               ymd("2019-10-11") + minutes(1)*0:(1440*2-1))
})
