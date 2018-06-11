fishmarkettest <- test_that("fishery summary function works", {
  expect_that(length(fisheries_summary(prices = fish_price,
                                       catch_table = catch_table)[[1]]),
              equals(2))})
