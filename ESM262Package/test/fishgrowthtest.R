fishgrowthtest <- test_that("fish growth works", {

  expect_that(fish_growth(T = 1,
                          a = 1,
                          b = 1,
                          c = 1,
                          d = 1),
              equals(4))
})
