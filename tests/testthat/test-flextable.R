context("Custom layout")

test_that("Font size", {
  x <- tail(iris, 10)[,c(1,5)]
  expect_equivalent(
    phl_calc_fontsize(x, 5),
    c(24, 0.4545455),
    tolerance = 0.0001)
})
