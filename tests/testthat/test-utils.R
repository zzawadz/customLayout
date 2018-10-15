context("Test utility functions")

test_that("Print CustomLayout object", {
  lay <- lay_new(matrix(1:3))
  capture_output(print(lay))
})

test_that("Print OfficerCustomLayout object", {
  lay <- phl_layout(lay_new(matrix(1:3)))
  capture_output(print(lay))
})
