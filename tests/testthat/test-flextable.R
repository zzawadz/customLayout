context("Custom layout")

test_that("Font size", {
  x <- tail(iris, 10)[,c(1,5)]
  expect_equivalent(
    phl_calc_fontsize(x, 5),
    c(24, 0.4545455),
    tolerance = 0.0001)
})

context("Compare flextable with standard")

test_phl_with_flextable <- function(seed = 123) {
  lay <- lay_bind_row(
    lay_new(1),
    lay_new(cbind(1,2)),
    heights = c(3, 2)
  )
  olay <- phl_layout(lay)
  
  set.seed(seed)
  
  pptx <- officer::read_pptx()
  pptx <- officer::add_slide(
    pptx,
    layout = "Two Content", master = "Office Theme")
  
  data("diamonds", package = "ggplot2")
  diamonds2 <- diamonds[sample.int(nrow(diamonds), 100), ]
  
  tbl1 <- dplyr::group_by(diamonds2, cut)
  tbl1 <- dplyr::summarise(tbl1, Price = mean(price))
  
  t1 <- phl_adjust_table(tbl1, olay, 1)
  t2 <- phl_adjust_table(tbl1, olay, 2)
  t3 <- phl_adjust_table(tbl1, olay, 3)
  
  phl_with_flextable(pptx, olay, 1, t1)
  phl_with_flextable(pptx, olay, 2, t2)
  phl_with_flextable(pptx, olay, 3, t3)
  pptx
}

test_that("phl_with_flextable - compare with standard", {
  
  testthat::skip_on_cran()
  # pptx is identical with the standard
  expect_pptx_identical(
    test_phl_with_flextable,
    expected = "phl_with_flextable.pptx")
  
  # pptx created with different seed should not be equal
  expect_false(
    pptx_testcase(
      test_phl_with_flextable,
      "phl_with_flextable.pptx",
      seed = 125)
  )
  
})
