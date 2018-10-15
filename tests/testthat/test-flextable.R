context("Flextable")

test_that("Font size", {
  x <- tail(iris, 10)[,c(1,5)]
  expect_equivalent(
    phl_calc_fontsize(x, 5),
    c(24, 0.4545455),
    tolerance = 0.0001)
})

test_that("phl_adjust_table - some tests", {
  irs <- head(iris, 10)
  lay <- lay_bind_row(
    lay_new(1),
    lay_new(cbind(1,2)),
    heights = c(3, 2)
  )
  olay <- phl_layout(lay)
  
  expect_warning(phl_adjust_table(irs, olay, 3, method = "height"))
  
  tbl <- phl_adjust_table(irs, olay, 3)
  fs  <- phl_calc_fontsize(irs, height = 2.750)
  expect_true(tbl$header$rowheights < fs[["height"]])
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

test_phl_with_flextable_large_tables_warns <- function(seed = 123) {
  
  lay <- lay_bind_col(
    lay_new(rbind(1,2)),
    lay_new(rbind(1,2))
  )
  
  olay <- phl_layout(lay)
  
  set.seed(seed)
  
  pptx <- officer::read_pptx()
  pptx <- officer::add_slide(
    pptx,
    layout = "Two Content", master = "Office Theme")
  
  irs <- iris[sample.int(nrow(iris), 10), ]
  
  tbl1 <- phl_adjust_table(irs, olay, 1)
  tbl2 <- phl_adjust_table(irs, olay, 2, method = "height")
  tbl3 <- phl_adjust_table(irs, olay, 3)
  tbl4 <- flextable::flextable(irs)
  tbl4 <- flextable::height_all(tbl4, tbl3$header$rowheights * 1.5)
  tbl4 <- flextable::width(tbl4, width = tbl4$body$colwidths * 1.5)
  
  
  phl_with_flextable(pptx, olay, 1, tbl1)
  phl_with_flextable(pptx, olay, 2, tbl2)
  phl_with_flextable(pptx, olay, 3, tbl3)
  phl_with_flextable(pptx, olay, 4, tbl4)
  pptx
}

test_that("phl_with_flextable - warns - compare with standard", {
  
  testthat::skip_on_cran()
  # pptx is identical with the standard
  expect_warning(
    expect_pptx_identical(
      test_phl_with_flextable_large_tables_warns,
      expected = "test_phl_with_flextable_large_tables_warns.pptx")
  )
  
  # pptx created with different seed should not be equal
  expect_warning(expect_false(
    pptx_testcase(
      test_phl_with_flextable_large_tables_warns,
      "test_phl_with_flextable_large_tables_warns.pptx",
      seed = 125)
  ))
  
})
