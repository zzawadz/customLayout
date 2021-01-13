context("Flextable")

test_that("Font size", {
  x <- tail(iris, 10)[,c(1,5)]
  expect_equivalent(
    phl_calc_fontsize(x, 5),
    c(24, 0.4545455),
    tolerance = 0.0001)
})

test_that("phl_adjust_table - some tests", {
  testthat::skip_if_not(
    Sys.info()["sysname"] == "Linux",
    message = "This can only pass on linux"
  )
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
