context("Asserts")

test_that("Creating assert function works", {
  
  fnc <- customLayout:::make_assert_function("Tmp", "Tmp")
  
  x <- list(1)
  expect_error(fnc(x))
  class(x) <- "Tmp"
  expect_true(fnc(x))
  
})

test_that("id in layout", {
  
  lay <- lay_new(matrix(1:3))
  expect_true(customLayout:::assert_id_inlayout(1, lay))
  expect_error(customLayout:::assert_id_inlayout(0, lay))
  expect_error(customLayout:::assert_id_inlayout(-1, lay))
  expect_error(customLayout:::assert_id_inlayout(4, lay))
  
  # only single value is allowed
  expect_error(customLayout:::assert_id_inlayout(c(1:5), lay))
})

test_that("pptx_testcase fails if the file does not exists", {
  expect_error(
    pptx_testcase(fnc = function() {}, expected = "test.pptx")
  )
})

