context("Reps functions")

make_rep_test_function <- function(fnc) {
  function() {
    l1 <- lay_new(
      matrix(1:4, ncol = 2),
      heights = c(1, 3))
    l2 <- fnc(l1, 3)
    lay_show(l2)
  }
}

test_that("rep by col", {
  vdiffr::expect_doppelganger(
    "rep by col",
    make_rep_test_function(customLayout:::layRepByCol)
  )
})

test_that("rep by row", {
  vdiffr::expect_doppelganger(
    "rep by row",
    make_rep_test_function(customLayout:::layRepByRow)
  )
})

test_that("mock layColRepGenerator", {
  customLayout:::layColRepGenerator("widths")
  customLayout:::layColRepGenerator("heights")
})
