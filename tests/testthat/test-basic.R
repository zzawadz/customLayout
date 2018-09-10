library(testthat)

test_that("Combine two layouts", {
  
  l1 <- layCreate(matrix(c(1:2),ncol = 2),widths=c(4,1))
  l2 <- layCreate(matrix(c(1:4),ncol = 2),widths=c(1,1))
  lb <- layColBind(l1,l2, widths = c(2,1))
  
  expect_equal(lb@widths, c(16, 4, 5, 5))
  expect_equal(sum(lb@widths[1:2]) / sum(lb@widths[3:4]), 2)
})

test_that("Check erros in layCreate", {
  expect_error(layCreate(matrix(c(1:2),ncol = 2),widths = c(4,1,1)))
  expect_error(layCreate(matrix(c(1:2),ncol = 2),heights = c(4,1)))
})

make_basic_test <- function(fnc) {
  function() {
    l1 <- layCreate(
      matrix(c(1:4), ncol = 2),
      widths = c(4, 1))
    l2 <- layCreate(
      matrix(c(1:4), ncol = 2),
      widths = c(1, 1), heights = c(3,1))
    l3 <- fnc(l1, l2)
    layShow(l3)
  }
}

test_that("Basic bind", {
  vdiffr::expect_doppelganger(
    "basic bind col",
    make_basic_test(layColBind)
  )
  
  vdiffr::expect_doppelganger(
    "basic bind row",
    make_basic_test(layRowBind)
  )
})

if(require(ggplot2)) {
  
  library(ggplot2)
  
  basic_grid_layout <- function() {
    l1 <- layCreate(matrix(1:2, ncol = 1), heights = c(2, 3))
    l2 <- layCreate(matrix(1:2, ncol = 1), heights = c(1, 3))
    l3 <- layColBind(l1, l2)

    pl1 <- qplot(mpg, wt, data = mtcars)
    pl2 <- qplot(mpg, gear, data = mtcars)
    pl3 <- qplot(cyl, gear, data = mtcars)
    pl4 <- qplot(qsec, am, data = mtcars)

    layGrid(list(pl1, pl2, pl3, pl4), l3)
  }
  
  test_that("Basic grid", {
    vdiffr::expect_doppelganger(
      "basic grid layout",
      basic_grid_layout
    )
  })
  
}
