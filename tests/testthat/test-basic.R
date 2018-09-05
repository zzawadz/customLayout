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
