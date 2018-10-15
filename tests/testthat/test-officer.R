context("Officer")

library(customLayout)
library(officer)
library(magrittr)
library(ggplot2)
library(officer)

lay <- lay_new(matrix(1:4,nc=2),widths=c(3,2),heights=c(2,1))
lay2 <- lay_new(matrix(1:3))
titleLay <- lay_new(1, widths = 1, heights = 1)

lay3 <- lay_bind_col(lay,lay2, widths=c(3,1))
layout <- lay_bind_row(titleLay, lay3, heights = c(1,7))
offLayout <- phl_layout(
  layout,
  margins = c(0.25, 0.25, 0.25, 0.25),
  innerMargins = rep(0.15,4))

test_make_pptx <- function() 
{
  pptx <- read_pptx()
  pptx <- add_slide(
    pptx, 
    master = "Office Theme",
    layout = "Title and Content")
  pptx
}

test_get_slide_summary <- function(pptx) {
  unlist(slide_summary(pptx)[,c("offx", "offy", "cx", "cy")])
}

test_that("phl_with_gg works", {
  pptx <- test_make_pptx()
  plot1 <- qplot(mpg, wt, data = mtcars)
  pptx <- phl_with_gg(pptx, offLayout, 2, plot1)
  
  vals <- test_get_slide_summary(pptx)
  expect_equivalent(offLayout[[2]], vals) 
})

test_that("phl_with_plot works", {
  pptx <- test_make_pptx()
  pl7 <- function() {
    par(mar = rep(0.1, 4))
    pie(c(5, 4, 2), col = 2:4 + 6)
  }
  
  pptx <- phl_with_plot(pptx, offLayout, 7, pl7)
  
  vals <- test_get_slide_summary(pptx)
  expect_equivalent(offLayout[[7]], vals) 
})

test_that("phl_with_text works", {
  pptx <- test_make_pptx()
  pptx <- phl_with_text(pptx, offLayout, 1, "Custom Layout")
  
  vals <- test_get_slide_summary(pptx)
  expect_equivalent(offLayout[[1]], vals) 
})

########## Compare pptx ###########
context("Compare pptx files")

test_phl_with_vg <- function(seed = 123) {
  lay <- lay_bind_row(
    lay_new(1),
    lay_new(cbind(1,2)),
    heights = c(3, 2)
  )
  olay <- phl_layout(lay)
  
  set.seed(seed)
  
  pptx <- officer::read_pptx()
  pptx <- officer::add_slide(pptx,
    layout = "Two Content", master = "Office Theme")
  
  phl_with_vg(pptx, olay, 1, code = {
    plot(rnorm(10), rnorm(10))
    title("Title")
  })
  
  data("diamonds", package = "ggplot2")
  diamonds2 <- diamonds[sample.int(nrow(diamonds), 100), ]
  gg1 <- ggplot2::ggplot(diamonds2) + 
    ggplot2::geom_point(aes(carat, price))
  gg2 <- ggplot2::ggplot(diamonds2) + 
    ggplot2::geom_point(aes(depth, price))
  
  phl_with_vg(pptx, olay, 2, ggobj = gg1)
  phl_with_vg(pptx, olay, 3, ggobj = gg2)
  pptx
}

test_that("phl_with_vg - compare with standard", {
  
  testthat::skip_on_cran()
  # pptx is identical with standard
  expect_pptx_identical(
    test_phl_with_vg,
    expected = "phl_with_vg.pptx")
  
  # pptx created with different seed should not be equal
  expect_false(
    pptx_testcase(
      test_phl_with_vg,
      "phl_with_vg.pptx",
      seed = 125)
  )
  
})

test_phl_with_gg <- function(seed = 123) {
  lay <- lay_bind_row(
    lay_new(1),
    lay_new(cbind(1,2)),
    heights = c(3, 2)
  )
  olay <- phl_layout(lay)
  
  set.seed(seed)
  
  pptx <- officer::read_pptx()
  pptx <- officer::add_slide(pptx,
                             layout = "Two Content", master = "Office Theme")
  
  data("diamonds", package = "ggplot2")
  diamonds2 <- diamonds[sample.int(nrow(diamonds), 100), ]
  
  gg <- ggplot2::ggplot(diamonds2) + 
    ggplot2::geom_point(aes(x, price))
  
  phl_with_gg(pptx, olay, 1, gg)
  
  gg1 <- ggplot2::ggplot(diamonds2) + 
    ggplot2::geom_point(aes(carat, price))
  gg2 <- ggplot2::ggplot(diamonds2) + 
    ggplot2::geom_point(aes(depth, price))
  
  phl_with_gg(pptx, olay, 2, gg1)
  phl_with_gg(pptx, olay, 3, gg2)
  pptx
}

test_that("phl_with_gg - compare with standard", {
  
  testthat::skip_on_cran()
  # pptx is identical with standard
  expect_pptx_identical(
    test_phl_with_gg,
    expected = "phl_with_gg.pptx")
  
  # pptx created with different seed should not be equal
  expect_false(
    pptx_testcase(
      test_phl_with_gg,
      "phl_with_gg.pptx",
      seed = 125)
  )
  
})

test_phl_with_table <- function(seed = 123) {
  
  lay <- lay_bind_col(
    lay_new(rbind(1,2)),
    lay_new(rbind(1,2)), 
    widths = c(3,4)
  )
  
  olay <- phl_layout(lay)
  
  set.seed(seed)
  
  pptx <- officer::read_pptx()
  pptx <- officer::add_slide(
    pptx,
    layout = "Two Content", master = "Office Theme")
  
  irs <- iris[sample.int(nrow(iris), 10), ]
  
  phl_with_table(pptx, olay, 1, head(irs,3))
  phl_with_table(pptx, olay, 2, head(irs,4))
  phl_with_table(pptx, olay, 3, head(irs[,c(1,5)],3))
  phl_with_table(pptx, olay, 4, head(irs[,c(1,5)],4))
  pptx
}

test_that("phl_with_table - compare with standard", {
  
  testthat::skip_on_cran()
  # pptx is identical with the standard
  expect_pptx_identical(
    test_phl_with_table,
    expected = "test_phl_with_table.pptx")

  # pptx created with different seed should not be equal
  expect_false(
    pptx_testcase(
      test_phl_with_table,
      "test_phl_with_table.pptx",
      seed = 125)
  )
  
})

