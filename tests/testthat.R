library(testthat)
library(customLayout)
library(vdiffr)

on_appveyor <- function() {
  identical(Sys.getenv("APPVEYOR"), "True")
}
on_cran <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true")
}

# Use minimal fonts.conf to speed up fc-cache
if (on_appveyor() || on_cran()) {
  gdtools::set_dummy_conf()
}


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
  
  pptx <- phl_with_flextable(pptx, olay, 1, t1)
  pptx <- phl_with_flextable(pptx, olay, 2, t2)
  pptx <- phl_with_flextable(pptx, olay, 3, t3)
  pptx
}


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
  
  pptx <- phl_with_table(pptx, olay, 1, head(irs,3))
  pptx <- phl_with_table(pptx, olay, 2, head(irs,4))
  pptx <- phl_with_table(pptx, olay, 3, head(irs[,c(1,5)],3))
  pptx <- phl_with_table(pptx, olay, 4, head(irs[,c(1,5)],4))
  pptx
}

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

# print(test_phl_with_flextable(), "tests/pptx/phl_with_flextable.pptx")
# print(test_phl_with_flextable_large_tables_warns(), "tests/pptx/test_phl_with_flextable_large_tables_warns.pptx")
# print(test_phl_with_gg(), "tests/pptx/phl_with_gg.pptx")
# print(test_phl_with_table(), "tests/pptx/test_phl_with_table.pptx")
# print(test_phl_with_vg(), "tests/pptx/phl_with_vg.pptx")

test_check("customLayout")
