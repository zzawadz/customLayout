context("Split field")

disp_basic_split_field <- function() {
  l1 <- layCreate(matrix(c(1:4), ncol = 2), widths = c(4, 1))
  l2 <- layCreate(matrix(c(1:4), ncol = 2), widths = c(1, 1))
  l3 <- laySplitField(l1, l2, 2)
  stop("vardiff test")
  layShow(l3)
}

test_that("Basic split field", {
  vdiffr::expect_doppelganger(
    "basic split field",
    disp_basic_split_field
  )
})

