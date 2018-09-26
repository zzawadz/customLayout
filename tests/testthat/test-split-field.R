context("Split field")

make_disp_basic_split_field <- function(n = 2) {
  function() {
    l1 <- lay_new(
      matrix(c(1:4), ncol = 2),
      widths = c(4, 1))
    l2 <- lay_new(
      matrix(c(1:4), ncol = 2),
      widths = c(1, 1))
    l3 <- lay_split_field(l1, l2, n)
    lay_show(l3)
  }
}

disp_basic_split_field_1 <- make_disp_basic_split_field(1)
disp_basic_split_field_2 <- make_disp_basic_split_field(2)
disp_basic_split_field_3 <- make_disp_basic_split_field(3)
disp_basic_split_field_4 <- make_disp_basic_split_field(4)

test_that("Basic split field", {
  vdiffr::expect_doppelganger(
    "basic split field 1",
    disp_basic_split_field_1
  )
  vdiffr::expect_doppelganger(
    "basic split field 2",
    disp_basic_split_field_2
  )
  vdiffr::expect_doppelganger(
    "basic split field 3",
    disp_basic_split_field_3
  )
  vdiffr::expect_doppelganger(
    "basic split field 4",
    disp_basic_split_field_4
  )
})
