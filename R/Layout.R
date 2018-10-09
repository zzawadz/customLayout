#' Print a CustomLayout object.
#'
#' @param x object of class CustomLayout.
#' @param ... optional arguments to print or plot methods. Not used here.
#'
#' @export
#' 
#' @seealso lay_new lay_show
#' 
#' @examples
#' 
#' lay  <- lay_new(matrix(1:4,nc=2),widths=c(3,2),heights=c(2,1))
#' lay2 <- lay_new(matrix(1:3))
#' cl <- lay_bind_col(lay,lay2, widths=c(3,1))
#' print(cl)
#' 
#' cl2 <- lay_bind_col(cl,cl, c(2,1))
#' print(cl2)
#' 
#' cl3 <- lay_bind_row(cl,cl, c(20,1))
#' print(cl3) 
#' 
print.CustomLayout <- function(x, ...) {
  
  xname <- deparse(substitute(x))
  
  cat("CustomLayout object:\n")
  cat("  Specification:\n")
  
  mat <- apply(x$mat, 1, function(x) {
    paste(sprintf("% 3i", x), collapse = "  ")
  })
  
  mat <- paste("  ", sprintf("%3i", x$heights), mat)
  head <- paste("      ", 
      paste(sprintf("%3i", x$widths), collapse = "  "))
  cat(c(head, mat), sep = "\n")
  
  cat(
    "\n\n  To print the layout on your graphics device plese use:",
    sprintf("  lay(%s)", xname), sep = "\n"
  )
  
  invisible(x)
}

#' Create custom layout.
#' 
#' @param mat a matrix specifying the location of the figures. See \code{\link{layout}} for more information.
#' @param widths a vector of values for the relative heights of rows in mat.
#' @param heights a vector of values for the relative heights of rows in mat.
#' 
#' @export
#' @rdname lay_new
#' @examples
#' library(customLayout)
#' set.seed(123)
#' par(mar = c(3, 2, 2, 1))
#' 
#' # Prepare layout
#' lay  <- lay_new(matrix(1:4, nc = 2),
#'                 widths = c(3, 2),
#'                 heights = c(2, 1))
#' lay2 <- lay_new(matrix(1:3))
#' cl <- lay_bind_col(lay, lay2, widths = c(3, 1))
#' lay_set(cl) # initialize drawing area
#' 
#' # add plots
#' plot(1:100 + rnorm(100))
#' plot(rnorm(100), type = "l")
#' hist(rnorm(500))
#' acf(rnorm(100))
#' pie(c(3, 4, 6), col = 2:4)
#' pie(c(3, 2, 7), col = 2:4 + 3)
#' pie(c(5, 4, 2), col = 2:4 + 6)
#' 
lay_new <- function(mat, widths = NULL, heights = NULL)
{
  if(!is.matrix(mat)) mat <- matrix(mat)
  if(is.null(widths)) widths <- rep(1,ncol(mat))
  if(is.null(heights)) heights <- rep(1,nrow(mat))
  
  assertthat::assert_that(
    is.matrix(mat),
    is.numeric(widths),
    is.numeric(heights)
  )
  
  if(ncol(mat) != length(widths)) {
    stop(paste(
      "Number of columns in the 'mat' must",
      "match the length of the 'widths' vector.")
    )
  }
  
  if(nrow(mat) != length(heights)) {
    stop(paste(
      "Number of rows in the 'mat' must",
      "match the length of the 'heights' vector.")
    )
  }
  
  res <- list(mat = mat, widths = widths, heights = heights)
  class(res) <- "CustomLayout"
  
  .clean_lay(res)
}

#' @export
#' @rdname lay_new
layCreate <- function(mat, widths = NULL, heights = NULL) {
  .Deprecated("lay_new")
  lay_new(mat, widths, heights)
}

#' Set custom layout.
#' 
#' @param layout object of class Layout.
#' 
#' @export
#' @rdname lay_set
#' @examples
#' 
#' lplots = lay_new(matrix(1:2))
#' lpie   = lay_new(1)
#' lay = lay_bind_col(lplots,lpie)
#' lay_set(lay)
#' plot(1:10)
#' plot(1:10)
#' plot(1:20)
#' 
lay_set <- function(layout)
{
  assert_layout(layout)
  layout(
    layout$mat,
    widths = layout$widths,
    heights = layout$heights
  )
}

#' @export
#' @rdname lay_set
laySet <- function(layout) {
  .Deprecated("lay_set")
  lay_set(layout)
}

#' Take two Layout objects and combine by rows.
#' 
#' @param x object of class Layout
#' @param y object of class Layout
#' @param widths a vector with relative widths used in combining 
#'                the x and y layouts.
#' @param addmax if true (default) the ids of the plots in the second 
#'               layout will be shifted by the number of plots in 
#'               the first layout.
#'
#' @rdname lay_bind_col
#' @export
#' @examples
#' l1 = lay_new(matrix(c(1:2),ncol = 2),widths=c(4,1))
#' l2 = lay_new(matrix(c(1:4),ncol = 2),widths=c(1,1))
#' lb = lay_bind_col(l1,l2)
#' lay_show(lb)
#' 
lay_bind_col <- function(
    x, y,
    widths = c(1, 1),
    addmax = TRUE)
{
  assert_layout(x)
  assert_layout(y)
  assertthat::assert_that(length(widths) == 2)
  
  xmat <- x$mat
  ymat <- y$mat
  
  # move ids from the second matrix
  if (addmax) {
    ymat[ymat > 0] <- ymat[ymat > 0] + max(xmat)
  }
  
  ymat <- lay_rep_row(ymat, y$heights)
  xmat <- lay_rep_row(xmat, x$heights)
  
  rowx <- nrow(xmat)
  rowy <- nrow(ymat)
  lcm  <- .getSCM(rowx, rowy)
  
  xmat <- lay_rep_row(xmat, lcm / rowx)
  ymat <- lay_rep_row(ymat, lcm / rowy)
  
  mat <- cbind(xmat, ymat)
  widths <- c(x$widths * widths[1] * sum(y$widths),
             y$widths * widths[2] * sum(x$widths))
  widths  <- widths / .multipleGCD(widths)
  heights <- rep(1, nrow(mat))
  
  lay_new(
    mat = mat,
    widths = widths,
    heights = heights)
}

#' @rdname lay_bind_col
#' @export
layColBind <- function(
  x, y,
  widths = c(1, 1),
  addmax = TRUE) {
  
  .Deprecated("lay_bind_col")  
  lay_bind_col(x, y, widths, addmax)
}

#' Take two Layout objects and combine by rows.
#' 
#' @param x object of class Layout
#' @param y object of class Layout
#' @param heights a vector with relative heights used in combining 
#'                the x and y layouts.
#' @param addmax if true (default) the ids of the plots in the second 
#'               layout will be shifted by the number of plots in 
#'               the first layout.
#' 
#' @rdname lay_bind_row
#' @export
#' @examples
#' l1 = lay_new(matrix(c(1:2),ncol = 2),widths=c(4,1))
#' l2 = lay_new(matrix(c(1:4),ncol = 2),widths=c(1,1))
#' lb = lay_bind_row(l1,l2)
#' lay_show(lb)
#' 
lay_bind_row <- function(
   x,
   y,
   heights = c(1, 1),
   addmax = TRUE
) {
  
  assert_layout(x)
  assert_layout(y)
  assertthat::assert_that(length(heights) == 2)
  
  xmat <- x$mat
  ymat <- y$mat
  if (addmax) {
    ymat[ymat > 0] <- ymat[ymat > 0] + max(xmat)
  }
  
  ymat <- lay_rep_col(ymat, y$widths)
  xmat <- lay_rep_col(xmat, x$widths)
  
  colx <- ncol(xmat)
  coly <- ncol(ymat)
  lcm  <- .getSCM(colx, coly)
  
  xmat <- lay_rep_col(xmat, lcm / colx)
  ymat <- lay_rep_col(ymat, lcm / coly)
  
  mat <- rbind(xmat, ymat)
  widths  <- rep(1, ncol(mat))
  heights <- c(x$heights * heights[1] * sum(y$heights),
              sum(x$heights) * y$heights * heights[2])
  heights <- heights / .multipleGCD(heights)
  
  lay_new(
    mat = mat,
    widths = widths,
    heights = heights)
}

#' @export
#' @rdname lay_bind_row
layRowBind <- function(
  x,
  y,
  heights = c(1, 1),
  addmax = TRUE
) {
  .Deprecated("lay_bind_row")
  lay_bind_row(x, y, heights, addmax)
}
#' Use Layout object with grid graphics.
#'
#' @param grobs list of grobs.
#' @param lay a Layout object.
#' @param ... other parameters passed to \code{\link{grid.arrange}}.
#'
#' @export
#' @rdname lay_grid
#' @examples
#' 
#' library(ggplot2)
#' 
#' l1 <- lay_new(matrix(1:2, ncol = 1), heights = c(2, 3))
#' l2 <- lay_new(matrix(1:2, ncol = 1), heights = c(1, 3))
#' l3 <- lay_bind_col(l1, l2)
#' 
#' pl1 <- qplot(mpg, wt, data = mtcars)
#' pl2 <- qplot(mpg, gear, data = mtcars)
#' pl3 <- qplot(cyl, gear, data = mtcars)
#' pl4 <- qplot(qsec, am, data = mtcars)
#' 
#' lay_grid(list(pl1, pl2, pl3, pl4), l3)
#'
lay_grid <- function(grobs, lay, ...) {
  assert_layout(lay)
  gridExtra::grid.arrange(
    grobs = grobs,
    layout_matrix = lay$mat,
    widths = lay$widths,
    heights = lay$heights, ...)
}

#' @export
#' @rdname lay_grid
layGrid <- function(grobs, lay, ...) {
  .Deprecated("lay_grid")
  lay_grid(grobs, lay, ...)
}
