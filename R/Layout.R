#### TODO: Fix problem with zero


setClass("Layout", slots=c(mat="matrix",widths = "numeric",heights = "numeric"))


#' Create custom layout.
#' 
#' @param mat a matrix specifying the location of the figures. See \code{\link{layout}} for more information.
#' @param widths a vector of values for the relative heights of rows in mat.
#' @param heights a vector of values for the relative heights of rows in mat.
#' 
#' @export
#' @examples
#' library(customLayout)
#' par(mar = c(3,2,2,1))
#' lay = layCreate(matrix(1:4,nc=2),widths=c(3,2),heights=c(2,1))
#' lay2 = layCreate(matrix(1:3))
#' cl = layColBind(lay,lay2, widths=c(3,1))
#' laySet(cl) # initialize drawing area
#' plot(1:100+rnorm(100))
#' plot(rnorm(100), type = "l")
#' hist(rnorm(500))
#' acf(rnorm(100))
#' pie(c(3,4,6),col = 2:4)
#' pie(c(3,2,7),col = 2:4+3)
#' pie(c(5,4,2),col = 2:4+6)
layCreate <- function(mat, widths = NULL, heights = NULL)
{
  if(!is.matrix(mat)) mat <- matrix(mat)
  if(is.null(widths)) widths <- rep(1,ncol(mat))
  if(is.null(heights)) heights <- rep(1,nrow(mat))
  
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
  
  
  methods::new("Layout",mat=mat,widths = widths, heights = heights)
}

#' Set custom layout.
#' 
#' @param layout object of class Layout.
#' 
#' @export
#' @examples
#' 
#' lplots = layCreate(matrix(1:2))
#' lpie   = layCreate(1)
#' lay = layColBind(lplots,lpie)
#' laySet(lay)
#' plot(1:10)
#' plot(1:10)
#' plot(1:20)
laySet <- function(layout)
{
  layout(layout@mat,widths=layout@widths,heights=layout@heights)
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
#' @rdname layColBind 
#' @export
#' @examples
#' l1 = layCreate(matrix(c(1:2),ncol = 2),widths=c(4,1))
#' l2 = layCreate(matrix(c(1:4),ncol = 2),widths=c(1,1))
#' lb = layColBind(l1,l2)
#' layShow(lb)
#' 
setGeneric(
  "layColBind",
  function(
    x, y,
    widths = c(1, 1),
    addmax = TRUE)
    standardGeneric("layColBind")
)

#' @rdname layColBind
setMethod(
  "layColBind",
  signature = c(x = "Layout", y = "Layout"),
  function(
    x, y,
    widths = c(1, 1),
    addmax = TRUE
  )
{
  #Przesuwanie wszystkich wykresow z drugiej macierzy:
  xmat <- x@mat
  ymat <- y@mat
  if (addmax)
    ymat[ymat > 0] <- ymat[ymat > 0] + max(xmat)
  
  ymat <- layRepRow(ymat, y@heights)
  xmat <- layRepRow(xmat, x@heights)
  
  rowx <- nrow(xmat)
  rowy <- nrow(ymat)
  # najmniejszy wspolny dzielnik:
  lcm  <- .getSCM(rowx, rowy)
  
  xmat <- layRepRow(xmat, lcm / rowx)
  ymat <- layRepRow(ymat, lcm / rowy)
  
  mat <- cbind(xmat, ymat)
  widths <- c(x@widths * widths[1] * sum(y@widths),
             y@widths * widths[2] * sum(x@widths))
  widths  <- widths / .multipleGCD(widths)
  heights <- rep(1, nrow(mat))
  
  layout <- methods::new("Layout",
                        mat = mat,
                        widths = widths,
                        heights = heights)
  .cleanLay(layout)
})

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
#' @rdname layRowBind
#' @export
#' @examples
#' l1 = layCreate(matrix(c(1:2),ncol = 2),widths=c(4,1))
#' l2 = layCreate(matrix(c(1:4),ncol = 2),widths=c(1,1))
#' lb = layRowBind(l1,l2)
#' layShow(lb)
#' 
setGeneric(
  "layRowBind",
  function(
    x, y,
    heights = c(1, 1),
    addmax = TRUE)
  standardGeneric("layRowBind"))

#' @rdname layRowBind
setMethod(
  "layRowBind",
  signature = c(x = "Layout", y = "Layout"),
  function(x,
           y,
           heights = c(1, 1),
           addmax = TRUE
  )
{
  #Przesuwanie wszystkich wykresow z drugiej macierzy:
  xmat <- x@mat
  ymat <- y@mat
  if (addmax)
    ymat[ymat > 0] <- ymat[ymat > 0] + max(xmat)
  
  ymat <- layRepCol(ymat, y@widths)
  xmat <- layRepCol(xmat, x@widths)
  
  colx <- ncol(xmat)
  coly <- ncol(ymat)
  # najmniejszy wspolny dzielnik:
  lcm  <- .getSCM(colx, coly)
  
  xmat <- layRepCol(xmat, lcm / colx)
  ymat <- layRepCol(ymat, lcm / coly)
  
  mat <- rbind(xmat, ymat)
  widths  <- rep(1, ncol(mat))
  heights <- c(x@heights * heights[1] * sum(y@heights),
              sum(x@heights) * y@heights * heights[2])
  heights <- heights / .multipleGCD(heights)
  
  layout <- methods::new("Layout",
                        mat = mat,
                        widths = widths,
                        heights = heights)
  .cleanLay(layout)
})


#' Use Layout object with grid graphics.
#'
#' @param grobs list of grobs.
#' @param lay a Layout object.
#' @param ... other parameters passed to \code{\link{grid.arrange}}.
#'
#' @export
#'
#' @examples
#' 
#' library(ggplot2)
#' 
#' l1 <- layCreate(matrix(1:2, ncol = 1), heights = c(2, 3))
#' l2 <- layCreate(matrix(1:2, ncol = 1), heights = c(1, 3))
#' l3 <- layColBind(l1, l2)
#' 
#' pl1 <- qplot(mpg, wt, data = mtcars)
#' pl2 <- qplot(mpg, gear, data = mtcars)
#' pl3 <- qplot(cyl, gear, data = mtcars)
#' pl4 <- qplot(qsec, am, data = mtcars)
#' 
#' layGrid(list(pl1, pl2, pl3, pl4), l3)
#'
layGrid <- function(grobs, lay, ...) {

  gridExtra::grid.arrange(
    grobs = grobs,
    layout_matrix = lay@mat,
    widths = lay@widths,
    heights = lay@heights, ...)
}
