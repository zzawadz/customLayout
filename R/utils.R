#' Repeats rows in matrix.
#' 
#' @param x matrix
#' @param fr numeric value or vector with the number of repetition of a given row.
#' 
#' @noRd
#' @examples
#' 
#' lay_rep_row(matrix(1:4,ncol=2),c(4,2))
#' lay_rep_row(matrix(1:4,ncol=2),2)
#' 
lay_rep_row <- function(x,fr)
{
  nr <- sum(fr)# ilosc wierszy w nowej macierzy:
  nx <- NULL
  if(length(fr)!=nrow(x)) fr <- rep(fr,nrow(x))
  for(i in seq_len(nrow(x)))
    nx <- rbind(nx, apply(x[i,,drop=FALSE],2, rep, fr[i]))
  nx
}

#' Repeats columns in matrix.
#' 
#' @param x matrix
#' @param fr numeric value or vector with the number of repetition of a given column.
#' 
#' @noRd
#' @examples
#' 
#' lay_rep_col(matrix(1:4,ncol=2),c(4,2))
#' lay_rep_col(matrix(1:4,ncol=2),2)
#' 
lay_rep_col <- function(x, fr)
{
  nr <- sum(fr)# ilosc wierszy w nowej macierzy:
  nx <- NULL
  if(length(fr)!=ncol(x)) fr <- rep(fr,ncol(x))
  for(i in seq_len(ncol(x)))
    for(j in 1:fr[i])
      nx <- cbind(nx,x[,i])
  nx
}


#' Print the layout structure to the graphical device.
#'
#' @param layout an object of class Layout.
#'
#' @export
#' @rdname lay_show
#'
#' @examples
#' 
#' l1 <- lay_new(matrix(c(1:2), ncol = 2), widths = c(4, 1))
#' l2 <- lay_new(matrix(c(1:3), ncol = 3), widths = c(2, 1, 3))
#' l3 <- lay_bind_row(l1, l2, heights = c(2, 1))
#' lay_show(l3)
#' 
#' l4 <- lay_new(matrix(c(1:2), ncol = 2), widths = c(4, 1))
#' l5 <- lay_new(matrix(c(1:3), ncol = 1), heights = c(2, 1, 1))
#' l6 <- lay_bind_col(l4, l5, widths = c(1, 1))
#' lay_show(l6)
#' 
lay_show <- function(layout)
{
  lay_set(layout)
  n <- max(layout$mat)
  
  oma.saved <- graphics::par("oma")
  graphics::par(oma = rep.int(0, 4))
  graphics::par(oma = oma.saved)
  o.par <- graphics::par(mar = rep.int(0, 4))
  on.exit(graphics::par(o.par))
  
  colors <- RColorBrewer::brewer.pal(11, "Spectral")[-c(1, 2, 10, 11)]
  colsIdx <- seq_len(n) %% length(colors)
  colsIdx[colsIdx == 0] <- length(colors)
  colors <- colors[colsIdx]
  
  for (i in seq_len(n)) {
    graphics::plot.new()
    graphics::box(col = colors[i], fg = 1)
    graphics::rect(-0.04,-0.04,1.04,1.04, col = colors[i])
    graphics::text(0.5, 0.5, i, cex = 2)
  }
  invisible()
  
}

#' @export
#' @rdname lay_show
layShow <- function(layout) {
  .Deprecated("lay_show")
  lay_show(layout)
}

#' Internal customLayout function for shrinking the size of layuot's matrix.
#'
#' @param lay a CustomLayout object.
#' @noRd
#' 
.clean_cols <- function(lay)
{
  mat <- lay$mat
  widths <- lay$widths
  i <- 1
  while(i < ncol(mat))
  {
    if(all(mat[,i] == mat[,i+1]))
    {
      mat <- mat[,-(i+1), drop = FALSE]
      widths[i] <- widths[i] + widths[i+1]
      widths <- widths[-(i+1)]
    } else i <- i + 1
  }
  lay$mat <- mat
  lay$widths <- widths
  lay
}

#' Internal customLayout function for shrinking the size of layuot's matrix.
#'
#' @param lay a CustomLayout object.
#' @noRd
#' 
.clean_rows <- function(lay)
{
  mat <- lay$mat
  heights <- lay$heights
  i <- 1
  while(i < nrow(mat))
  {
    if(all(mat[i,] == mat[i+1,]))
    {
      mat <- mat[-(i+1),, drop = FALSE]
      heights[i] <- heights[i] + heights[i+1]
      heights <- heights[-(i+1)]
    } else i <- i + 1
  }
  lay$mat <- mat
  lay$heights <- heights
  lay
}

#' Internal customLayout function for shrinking the size of layuot's matrix.
#'
#' @param lay a CustomLayout object.
#' @noRd
#' 
.clean_lay <- function(lay)
{
  lay <- .clean_cols(lay)
  lay <- .clean_rows(lay)
  lay
}


.getGCD <- function(a,b) ifelse (b==0, a, .getGCD(b, a %% b)) 
.getSCM <- function(a,b) (a*b)/.getGCD(a,b)

.multipleGCD <- function(x) 
{
  for(i in 2:length(x)) x[i] <- .getGCD(x[i-1],x[i])
  utils::tail(x,1) 
}
