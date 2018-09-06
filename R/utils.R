#' Repeats rows in matrix.
#' 
#' @param x matrix
#' @param fr numeric value or vector with number of repetition of given row.
#' 
#' @noRd
#' @examples
#' 
#' layRepRow(matrix(1:4,ncol=2),c(4,2))
#' layRepRow(matrix(1:4,ncol=2),2)
#' 
layRepRow <- function(x,fr)
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
#' @param fr numeric value or vector with number of repetition of given column.
#' 
#' @noRd
#' @examples
#' 
#' layRepCol(matrix(1:4,ncol=2),c(4,2))
#' layRepCol(matrix(1:4,ncol=2),2)
#' 
layRepCol <- function(x,fr)
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
#'
#' @examples
#' 
#' l1 <- layCreate(matrix(c(1:2), ncol = 2), widths = c(4, 1))
#' l2 <- layCreate(matrix(c(1:3), ncol = 3), widths = c(2, 1, 3))
#' l3 <- layRowBind(l1, l2, heights = c(2, 1))
#' layShow(l3)
#' 
#' l4 <- layCreate(matrix(c(1:2), ncol = 2), widths = c(4, 1))
#' l5 <- layCreate(matrix(c(1:3), ncol = 1), heights = c(2, 1, 1))
#' l6 <- layColBind(l4, l5, widths = c(1, 1))
#' layShow(l6)
#' 
layShow <- function(layout)
{
  laySet(layout)
  n <- max(layout@mat)
  
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

.cleanCols <- function(lay)
{
  mat <- lay@mat
  widths <- lay@widths
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
  lay@mat <- mat
  lay@widths <- widths
  lay
}


.cleanRows <- function(lay)
{
  mat <- lay@mat
  heights <- lay@heights
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
  lay@mat <- mat
  lay@heights <- heights
  lay
}

.cleanLay <- function(lay)
{
  lay <- .cleanCols(lay)
  lay <- .cleanRows(lay)
  lay
}


.getGCD <- function(a,b) ifelse (b==0, a, .getGCD(b, a %% b)) 
.getSCM <- function(a,b) (a*b)/.getGCD(a,b)




.multipleGCD <- function(x) 
{
  for(i in 2:length(x)) x[i] <- .getGCD(x[i-1],x[i])
  utils::tail(x,1) 
}

