#' Repeats rows in matrix.
#' 
#' @param x matrix
#' @param fr numeric value or vector with number of repetition of given row.
#' 
#' @examples
#' 
#' layRepRow(matrix(1:4,ncol=2),c(4,2))
#' layRepRow(matrix(1:4,ncol=2),2)
layRepRow = function(x,fr)
{
  nr = sum(fr)# ilosc wierszy w nowej macierzy:
  nx = NULL
  if(length(fr)!=nrow(x)) fr = rep(fr,nrow(x))
  for(i in 1:nrow(x))
    nx = rbind(nx, apply(x[i,,drop=FALSE],2, rep, fr[i]))
  nx
}

#' Repeats columns in matrix.
#' 
#' @param x matrix
#' @param fr numeric value or vector with number of repetition of given column.
#' 
#' @examples
#' 
#' layRepCol(matrix(1:4,ncol=2),c(4,2))
#' layRepCol(matrix(1:4,ncol=2),2)
layRepCol = function(x,fr)
{
  nr = sum(fr)# ilosc wierszy w nowej macierzy:
  nx = NULL
  if(length(fr)!=ncol(x)) fr = rep(fr,ncol(x))
  for(i in 1:ncol(x))
    for(j in 1:fr[i])
      nx = cbind(nx,x[,i])
  nx
}

layShow = function(layout)
{
  laySet(layout)
  layout.show(max(layout@mat))
}





.cleanCols = function(lay)
{
  mat = lay@mat
  widths = lay@widths
  i = 1
  while(i < ncol(mat))
  {
    if(all(mat[,i] == mat[,i+1]))
    {
      mat = mat[,-(i+1), drop = FALSE]
      widths[i] = widths[i] + widths[i+1]
      widths = widths[-(i+1)]
    } else i = i + 1
  }
  lay@mat = mat
  lay@widths = widths
  lay
}


.cleanRows = function(lay)
{
  mat = lay@mat
  heights = lay@heights
  i = 1
  while(i < nrow(mat))
  {
    if(all(mat[i,] == mat[i+1,]))
    {
      mat = mat[-(i+1),, drop = FALSE]
      heights[i] = heights[i] + heights[i+1]
      heights = heights[-(i+1)]
    } else i = i + 1
  }
  lay@mat = mat
  lay@heights = heights
  lay
}

.cleanLay = function(lay)
{
  lay = .cleanCols(lay)
  lay = .cleanRows(lay)
  lay
}


.getGCD = function(a,b) ifelse (b==0, a, .getGCD(b, a %% b)) 
.getSCM = function(a,b) (a*b)/.getGCD(a,b)




.multipleGCD = function(x) 
{
  for(i in 2:length(x)) x[i] = .getGCD(x[i-1],x[i])
  tail(x,1) 
}

