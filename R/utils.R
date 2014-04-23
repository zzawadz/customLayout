#' Repeats rows in matrix.
#' 
#' @param x matrix
#' @param fr numeric value or vector with number of repetition of given row.
#' 
#' @examples
#' 
#' repRow(matrix(1:4,ncol=2),c(4,2))
#' repRow(matrix(1:4,ncol=2),2)

repRow = function(x,fr)
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
#' repCol(matrix(1:4,ncol=2),c(4,2))
#' repCol(matrix(1:4,ncol=2),2)
repCol = function(x,fr)
{
  nr = sum(fr)# ilosc wierszy w nowej macierzy:
  nx = NULL
  if(length(fr)!=ncol(x)) fr = rep(fr,ncol(x))
  for(i in 1:ncol(x))
    for(j in 1:fr[i])
      nx = cbind(nx,x[,i])
  nx
}

layoutShow = function(layout)
{
  setLayout(layout)
  layout.show(max(layout@mat))
}
