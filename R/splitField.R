
#' Split a selected field from layout using a schema from another layout.
#'
#' @param lay a Layout object.
#' @param newlay a Layout object used to split a field from \code{lay}.
#' @param field id of a field from \code{lay}.
#'
#' @export
#'
#' @examples
#' 
#' l1 <- layCreate(matrix(c(1:4), ncol = 2), widths = c(4, 1))
#' l2 <- layCreate(matrix(c(1:4), ncol = 2), widths = c(1, 1))
#' l3 <- laySplitField(l1, l2, 2)
#' layShow(l3)
#' 
laySplitField <- function(lay, newlay, field)
{

  n <- field
  mat     <- lay@mat
  widths  <- lay@widths
  heights <- lay@heights
  
  newmat <- newlay@mat
  
  maxn <- max(newmat)
  mat[mat > n] <- mat[mat > n] + (maxn - 1)
  newlay@mat <- newlay@mat + (n - 1)
  
  
  rows <- unique(unlist(apply(mat, 2, function(x)
    which(x == n))))
  cols <- unique(unlist(apply(mat, 1, function(x)
    which(x == n))))
  
  neast <- numeric(0)
  if((max(cols)+1) <= ncol(mat))
  {
    neast <- (max(cols)+1):ncol(mat)
    east  <- mat[,neast]
    east_width  <- widths[neast]
    east_height <- heights
    east <- layCreate(east,widths = east_width,heights = east_height)
    east_ratio <- c(sum(widths[-neast]), sum(east_width))
  } else east <- NULL
  
  if((min(cols)-1) >= 1)
  {
    nwest <- 1:(min(cols)-1)
    west  <- mat[,nwest]
    west_width  <- widths[nwest]
    west_height <- heights
    west <- layCreate(west,widths=west_width,heights=west_height)
    west_ratio <- rev(c(sum(widths[-c(nwest, neast)]), sum(west_width)))
  } else west <- NULL
  
  rownorth <- numeric(0)
  if((min(rows)-1) >= 1)
  {
    colnorth <- cols
    rownorth <- 1:(min(rows)-1)
    north <- mat[rownorth,colnorth, drop = FALSE]
    north_width <- widths[colnorth]
    north_height <- heights[rownorth]
    north <- layCreate(north,widths = north_width,heights = north_height)
    north_ratio <- rev(c(sum(heights[-rownorth]), sum(north_height)))
  } else north <- NULL
  
  if(max(rows)+1 <= nrow(mat))
  {
    colsouth <- cols
    rowsouth <- (max(rows)+1):nrow(mat)
    south <- mat[rowsouth, colsouth, drop = FALSE]
    south_width  <- widths[colsouth]
    south_height <- heights[rowsouth]
    south <- layCreate(south,widths = south_width,heights = south_height)
    south_ratio <- c(sum(heights[-c(rowsouth, rownorth)]), sum(south_height)) 
  } else south <- NULL
  
  if(!is.null(south))
  {
    ls <- layRowBind(newlay, south, heights=south_ratio,addmax=FALSE)
  } else ls <- newlay
  
  if(!is.null(north))
  {
    lsn <- layRowBind(north, ls,heights=north_ratio, addmax = FALSE)
  } else lsn <- ls
  
  if(!is.null(west))
  {
    lsne <- layColBind(west ,lsn, widths=west_ratio,addmax = FALSE)
  } else lsne <- lsn
  
  if(!is.null(east))
  {
    lsnew <- layColBind(lsne, east, widths= east_ratio,addmax = FALSE)
  } else lsnew <- lsne
  
  lsnew
}
