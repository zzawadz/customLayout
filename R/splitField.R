
#' Split a selected field from layout using a schema from another layout.
#'
#' @param lay a Layout object.
#' @param newlay a Layout object used to split a field from \code{lay}.
#' @param field id of a field from \code{lay}.
#'
#' @export
#' @rdname lay_split_field
#' @examples
#' 
#' l1 <- lay_new(matrix(c(1:4), ncol = 2), widths = c(4, 1))
#' l2 <- lay_new(matrix(c(1:4), ncol = 2), widths = c(1, 1))
#' l3 <- lay_split_field(l1, l2, 2)
#' lay_show(l3)
#' 
lay_split_field <- function(lay, newlay, field)
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
    east <- lay_new(east,widths = east_width,heights = east_height)
    east_ratio <- c(sum(widths[-neast]), sum(east_width))
  } else east <- NULL
  
  if((min(cols)-1) >= 1)
  {
    nwest <- 1:(min(cols)-1)
    west  <- mat[,nwest]
    west_width  <- widths[nwest]
    west_height <- heights
    west <- lay_new(west,widths=west_width,heights=west_height)
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
    north <- lay_new(north,widths = north_width,heights = north_height)
    north_ratio <- rev(c(sum(heights[-rownorth]), sum(north_height)))
  } else north <- NULL
  
  if(max(rows)+1 <= nrow(mat))
  {
    colsouth <- cols
    rowsouth <- (max(rows)+1):nrow(mat)
    south <- mat[rowsouth, colsouth, drop = FALSE]
    south_width  <- widths[colsouth]
    south_height <- heights[rowsouth]
    south <- lay_new(south,widths = south_width,heights = south_height)
    south_ratio <- c(sum(heights[-c(rowsouth, rownorth)]), sum(south_height)) 
  } else south <- NULL
  
  if(!is.null(south))
  {
    ls <- lay_bind_row(newlay, south, heights=south_ratio,addmax=FALSE)
  } else ls <- newlay
  
  if(!is.null(north))
  {
    lsn <- lay_bind_row(north, ls,heights=north_ratio, addmax = FALSE)
  } else lsn <- ls
  
  if(!is.null(west))
  {
    lsne <- lay_bind_col(west ,lsn, widths=west_ratio,addmax = FALSE)
  } else lsne <- lsn
  
  if(!is.null(east))
  {
    lsnew <- lay_bind_col(lsne, east, widths= east_ratio,addmax = FALSE)
  } else lsnew <- lsne
  
  lsnew
}

#' @export
#' @rdname lay_split_field
laySplitField <- function(lay, newlay, field) {
  .Deprecated("lay_split_field")
  lay_split_field(lay, newlay, field)
}
