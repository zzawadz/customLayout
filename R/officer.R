#' Create layout for the officer PowerPoint slide.
#'
#' @param cl layout object
#' @param slideWidth width of the slide in inches (default 10)
#' @param slideHeight height of the slide in inches (default 7.5)
#'
#' @return
#' 
#' A list containing the coordinates of the slide segments created from layout scheme.
#' 
#' @export
#'
#' @examples
#' 
#' library(officer)
#' library(customLayout)
#' library(magrittr)
#' library(ggplot2)
#' 
#' lay = layCreate(matrix(1:4,nc=2),widths=c(3,2),heights=c(2,1))
#' lay2 = layCreate(matrix(1:3))
#' cl = layColBind(lay,lay2, widths=c(3,1))
#' 
#' allPositions <- layOfficerLayout(cl, innerMargins = rep(0.1,4))
#' 
#' my_pres <- read_pptx() %>% 
#'   add_slide(master = "Office Theme", layout = "Two Content")
#' 
#' p <- qplot(mpg, wt, data = mtcars)
#' 
#' for(pos in allPositions) {
#'   my_pres <- my_pres %>% ph_with_gg_at(
#'     p, 
#'     width = pos["width"],
#'     height = pos["height"], 
#'     left = pos["left"],
#'     top = pos["top"])
#' }
#' 
#' if(!dir.exists("tmp")) dir.create("tmp")
#' print(my_pres, target = "tmp/test-officer-layout.pptx")
#'
layOfficerLayout <- function(cl, slideWidth = 10, slideHeight = 7.5,
    margins = c(bottom = 0.25, left = 0.25, top = 0.25, right = 0.25),
    innerMargins = c(bottom = 0.025, left = 0.025, top = 0.025, right = 0.025)
    ) {
  x <- slideWidth  - sum(margins[c(1,3)])
  y <- slideHeight - sum(margins[c(2,4)])
  
  
  widths <- cl@widths / sum(cl@widths) * x
  heights  <- cl@heights / sum(cl@heights) * y
  
  ids <- seq_len(max(cl@mat))
  
  mat <- cl@mat
  
  startWidths <- c(0, cumsum(widths))
  startHeights <- c(0, cumsum(heights))
  
  getPositions <- function(id) {
    yy <- apply(mat, 1, function(x) any(x == id))
    xx <- apply(mat, 2, function(x) any(x == id))
    
    res <- c(
      left = as.numeric(startWidths[which(xx)[1]] + margins[2]),
      top = as.numeric(startHeights[which(yy)[1]] + margins[3]),
      width = sum(xx * widths),
      height = sum(yy * heights)
    )
    
    layOfficerAddInnerMargins(res, innerMargins = innerMargins)
  }
  
  allPositions <- setNames(lapply(ids, getPositions), ids)
  allPositions
}

layOfficerAddInnerMargins <- function(x, innerMargins) {
  x[1] <- x[1] + innerMargins[2]
  x[2] <- x[2] + innerMargins[3]
  x[3] <- x[3] - sum(innerMargins[c(2,4)])
  x[4] <- x[4] - sum(innerMargins[c(1,3)])
  x
}
