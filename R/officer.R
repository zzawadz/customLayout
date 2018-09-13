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
#' allPositions <- layOfficerLayout(cl)
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
layOfficerLayout <- function(cl, slideWidth = 10, slideHeight = 7.5) {
  x <- slideWidth
  y <- slideHeight
  
  
  widths <- cl@widths / sum(cl@widths) * x
  heights  <- cl@heights / sum(cl@heights) * y
  
  ids <- seq_len(max(cl@mat))
  
  mat <- cl@mat
  id <- 1
  
  startWidths <- c(0, cumsum(widths))
  startHeights <- c(0, cumsum(heights))
  
  getPositions <- function(id) {
    yy <- apply(mat, 1, function(x) any(x == id))
    xx <- apply(mat, 2, function(x) any(x == id))
    
    c(
      left = startWidths[which(xx)[1]],
      top = startHeights[which(yy)[1]],
      width = sum(xx * widths),
      height = sum(yy * heights)
    )
  }
  
  allPositions <- setNames(lapply(ids, getPositions), ids)
  allPositions
}
