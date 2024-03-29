#' Calculate optimal fontsize and height of the cell for given height for flextable.
#'
#' @param data data.frame.
#' @param height single numeric value with desired height.
#'
#' @return 
#' 
#' A named numeric vector containing two elements:
#' \itemize{
#'   \item \code{fs} font size
#'   \item \code{height} of the single cell.
#' }
#' 
#' @export
#'
#' @examples
#' 
#' x <- tail(iris, 10)[,c(1,5)]
#' phl_calc_fontsize(x, 5)
#' 
phl_calc_fontsize <- function(data, height) {
  
  assertthat::assert_that(
    is.data.frame(data),
    assertthat::is.scalar(height)
  )
  
  nrows <- nrow(data) + 1 # add header
  const <- 0.01862963
  res <- c(fs = floor(height / nrows / const), height = height / nrows)
  res <- stats::setNames(res, c("fs", "height"))
  res
}

#' Create flextable for layout's placeholder.
#' 
#' @description 
#' 
#' Create flextable from data.frame and try to fit the result into layout's placeholder.
#' 
#' @param x data.frame.
#' @param olay officer layout created using \code{\link{phl_layout}}.
#' @param id of placeholder in \code{olay}.
#' @param method if 'all' (default) fits both the width and height. If 'height'
#'   fits only height.
#'
#' @return
#' 
#' A \code{flextable} object, which should 
#' fit into the layout's placeholder.
#' 
#' The result should be ready to pass it 
#' into \code{\link{phl_with_flextable}}.
#' 
#' @export
#'
#' @examples
#' 
#' lay <- lay_new(matrix(1:4,nc=2),widths=c(3,2),heights=c(2,1))
#' lay2 <- lay_new(matrix(1:3))
#' lay3 <- lay_bind_col(lay,lay2, widths=c(3,1))
#' offLayout <- phl_layout(lay3)
#' 
#' x <- tail(iris, 10)[,c(1,5)]
#' 
#' phl_adjust_table(x, offLayout, 1)
#' phl_adjust_table(x, offLayout, 2)
#' 
phl_adjust_table <- function(x, olay, id, method = c("all", "height")) {
  
  assert_id_inlayout(id, olay)
  assertthat::assert_that(is.data.frame(x))
  
  method <- match.arg(method, choices = c("all", "height"))
  dims <- olay[[id]]
  
  sizes <- phl_calc_fontsize(data = x, height = dims["height"])
  flTable <- flextable::flextable(x, cheight = sizes["height"])
  flTable <- flextable::fontsize(flTable, size = sizes["fs"], part = "all")
  
  widths <- flextable::dim_pretty(flTable)$widths
  if(sum(widths) > dims[["width"]]) {
    if(method == "height") {
      warning("Calculated width exceedes the placeholder width.")
    } else {
      
      fontSize <- floor(sizes["fs"] / (sum(widths) / dims[["width"]]))
      
      while (TRUE) {
        flTable <- flextable::fontsize(flTable, size = fontSize, part = "all")
        widths <- flextable::dim_pretty(flTable)$widths
        if(sum(widths) < dims[["width"]]) {
          break
        }
        fontSize <- floor(fontSize / (sum(widths) / dims[["width"]]))
      }
      height <- flextable::dim_pretty(flTable)$height
      height <- pmin(max(height), sizes[["height"]])
      flTable <- flextable::height_all(flTable, height = height)
    }
  }
  widths <- widths / sum(widths) * dims[["width"]]
  
  flTable <- flextable::width(flTable, width = widths)
  flTable
}

#' Add flextable into layout placeholder
#'
#' @param x rpptx object
#' @param olay an OfficerLayout object created using \code{\link{phl_layout}}.
#' @param id an single integer with an id of the placeholder from olay object.
#' @param value a \code{flextable} object. 
#'    Possibly the result of the \code{\link{phl_adjust_table}}
#'
#' @export
#' 
#' @return \code{rpptx} object which represents PowerPoint presentation in \code{officer}. The returned object contains a new element on the slide.
#'
#' @examples
#' 
#' library(officer)
#' lay <-  lay_new(matrix(1:4,nc=2),widths=c(3,2),heights=c(2,1))
#' lay2 <- lay_new(matrix(1:3))
#' lay3 <- lay_bind_col(lay,lay2, widths=c(3,1))
#' offLayout <- phl_layout(lay3)
#' 
#' pptx <- read_pptx()
#' pptx <- add_slide(
#'   pptx, 
#'   master = "Office Theme",
#'   layout = "Title and Content"
#' )
#' 
#' # add table to pptx file
#' x <- tail(iris, 10)[,c(1,5)]
#' xf <- phl_adjust_table(x, offLayout, 1)
#' pptx <- phl_with_flextable(pptx, offLayout, 1, xf)
#' 
#' x2 <- tail(iris, 10)[,c(1,5)]
#' xf2 <- phl_adjust_table(x, offLayout, 2)
#' pptx <- phl_with_flextable(pptx, offLayout, 2, xf2)
#' 
#' if(interactive()) {
#'   file <- tempfile(fileext = ".pptx")
#'   print(pptx, target = file)
#' }
#' 
phl_with_flextable <- function(x, olay, id, value) {
  
  assert_id_inlayout(id, olay)
  tableWidths <- vapply(value[c("header", "body", "footer")],
         function(x) sum(x$colwidths), FUN.VALUE = c(0.0))
  tableWidths <- max(tableWidths)
  
  tableHeights <- vapply(value[c("header", "body", "footer")],
          function(x) sum(x$rowheights), FUN.VALUE = c(0.0))
  tableHeights <- sum(tableHeights)
  
  lheight <- olay[[id]][["height"]]
  lwidth  <- olay[[id]][["width"]]
  
  if(tableWidths > lwidth + 0.001) {
    warning("Table total width is larger than placeholder size.\n",
      "Table width:       ", round(tableWidths, 3),
      "\nPlaceholder width: ", round(lwidth, 3))
  }
  
  if(tableHeights > lheight + 0.001) {
    warning("Table total height is larger than placeholder size.\n",
      "Table height:       ", round(tableHeights, 3),
      "\nPlaceholder height: ", round(lheight, 3))
  }
  
  officer::ph_with(
    x, value = value,
    location = ph_location(
    left = olay[[id]]["left"],
    top = olay[[id]]["top"]))
}
