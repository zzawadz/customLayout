#' Print a OfficerCustomLayout object.
#'
#' @param x object of class OfficerCustomLayout
#' @param ... optional arguments to print or plot methods. Not used here.
#'
#' @export
#' 
#' @seealso lay_new lay_show phl_layout
#' 
#' @examples
#' 
#' lay  <- lay_new(matrix(1:4,nc = 2),widths = c(3, 2),heights = c(2, 1))
#' lay2 <- lay_new(matrix(1:3))
#' cl <- lay_bind_col(lay,lay2, widths=c(3,1))
#' ofl <- phl_layout(cl, innerMargins = rep(0.1,4))
#' print(ofl)
#' 
print.OfficerCustomLayout <- function(x, ...) {
  
  xname <- deparse(substitute(x))
  
  cat("OfficerCustomLayout object:\n")
  att <- attributes(x)
  
  print_attr <- function(att, name) {
    cat(
      sprintf(
        "\n  %s: %s",
        name, 
        paste(round(att[[name]], 2), collapse = ", ")
      )
    )
  }
  
  cat("  Elements:", length(x))
  print_attr(att, "slideWidth")
  print_attr(att, "slideHeight")
  print_attr(att, "margins")
  print_attr(att, "innerMargins")
}

#' Create layout for the officer PowerPoint slide.
#'
#' @param cl layout object
#' @param slideWidth width of the slide in inches (default 10)
#' @param slideHeight height of the slide in inches (default 7.5)
#' @param margins A numerical vector of the form c(bottom, left, top, right)f
#'  which gives the size of margins on the four sides of the layout.
#'  The default is c(0.25, 0.25, 0.25, 0.25).
#' @param innerMargins A numerical vector of the form c(bottom, left, top, right)
#'  which gives the size of margins on the four sides of the each placeholder in the layout.
#'  The default is c(0.025, 0.025, 0.025, 0.025).
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
#' lay = lay_new(matrix(1:4,nc = 2),widths=c(3, 2),heights=c(2, 1))
#' lay2 = lay_new(matrix(1:3))
#' cl = lay_bind_col(lay,lay2, widths = c(3,1))
#' 
#' allPositions <- phl_layout(cl, innerMargins = rep(0.1,4))
#' 
#' my_pres <- read_pptx() %>% 
#'   add_slide(master = "Office Theme", layout = "Two Content")
#' 
#' p <- qplot(mpg, wt, data = mtcars)
#' 
#' for(pos in allPositions) {
#'   my_pres <- my_pres %>% officer::ph_with_gg_at(
#'     p, 
#'     width = pos["width"],
#'     height = pos["height"], 
#'     left = pos["left"],
#'     top = pos["top"])
#' }
#' 
#' \dontrun{
#' if(!dir.exists("tmp")) dir.create("tmp")
#' print(my_pres, target = "tmp/test-officer-layout.pptx")
#' }
#' 
phl_layout <- function(cl, slideWidth = 10, slideHeight = 7.5,
    margins = c(bottom = 0.25, left = 0.25, top = 0.25, right = 0.25),
    innerMargins = c(bottom = 0.025, left = 0.025, top = 0.025, right = 0.025)
    ) {
  
  assert_layout(cl)
  assertthat::assert_that(
    assertthat::is.scalar(slideWidth),
    assertthat::is.scalar(slideHeight),
    length(margins) == 4,
    length(innerMargins) == 4
  )
  
  x <- slideWidth  - sum(margins[c(2,4)])
  y <- slideHeight - sum(margins[c(1,3)])
  
  
  widths <- cl$widths / sum(cl$widths) * x
  heights  <- cl$heights / sum(cl$heights) * y
  
  ids <- seq_len(max(cl$mat))
  
  mat <- cl$mat
  
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
  
  allPositions <- stats::setNames(lapply(ids, getPositions), ids)
  
  attr(allPositions, "layout") <- cl
  attr(allPositions, "slideWidth") <- slideWidth
  attr(allPositions, "slideHeight") <- slideHeight
  attr(allPositions, "innerMargins") <- innerMargins
  attr(allPositions, "margins") <- margins
  
  class(allPositions) <- "OfficerCustomLayout"
  
  allPositions
}

layOfficerAddInnerMargins <- function(x, innerMargins) {
  x[1] <- x[1] + innerMargins[2]
  x[2] <- x[2] + innerMargins[3]
  x[3] <- x[3] - sum(innerMargins[c(2,4)])
  x[4] <- x[4] - sum(innerMargins[c(1,3)])
  x
}

#' add ggplot into layout placeholder
#'
#' @param x rpptx object
#' @param olay an OfficerLayout object created using \code{\link{phl_layout}}
#' @param id an single integer with an id of the placeholder from \code{olay} object.
#' @param value a ggplot object
#' @param ... other arguments passed to \code{\link{ph_with_gg_at}}
#'
#' @export
#' 
phl_with_gg <- function(x, olay, id, value, ...) {
  
  assert_id_inlayout(id, olay)
  officer::ph_with_gg_at(
    x,
    value,
    width = olay[[id]]["width"],
    height = olay[[id]]["height"], 
    left = olay[[id]]["left"],
    top = olay[[id]]["top"],
    ...
  )
}

#' add a plot as vector graphics into layout placeholder
#'
#' @param x rpptx object
#' @param olay an OfficerLayout object created using \code{\link{phl_layout}}
#' @param id an single integer with an id of the placeholder from \code{olay} object.
#' @param code plot instructions.
#' @param ggobj ggplot objet to print. Argument code will be ignored if this argument is supplied.
#' @param ... other arguments passed to \code{\link{dml_pptx}}
#'
#' @export
#' 
phl_with_vg <- function(x, olay, id, code, ggobj = NULL, ...) {
  
  assert_officerlayout(olay)
  assert_id_inlayout(id, olay)
  rvg::ph_with_vg_at(
    x, code = code, ggobj = ggobj,
    width = olay[[id]]["width"],
    height = olay[[id]]["height"], 
    left = olay[[id]]["left"],
    top = olay[[id]]["top"],
    ...
  )
}


#' add plot into layout placeholder
#'
#' @param x rpptx object
#' @param olay an OfficerLayout object created using \code{\link{phl_layout}}
#' @param id an single integer with an id of the placeholder from \code{olay} object.
#' @param plotFnc a function which creates a plot when called.
#' @param ... other arguments passed to \code{\link{png}} function.
#'
#' @export
#' 
phl_with_plot <- function(x, olay, id, plotFnc, ...) {
  
  assert_id_inlayout(id, olay)
  file <- tempfile(fileext = ".png")
  options(bitmapType = "cairo")
  grDevices::png(filename = file,
      width = olay[[id]]["width"],
      height = olay[[id]]["height"],
      units = "in", 
      res = 300, ...)
  plotFnc()
  grDevices::dev.off()
  on.exit(unlink(file))
  
  officer::ph_with_img_at(
    x,
    file,
    width = olay[[id]]["width"],
    height = olay[[id]]["height"], 
    left = olay[[id]]["left"],
    top = olay[[id]]["top"]
  )
}


#' add text into layout placeholder
#'
#' @param x rpptx object
#' @param olay an OfficerLayout object created using \code{\link{phl_layout}}
#' @param id an single integer with an id of the placeholder from \code{olay} object.
#' @param str text to add.
#' @param type type of the text placeholder. See \code{\link{ph_add_text}} for more details.
#' @param ... other arguments passed to \code{\link{ph_add_text}}.
#'
#' @export
#'
phl_with_text <- function(x, olay, id, str, type = "title", ...) {
  
  assert_id_inlayout(id, olay)
  sldSum <- officer::slide_summary(x)
  
  x <- officer::ph_empty_at(
    x,
    width = olay[[id]]["width"],
    height = olay[[id]]["height"], 
    left = olay[[id]]["left"],
    top = olay[[id]]["top"],
    template_type = type
  )
  
  sldSum2 <- officer::slide_summary(x)
  index <- as.numeric(sldSum2[["id"]][!sldSum2[["id"]] %in% sldSum[["id"]]])
  
  officer::ph_add_par(x, type = type, id_chr = as.character(index))
  officer::ph_add_text(x, str, type = type, id_chr = as.character(index), ...)
}

#' add table into layout placeholder
#'
#' @param x rpptx object
#' @param olay an OfficerLayout object created using \code{\link{phl_layout}}
#' @param id an single integer with an id of the placeholder from \code{olay} object.
#' @param value a data.frame
#' @param ... other arguments passed to \code{\link{ph_with_table_at}}
#'
#' @export
#' 
phl_with_table <- function(x, olay, id, value, ...) {
  
  assert_id_inlayout(id, olay)
  
  officer::ph_with_table_at(
    x,
    value,
    width = olay[[id]]["width"],
    height = olay[[id]]["height"], 
    left = olay[[id]]["left"],
    top = olay[[id]]["top"],
    ...
  )
}
