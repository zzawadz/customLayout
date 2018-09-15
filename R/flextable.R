phl_calc_fontsize <- function(data, height) {
  nrows <- nrow(data) + 1 # add header
  const <- 0.01862963
  res <- c(fs = floor(height / nrows / const), height = height / nrows)
  res <- stats::setNames(res, c("fs", "height"))
  res
}

# lay <- layCreate(matrix(1:4,nc=2),widths=c(3,2),heights=c(2,1))
# lay2 <- layCreate(matrix(1:3))
# lay3 <- layColBind(lay,lay2, widths=c(3,1))
# offLayout <- phl_layout(lay3)
# 
# x <- tail(iris, 10)[,c(1,5)]

phl_adjust_table <- function(x, olay, id) {
  
  dims <- olay[[id]]
  
  sizes <- phl_calc_fontsize(data = x, height = dims["height"])
  flTable <- flextable(x, cheight = sizes["height"])
  flTable <- fontsize(flTable, size = sizes["fs"], part = "all")
  
  widths <- dim_pretty(flTable)$widths
  if(sum(widths) > dims[["width"]]) {
    warning("Calculated withd exceedes the placeholder width.")
  }
  widths <- widths / sum(widths) * dims[["width"]]
  
  flTable <- width(flTable, width = widths)
  flTable
}

phl_with_flextable <- function(x, value, olay, id) {
  
  tableWidths <- vapply(value[c("header", "body", "footer")],
         function(x) sum(x$colwidths), FUN.VALUE = c(0.0))
  tableWidths <- max(tableWidths)
  
  tableHeights <- vapply(value[c("header", "body", "footer")],
          function(x) sum(x$rowheights), FUN.VALUE = c(0.0))
  tableHeights <- sum(tableHeights)
  
  lheight <- olay[[id]][["height"]]
  lwidth  <- olay[[id]][["width"]]
  
  if(tableWidths > lwidth + 0.00001) {
    warning("Table total width is larger than placeholder size.\n",
      "Table width:       ", round(tableWidths, 3),
      "\nPlaceholder width: ", round(lwidth, 3))
  }
  
  if(tableHeights > lheight + 0.00001) {
    warning("Table total height is larger than placeholder size.\n",
      "Table height:       ", round(tableHeights, 3),
      "\nPlaceholder height: ", round(lheight, 3))
  }
  
  flextable::ph_with_flextable_at(
    x, value = value,
    left = olay[[id]]["left"],
    top = olay[[id]]["top"])
}
