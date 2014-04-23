#### TODO: poprawic problem z zerem!


setClass("Layout", slots=c(mat="matrix",widths = "numeric",heights = "numeric", plots = "list"))


#' Create custom layout.
#' 
#' @param layout object of class Layout.
#' 
#' @examples
#' require(customLayout)
#' par(mar = c(3,2,2,1))
#' lay = createLayout(matrix(1:4,nc=2),widths=c(3,2),heights=c(2,1))
#' lay2 = createLayout(matrix(1:3))
#' cl = colBind(lay,lay2, widths=c(3,1))
#' setLayout(cl) # initialize drawing area
#' plot(1:100+rnorm(100))
#' plot(rnorm(100), type = "l")
#' hist(rnorm(500))
#' acf(rnorm(100))
#' pie(c(3,4,6),col = 2:4)
#' pie(c(3,2,7),col = 2:4+3)
#' pie(c(5,4,2),col = 2:4+6)
createLayout = function(mat, widths = NULL, heights = NULL, plots = list())
{
  if(!is.matrix(mat)) mat = matrix(mat)
  if(is.null(widths)) widths = rep(1,ncol(mat))
  if(is.null(heights)) heights = rep(1,nrow(mat))
  
  new("Layout",mat=mat,widths = widths, heights = heights, plots = plots)
}


#' Set custom layout.
#' 
#' @param layout object of class Layout.
#' 
#' @examples
#' 
#' lplots = createLayout(matrix(1:2))
#' lpie   = createLayout(1)
#' lay = colBind(lplots,lpie)
#' setLayout(lay)
#' plot(1:10)
#' plot(1:10)
#' plot(1:20)
setLayout = function(layout)
{
  layout(layout@mat,widths=layout@widths,heights=layout@heights)
}


#' Take two Layout objects and combine by rows.
#' 
#' @param x object of class Layout
#' @param y object of class Layout
#' 
#' @examples
#' l1 = createLayout(matrix(c(1:2),ncol = 2),widths=c(4,1))
#' l2 = createLayout(matrix(c(1:4),ncol = 2),widths=c(1,1))
#' lb = colBind(l1,l2)
#' layoutShow(lb)
setGeneric("colBind",function(x,y, widths = c(1,1)) standardGeneric("colBind"))
setMethod("colBind", signature=c(x="Layout",y="Layout"),function(x,y, widths = c(1,1))
{
  #Przesuwanie wszystkich wykresow z drugiej macierzy:
  xmat = x@mat
  ymat = y@mat + max(xmat)
  
  ymat = repRow(ymat,y@heights)
  xmat = repRow(xmat,x@heights)
  
  rowx = nrow(xmat)
  rowy = nrow(ymat)
  # najmniejszy wspolny dzielnik:
  lcm  = scm(rowx, rowy)
  
  xmat = repRow(xmat,lcm/rowx)
  ymat = repRow(ymat,lcm/rowy)
  
  mat = cbind(xmat,ymat)
  widths = c(x@widths*widths[1]*sum(y@widths), y@widths*widths[2]*sum(x@widths))
  heights = rep(1,nrow(mat))
  
  layout = new("Layout",mat = mat, widths = widths, heights = heights, plots = c(x@plots,y@plots))
  .cleanLay(layout)
})

#' Take two Layout objects and combine by rows.
#' 
#' @param x object of class Layout
#' @param y object of class Layout
#' 
#' @examples
#' l1 = createLayout(matrix(c(1:2),ncol = 2),widths=c(4,1))
#' l2 = createLayout(matrix(c(1:4),ncol = 2),widths=c(1,1))
#' lb = rowBind(l1,l2)
#' layoutShow(lb)
setGeneric("rowBind",function(x,y, heights = c(1,1)) standardGeneric("rowBind"))
setMethod("rowBind", signature=c(x="Layout",y="Layout"),function(x,y, heights = c(1,1))
{
  #Przesuwanie wszystkich wykresow z drugiej macierzy:
  xmat = x@mat
  ymat = y@mat + max(xmat)
  
  ymat = repCol(ymat,y@widths)
  print(ymat)
  print(y@widths)
  print(x@widths)
  xmat = repCol(xmat,x@widths)
  
  colx = ncol(xmat)
  coly = ncol(ymat)
  # najmniejszy wspolny dzielnik:
  lcm  = scm(colx, coly)
  
  xmat = repCol(xmat,lcm/colx)
  ymat = repCol(ymat,lcm/coly)
  
  mat = rbind(xmat,ymat)
  widths = rep(1,ncol(mat))
  heights = c(x@heights*heights[1]*sum(y@heights), sum(x@heights)*y@heights*heights[2])
  
  layout = new("Layout",mat = mat, widths = widths, heights = heights,plots = c(x@plots,y@plots))
  .cleanLay(layout)
})


#' Create list of plots that can be stored in Layout object.
#' 
#' @param ... expressions that produces plots
#' 
#' @examples
#' 
#' # Easy way to draw plots stored in list of plots
#' list_plot = listPlot(plot(1:1000), plot(1:100))
#' par(mfrow = c(2,1))
#' sapply(list_plot, eval)
#' 
#' #' list_plot = listPlot(plot(1:1000), plot(1:100))
#'
#' lplots = createLayout(matrix(1:2),plots=list_plot)
#' renderPlots(lplots)
listPlot = function(...)
{
  as.list(eval(substitute(expression(...))))
}


#' Set custom layout and render all plots stored in Layout object.
#' 
#' @param layout object of class Layout.
#' 
#' @examples
#' 
#' list_plot = listPlot(plot(1:1000), plot(1:100))
#' list_pie  = listPlot(pie(1:3,col = 2:4))
#'
#' lplots = createLayout(matrix(1:2),plots=list_plot)
#' lpie   = createLayout(1,plots=list_pie)
#' lay = colBind(lplots,lpie)
#' renderPlots(lay)
renderPlots = function(layout)
{
  setLayout(layout)
  sapply(layout@plots, function(x) eval(x))
  return(invisible())
}



