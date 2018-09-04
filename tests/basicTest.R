library(customLayout)
lay = createLayout(matrix(1:4,nc=2),widths=c(3,2),heights=c(2,1))
layShow(lay)

lay2 = createLayout(matrix(1:4,nc=2),widths=c(3,5),heights=c(2,4))
layShow(lay2)

cl = colBind(lay,lay2, widths=c(3,2))
layShow(cl)

lay3 = createLayout(matrix(1:2))

lay4 = rowBind(cl,lay3, heights=c(5,2))

layShow(lay4)
lay5 = colBind(lay4,lay3, widths=c(5,2))
layShow(lay5)

lay5@mat


layShow(lay5)
lay6 = splitField(lay5,lay2,8)
layShow(lay6)

lay6 = splitField(lay5,lay2,7)

layShow(lay6)

layShow(splitField(lay5,lay2,11))
