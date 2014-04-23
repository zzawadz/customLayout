lay = createLayout(matrix(1:4,nc=2),widths=c(3,2),heights=c(2,1))

layoutShow(lay)

lay2 = createLayout(matrix(1:4,nc=2),widths=c(3,5),heights=c(2,4))
layoutShow(lay2)

cl = colBind(lay,lay2, widths=c(3,2))
layoutShow(cl)

lay3 = createLayout(matrix(1))

lay4 = rowBind(cl,lay3, heights=c(5,2))

layoutShow(lay4)
lay5 = colBind(lay4,lay3, widths=c(3,2))
layoutShow(lay5)

dim(lay5@mat)
