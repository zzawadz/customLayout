
# customLayout

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![GitHub
stars](https://img.shields.io/github/stars/zzawadz/customLayout.svg?style=social&label=Stars)](https://github.com/zzawadz/customLayout/stargazers)
[![GitHub
watchers](https://img.shields.io/github/watchers/zzawadz/customLayout.svg?style=social&label=Watch)](https://github.com/zzawadz/customLayout)

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/customLayout)](https://cran.r-project.org/package=customLayout)
[![Downloads](https://cranlogs.r-pkg.org/badges/customLayout)](https://cran.rstudio.com/package=customLayout)
[![](https://cranlogs.r-pkg.org/badges/grand-total/customLayout)](https://cran.rstudio.com/web/packages/customLayout/index.html)
[![Travis-CI Build
Status](https://travis-ci.org/zzawadz/customLayout.svg?branch=master)](https://travis-ci.org/zzawadz/customLayout)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/zzawadz/customLayout?branch=master&svg=true)](https://ci.appveyor.com/project/zzawadz/customLayout)
[![Coverage
Status](https://img.shields.io/codecov/c/github/zzawadz/customLayout/master.svg)](https://codecov.io/github/zzawadz/customLayout?branch=master)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

`customLayout` is a simple extension of the basic `layout` function from
`R`.

## Instalation:

`customLayout` is available only via GitHub and can be installed using
`devtools`:

``` r
require(devtools)
install_github("customLayout",username="zzawadz")
```

## Basic functionality:

You can create layouts in the using the same syntax as in the base
`layout` function:

``` r
library(customLayout)
lay <- layCreate(
  matrix(1:4, nc = 2),
  widths = c(3, 2),
  heights = c(2, 1))
layShow(lay)
```

![](man/figures/README-base-1.png)<!-- -->

``` r

lay2 <- layCreate(
  matrix(1:4, nc = 2),
  widths = c(3, 5),
  heights = c(2, 4))
layShow(lay2)
```

![](man/figures/README-base-2.png)<!-- -->

But the main strength of this package is in combining created layouts
with specific ratio:

``` r
# lay will be 3 times wider that lay2
# all ascects in lay and lay2 will be preserved
cl = layColBind(lay, lay2, widths = c(3, 1))
layShow(cl)
```

![](man/figures/README-combine-1.png)<!-- -->

You can create even more complicated layouts:

``` r
lay3 <- layCreate(matrix(1:2))
lay4 <- layRowBind(cl, lay3, heights = c(5, 2))
layShow(lay4)
```

![](man/figures/README-complicated-1.png)<!-- -->

``` r

lay5 <- layColBind(lay4, lay3, widths = c(5, 2))
layShow(lay5)
```

![](man/figures/README-complicated-2.png)<!-- -->

## Split layout

You can create very complicated layouts by splitting one field:

``` r
library(customLayout)
lay <- layCreate(
  matrix(1:4, nc = 2),
  widths = c(3, 2),
  heights = c(2, 1))
layShow(lay)
```

![](man/figures/README-split-1.png)<!-- -->

``` r
lay2 <- layCreate(
  matrix(1:4, nc = 2),
  widths = c(3, 5),
  heights = c(2, 4))
layShow(lay2)
```

![](man/figures/README-split-2.png)<!-- -->

``` r

# Split field 4 from lay into lay2:
slay <- laySplitField(lay, lay2, field = 4)
layShow(slay)
```

![](man/figures/README-split-3.png)<!-- -->

## Example session:

### Base graphics

``` r
library(customLayout)
par(mar = c(3, 2, 2, 1))
lay  <- layCreate(
  matrix(1:4, nc = 2),
  widths = c(3, 2),
  heights = c(2, 1))
lay2 <- layCreate(matrix(1:3))
cl   <- layColBind(lay, lay2, widths = c(3, 1))

laySet(cl) # initialize drawing area

set.seed(123)
plot(1:100 + rnorm(100))
plot(rnorm(100), type = "l")
hist(rnorm(500))
acf(rnorm(100))
pie(c(3, 4, 6), col = 2:4)
pie(c(3, 2, 7), col = 2:4 + 3)
pie(c(5, 4, 2), col = 2:4 + 6)
```

![](man/figures/README-example-1.png)<!-- -->

## Grid graphics (ggplot2 and friends)

``` r
library(customLayout)
library(ggplot2)
library(gridExtra)

lay  <- layCreate(
  matrix(1:2, ncol = 1))
lay2 <- layCreate(matrix(1:3))
cl   <- layColBind(lay, lay2, widths = c(3, 1))


library(ggplot2)
cuts <- sort(unique(diamonds[["cut"]]),
            decreasing = TRUE)

make_cut_plot <- function(cut) {
  dd <- diamonds[diamonds[["cut"]] == cut, ]
  pl <- ggplot(dd) +
      geom_point(aes(carat, price)) +
      facet_wrap("cut")
  pl
}

plots <- lapply(cuts, make_cut_plot)

layGrid(plots, cl)
```

![](man/figures/README-examplegrid-1.png)<!-- -->
