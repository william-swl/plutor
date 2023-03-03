
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plutor

<!-- badges: start -->

[![R-CMD-check](https://github.com/william-swl/plutor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/william-swl/plutor/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## installation

You can install the development version of `plutor` like so:

``` r
devtools::install_github("william-swl/plutor")
```

And load the package:

``` r
library(plutor)
```

## color

- select most distant colors among a color spectrum

``` r
select_color(5)
#> [1] "#AC6966" "#827E2B" "#308974" "#4981A1" "#9A6D8F"
```

## IO

- save a plot

``` r
# pl_save(p, 'plot.pdf', width=14, height=10)
```

- save a plot into an blank A4 canvas, or a custom canvas

``` r
# pl_save(p, 'plot.pdf', width=14, height=10, canvas='A4', units='cm')

# pl_save(p, 'plot.pdf', width=14, height=10, canvas=c(20, 25), units='cm')
```
