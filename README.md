
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
#> [1] "#AC6966" "#90792C" "#308974" "#4981A1" "#9A6D8F"
```

- generate gradient colors

``` r
gradient_color(c("blue", "red"), 10)
#>  [1] "#0000FF" "#1C00E2" "#3800C6" "#5500AA" "#71008D" "#8D0071" "#AA0055"
#>  [8] "#C60038" "#E2001C" "#FF0000"
```

- show colors

``` r
plot_col(gradient_color(c("blue", "red"), 10))
```

<img src="man/figures/README-color-plot_col-1.png" width="100%" />

## theme

- a custom flexible theme

``` r
ggplot(mini_diamond, aes(x = x, y = y, color = clarity)) +
  geom_point(size = 2) +
  facet_grid(. ~ cut) +
  labs(title = "title", tag = "tag", caption = "caption") +
  theme_pl()
```

<img src="man/figures/README-theme-theme_pl-1.png" width="100%" />

- a blank theme

``` r
ggplot(mini_diamond, aes(x = x, y = y, color = clarity)) +
  geom_point(size = 2) +
  facet_grid(. ~ cut) +
  labs(title = "title", tag = "tag", caption = "caption") +
  theme_pl0()
```

<img src="man/figures/README-theme-theme_pl0-1.png" width="100%" />

- a fixed mapping from size in `geom_xxx` to unit `pt` under 300 dpi

``` r
# for text and points
# geom_point(..., size = tpt(5))
# geom_text(..., size = tpt(5))

# for lines
# geom_line(..., linewidth = lpt(1))
```

- units transformation

``` r
# inches <-> centimeters
inch2cm(1)
#> [1] 2.54
#> attr(,"unit")
#> [1] 1
in2cm(1)
#> [1] 2.54
#> attr(,"unit")
#> [1] 1
cm2inch(1)
#> [1] 0.3937008
#> attr(,"unit")
#> [1] 2
cm2in(1)
#> [1] 0.3937008
#> attr(,"unit")
#> [1] 2

# inches <-> millimeters
inch2mm(1)
#> [1] 25.4
#> attr(,"unit")
#> [1] 7
in2mm(1)
#> [1] 25.4
#> attr(,"unit")
#> [1] 7
mm2inch(1)
#> [1] 0.03937008
#> attr(,"unit")
#> [1] 2
mm2in(1)
#> [1] 0.03937008
#> attr(,"unit")
#> [1] 2

# points <-> centimeters
pt2cm(1)
#> [1] 0.03514598
#> attr(,"unit")
#> [1] 1
cm2pt(1)
#> [1] 28.45276
#> attr(,"unit")
#> [1] 8

# points <-> millimeters
pt2mm(1)
#> [1] 0.3514598
#> attr(,"unit")
#> [1] 7
mm2pt(1)
#> [1] 2.845276
#> attr(,"unit")
#> [1] 8
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
