
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

It is recommended to perform initialization, which adjusts the default
plotting parameters in an interactive environment (such as jupyter
notebook) and sets the theme to `theme_pl()`.

``` r
pl_init()
```

## plots

Description values plot:

- The describe geom is used to create description values plot, including
  center symbol and error symbol.

- The center symbol can be mean, median or other custom functions.

- The error symbol can be sd, quantile or other custom functions.

``` r
mini_diamond %>% ggplot(aes(x = cut, y = price)) +
  geom_point() +
  geom_describe()
```

<img src="man/figures/README-plots-geom_describe-1.png" width="100%" />

``` r
center_func <- median
low_func <- function(x, na.rm) {
  quantile(x, 0.25, na.rm = na.rm)
}

high_func <- function(x, na.rm) {
  quantile(x, 0.75, na.rm = na.rm)
}

mini_diamond %>% ggplot(aes(x = cut, y = price)) +
  geom_point() +
  geom_describe(center_func = center_func, low_func = low_func, high_func = high_func)
```

<img src="man/figures/README-plots-geom_describe_custom-1.png" width="100%" />

Add p value and fold change on a plot

``` r
ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
  geom_point() +
  geom_describe(color = "red") +
  geom_compare(cp_label = c("psymbol", "right_deno_fc"), lab_pos = 25000, step_increase = 0.3) +
  ylim(0, 40000)
```

<img src="man/figures/README-plots-geom_point-1.png" width="100%" />

``` r
ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
  geom_point() +
  geom_describe(color = "red") +
  geom_compare(cp_ref = "Good", cp_inline = TRUE, lab_pos = 22000, brackets_widen = 0.1) +
  geom_compare(cp_ref = "Ideal", cp_inline = TRUE, lab_pos = 25000, brackets_widen = 0.1) +
  ylim(0, 40000)
```

<img src="man/figures/README-plots-geom_point_inline-1.png" width="100%" />

A new `Stat` class to add mean labels on a plot

``` r
mini_diamond %>% ggplot(aes(x = cut, y = price)) +
  geom_point() +
  geom_text(aes(label = price), stat = "meanPL")
```

<img src="man/figures/README-plots-StatMeanPL-1.png" width="100%" />

A new `Stat` class to add count labels on a plot

``` r
mini_diamond %>% ggplot(aes(x = cut, y = price)) +
  geom_point() +
  geom_text(aes(label = price), stat = "countPL")
```

<img src="man/figures/README-plots-StatCountPL-1.png" width="100%" />

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

- set repr size and resolution

``` r
pl_size(width = 3, height = 2)
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

## echarts4r

### echarts theme

- set colors for echarts plot

``` r
mini_diamond %>%
  group_by(cut) %>%
  e_charts(x, width = 500, height = 500) %>%
  e_scatter(y, symbol_size = 10) %>%
  pe_color(c("red", "yellow"), default = "black")
```

<div id="htmlwidget-ae275414e7dc98a1157d" style="width:500px;height:500px;" class="echarts4r html-widget "></div>
<script type="application/json" data-for="htmlwidget-ae275414e7dc98a1157d">{"x":{"theme":"custom","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["Fair","Good","Ideal"]},"series":[{"data":[{"value":[4.47,4.35]},{"value":[4.68,4.73]},{"value":[4.72,4.77]},{"value":[4.8,4.76]},{"value":[5.09,4.98]},{"value":[5.26,5.2]},{"value":[5.36,5.41]},{"value":[5.56,5.41]},{"value":[5.64,5.5]},{"value":[5.65,5.39]},{"value":[5.66,5.71]},{"value":[5.67,5.57]},{"value":[5.83,5.81]},{"value":[5.87,5.81]},{"value":[6.08,6.04]},{"value":[6.1,6.12]},{"value":[6.25,6.18]},{"value":[6.26,6.21]},{"value":[6.26,6.29]},{"value":[6.27,6.23]},{"value":[6.34,6.29]},{"value":[6.38,6.4]},{"value":[6.48,6.52]},{"value":[6.61,6.54]},{"value":[6.77,6.71]},{"value":[6.9,6.8]},{"value":[6.98,6.93]},{"value":[7.23,7.19]},{"value":[7.63,7.59]},{"value":[7.78,7.74]},{"value":[7.87,7.8]},{"value":[7.97,7.92]},{"value":[8.3,8.19]},{"value":[8.33,8.37]},{"value":[8.47,8.31]}],"name":"Fair","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":10},{"data":[{"value":[4.09,4.12]},{"value":[4.1,4.07]},{"value":[4.23,4.3]},{"value":[4.33,4.35]},{"value":[4.4,4.44]},{"value":[4.57,4.55]},{"value":[4.64,4.68]},{"value":[4.7,4.74]},{"value":[4.71,4.73]},{"value":[4.72,4.78]},{"value":[4.75,4.8]},{"value":[4.77,4.79]},{"value":[4.85,4.78]},{"value":[5.05,5.08]},{"value":[5.12,5.18]},{"value":[5.49,5.56]},{"value":[5.62,5.59]},{"value":[6.09,6.05]},{"value":[6.11,6.17]},{"value":[6.16,6.07]},{"value":[6.27,6.31]},{"value":[6.32,6.3]},{"value":[6.37,6.4]},{"value":[6.63,6.71]},{"value":[6.63,6.55]},{"value":[6.72,6.68]},{"value":[6.93,6.88]},{"value":[7.18,7.24]},{"value":[7.27,7.18]},{"value":[7.6,7.56]},{"value":[8.66,8.57]}],"name":"Good","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":10},{"data":[{"value":[4.19,4.23]},{"value":[4.27,4.29]},{"value":[4.32,4.34]},{"value":[4.33,4.39]},{"value":[4.33,4.36]},{"value":[4.36,4.41]},{"value":[4.38,4.4]},{"value":[4.38,4.34]},{"value":[4.45,4.46]},{"value":[4.72,4.75]},{"value":[4.73,4.76]},{"value":[5.11,5.05]},{"value":[5.15,5.18]},{"value":[5.15,5.11]},{"value":[5.31,5.32]},{"value":[5.34,5.39]},{"value":[5.4,5.36]},{"value":[5.69,5.72]},{"value":[5.72,5.78]},{"value":[5.73,5.77]},{"value":[5.77,5.82]},{"value":[5.91,5.96]},{"value":[6.34,6.28]},{"value":[6.35,6.39]},{"value":[6.37,6.41]},{"value":[6.4,6.43]},{"value":[6.43,6.4]},{"value":[6.54,6.5]},{"value":[6.6,6.64]},{"value":[6.62,6.67]},{"value":[6.65,6.69]},{"value":[6.66,6.7]},{"value":[7.43,7.45]},{"value":[8.31,8.26]}],"name":"Ideal","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":10}]},"dispose":true,"theme2":true,"customTheme":"{\"color\":[\"red\",\"yellow\",\"black\"]}","theme_name":"custom"},"evals":[],"jsHooks":[]}</script>

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
