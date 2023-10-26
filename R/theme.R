#' set size, resolution and default theme
#'
#' @param width width
#' @param height height
#' @param res resolution, 300 as default
#' @param w alias of width
#' @param h alias of height
#' @param theme default theme
#'
#' @export
#'
#' @return no return value
#'
#' @examples pl_init()
pl_init <- function(width = 4, height = 3, res = 300, w = NULL, h = NULL,
                    theme = theme_pl()) {
  width <- alias_arg(width, w, default = width)
  height <- alias_arg(height, h, default = height)
  options(
    repr.plot.width = width, repr.plot.height = height,
    repr.plot.res = res
  )
  theme_set(theme)
}

#' set repr size and resolution
#'
#' @param width width
#' @param height height
#' @param res resolution, 300 as default
#' @param w alias of width
#' @param h alias of height
#' @return no return value
#' @export
#' @examples pl_size(width = 3, height = 2)
pl_size <- function(width = 4, height = 3, res = 300, w = NULL, h = NULL) {
  width <- alias_arg(width, w, default = width)
  height <- alias_arg(height, h, default = height)
  options(
    repr.plot.width = width, repr.plot.height = height,
    repr.plot.res = res
  )
}



#' trans inch to cm
#'
#' @param x inch value
#'
#' @return cm value
#' @export
#'
#' @examples inch2cm(1)
inch2cm <- function(x) {
  grid::convertUnit(unit(x, "inch"), "cm") %>% unclass()
}

#' @rdname inch2cm
#' @export
in2cm <- inch2cm



#' trans cm to inch
#'
#' @param x cm value
#'
#' @return inch value
#' @export
#'
#' @examples cm2inch(1)
cm2inch <- function(x) {
  grid::convertUnit(unit(x, "cm"), "inch") %>% unclass()
}

#' @rdname inch2cm
#' @export
cm2in <- cm2inch

#' trans inch to mm
#'
#' @param x inch value
#'
#' @return mm value
#' @export
#'
#' @examples inch2mm(1)
inch2mm <- function(x) {
  grid::convertUnit(unit(x, "inch"), "mm") %>% unclass()
}

#' @rdname inch2mm
#' @export
in2mm <- inch2mm

#' trans mm to inch
#'
#' @param x mm value
#'
#' @return inch value
#' @export
#'
#' @examples mm2inch(1)
mm2inch <- function(x) {
  grid::convertUnit(unit(x, "mm"), "inch") %>% unclass()
}

#' @rdname mm2inch
#' @export
mm2in <- mm2inch


#' trans pt to cm
#'
#' @param x pt value
#'
#' @return cm value
#' @export
#'
#' @examples pt2cm(1)
pt2cm <- function(x) {
  grid::convertUnit(unit(x, "pt"), "cm") %>% unclass()
}

#' trans cm to pt
#'
#' @param x cm value
#'
#' @return pt value
#' @export
#'
#' @examples cm2pt(1)
cm2pt <- function(x) {
  grid::convertUnit(unit(x, "cm"), "pt") %>% unclass()
}


#' trans pt to mm
#'
#' @param x pt value
#'
#' @return mm value
#' @export
#'
#' @examples pt2mm(1)
pt2mm <- function(x) {
  grid::convertUnit(unit(x, "pt"), "mm") %>% unclass()
}

#' trans mm to pt
#'
#' @param x mm value
#'
#' @return pt value
#' @export
#'
#' @examples mm2pt(1)
mm2pt <- function(x) {
  grid::convertUnit(unit(x, "mm"), "pt") %>% unclass()
}



#' trans geom text or point to the real point
#'
#' @param x text point in geom
#'
#' @return real point
#' @export
#'
#' @examples tpt(1)
tpt <- function(x) {
  pt2mm(x)
}

#' @rdname tpt
#' @export
ppt <- tpt


#' trans geom line point and theme line point to the real point
#'
#' @param x line point in geom or theme
#'
#' @return real point
#' @export
#'
#' @examples lpt(1)
lpt <- function(x) {
  x / 2.14195903051181
}


#' scale element according to a vector of element scales
#'
#' @param level output level
#' @param base value of base level
#' @param ele_scales vector of element scales
#'
#' @return value of output level
#' @export
#'
#' @examples scale_ele(level = 2, base = 5, ele_scales = c(1, 2))
scale_ele <- function(level, base, ele_scales) {
  base * (ele_scales[level] / ele_scales[1])
}


#' a new extensible theme
#'
#' @param base_size base size of fonts and margins
#' @param size_scales a vector of element size scales, namely:
#' 1. base size, used by legend text, axis text, caption
#' 2. used by legend title, axis title, strip text (facet title), subtitle
#' 3. used by title, tag
#' @param base_line_size base linewidth
#' @param base_rect_size base linewidth of the rectangles
#' @param margin_factor factor to adjust the element margins according to
#' size_scales
#' @param font_family font family
#' @param plot_margin_factor factor to adjust the plot margins according to
#' `size_scales[3]`
#' @param legend_spacing_factor factor to adjust the space of legend items
#' according to `size_scales[2]`
#' @param ... arguments from ggplot2::theme()
#'
#' @return theme object of ggplot
#' @export
#'
#' @examples theme_pl()
theme_pl <- function(base_size = 10,
                     base_line_size = lpt(base_size / 10),
                     base_rect_size = lpt(1),
                     size_scales = c(5, 6, 7),
                     margin_factor = 0.25, plot_margin_factor = 0.5,
                     legend_spacing_factor = 1.2,
                     font_family = "", ...) {
  if (length(size_scales) != 3) {
    stop("the length of size_scales should be 3!")
  }

  scale_size <- function(x) scale_ele(x, base_size, size_scales)

  ggplot2::theme_grey(
    base_size = base_size,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size,
    base_family = font_family
  ) +
    ggplot2::theme(
      ##################################
      ### text
      ##################################

      # global text, margins in pt as default
      text = element_text(
        family = font_family, face = "plain", color = "black", size = base_size,
        hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(0, 0, 0, 0)
      ),
      # plot tag. e.g. A, B, C
      plot.tag = element_text(
        family = font_family, face = "bold",
        size = scale_size(3)
      ),
      # plot title
      plot.title = element_text(
        family = font_family, face = "bold", size = scale_size(3), hjust = 0,
        margin = margin(b = scale_size(3) * margin_factor)
      ),
      # plot subtitle, under the title
      plot.subtitle = element_text(
        family = font_family, size = scale_size(2), hjust = 0,
        margin = margin(b = scale_size(2) * margin_factor)
      ),
      # plot caption, bottom of the plot
      plot.caption = element_text(
        family = font_family,
        size = scale_size(1), hjust = 1
      ),
      # plot legend title and text
      legend.title = element_text(
        family = font_family,
        size = scale_size(2), hjust = 0
      ),
      legend.text = element_text(
        family = font_family,
        size = scale_size(1), hjust = 0
      ),
      # x, y axis title
      axis.title.x = element_text(
        family = font_family, size = scale_size(2),
        margin = margin(t = scale_size(2) * margin_factor * 0.8)
      ),
      axis.title.y = element_text(
        family = font_family, size = scale_size(2), angle = 90,
        margin = margin(r = scale_size(2) * margin_factor) * 2
      ),
      # x, y axis tick label
      axis.text = element_text(
        family = font_family, color = "black",
        size = scale_size(1)
      ),
      # facet title
      strip.text = element_text(
        family = font_family,
        size = scale_size(2),
        margin = margin(
          b = scale_size(2) * margin_factor,
          t = scale_size(2) * margin_factor
        )
      ),

      ##################################
      ### line
      ##################################
      # global line
      line = element_line(
        color = "black", linetype = 1, lineend = "round"
      ),
      # grid in plot panel
      panel.grid = element_blank(),
      # axis line
      axis.line = element_line(color = "black", linewidth = base_line_size),

      # axis tick line width
      axis.ticks = element_line(color = "black"),
      # axis tick line length
      axis.ticks.length = unit(6 * base_line_size, "pt"),
      ggh4x.axis.ticks.length.minor = ggplot2::rel(0.6),

      ##################################
      ### rect
      ##################################
      #  global rect
      rect = element_rect(
        fill = NA, color = NA, linewidth = base_rect_size, linetype = 1
      ),
      # backgrounds
      plot.background = element_blank(),
      panel.background = element_blank(),
      legend.background = element_blank(),
      strip.background = element_blank(),
      # the background of symbols in lenged
      legend.key = element_blank(),

      ##################################
      ### Others
      ##################################
      # legend position
      legend.position = "right",
      legend.direction = "vertical",
      legend.justification = c("left", "center"),
      # width of legend key
      legend.key.width = unit(scale_size(1), "pt"),
      # height of legend key, same as space between items
      legend.key.height = unit(scale_size(2) * legend_spacing_factor, "pt"),
      # space between lenged title and items
      legend.spacing.y = unit(scale_size(2) * legend_spacing_factor, "pt"),
      # space between legend keys and items
      legend.spacing.x = unit(scale_size(1) * margin_factor, "pt"),

      # margin of whole lenged box
      legend.margin = margin(0, 0, 0, 0),
      # space between facet panels
      panel.spacing = unit(scale_size(3) * 2 * margin_factor, "pt"),
      # tag position
      plot.tag.position = "topleft",
      # plot margins
      plot.margin = margin(
        scale_size(3) * plot_margin_factor,
        scale_size(3) * plot_margin_factor,
        scale_size(3) * plot_margin_factor,
        scale_size(3) * plot_margin_factor
      )
    ) +
    # allow modification
    ggplot2::theme(...)
}



#' a blank theme
#'
#' @param ... arguments of theme_pl()
#'
#' @return theme object of ggplot
#' @export
#'
#' @examples theme_pl0()
theme_pl0 <- function(...) {
  theme_pl(
    legend.position = "none", title = element_blank(),
    axis.line = element_blank(), axis.ticks = element_blank(),
    axis.title.x = element_blank(), axis.title.y = element_blank(),
    axis.text = element_blank()
  ) +
    ggplot2::theme(...)
}
