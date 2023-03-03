#' trans inch to cm
#'
#' @param x inch value
#'
#' @return cm value
#' @export
#'
#' @examples inch2cm(29.7)
inch2cm <- function(x) {
  x * 2.54
}

#' trans cm to inch
#'
#' @param x cm value
#'
#' @return inch value
#' @export
#'
#' @examples cm2inch(7)
cm2inch <- function(x) {
  x / 2.54
}


#' trans geom text point to the real point
#'
#' @param x text point in geom
#'
#' @return real point
#' @export
#'
#' @examples tpt(5)
tpt <- function(x) {
  x / 2.84527559055118
}

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
#' 2. used by annotation labels
#' 3. used by legend title, axis title, strip text
#' 4. used by subtitle
#' 5. used by title
#' 6. used by tag
#' @param base_lw base line width
#' @param margin_factor the factor of margin to size, default is 0.25
#' @param font_family font family
#' @param ...
#'
#' @return theme object of ggplotusethis::use_version()
#' @export
#'
#' @examples theme_pl()
theme_pl <- function(base_size = 10, size_scales = c(5, 5, 6, 8, 10, 10),
                     base_lw = 1,
                     margin_factor = 0.25, font_family = "",
                     ...) {
  if (length(size_scales) != 6) {
    stop("the length of size_scales should be 6!")
  }
  scale_size <- function(x) scale_ele(x, base_size, size_scales, base_lw = 1)

  ggplot2::theme_classic(
    base_size = base_size,
    base_family = font_family
  ) %+replace%
    ggplot2::theme(
      ##################################
      ### text
      ##################################

      # global text
      text = element_text(
        face = "plain", color = "black", size = base_size,
        hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(0, 0, 0, 0)
      ),
      # plot tag. e.g. A, B, C
      plot.tag = element_text(face = "bold", size = scale_size(6), hjust = 0),
      # plot title
      plot.title = element_text(
        face = "bold", size = scale_size(5), hjust = 0,
        margin = margin(b = scale_size(5) * margin_factor)
      ),
      # plot subtitle, under the title
      plot.subtitle = element_text(
        size = scale_size(4), hjust = 0,
        margin = margin(b = scale_size(4) * margin_factor)
      ),
      # plot caption, bottom of the plot
      plot.caption = element_text(size = scale_size(1), hjust = 1),
      # plot legend title and text
      legend.title = element_text(size = scale_size(3), hjust = 0),
      legend.text = element_text(size = scale_size(1), hjust = 0),
      # x, y axis title
      axis.title = element_text(size = scale_size(3)),
      # x, y axis tick label
      axis.text = element_text(size = scale_size(1)),
      # facet title
      strip.text = element_text(
        size = scale_size(3),
        margin = margin(
          b = scale_size(3) * margin_factor,
          t = scale_size(3) * margin_factor
        )
      ),

      ##################################
      ### line
      ##################################
      # axis
      axis.line = element_line(size = lpt(base_lw))
    ) %+replace%
    # allow modification
    ggplot2::theme(...)
}
