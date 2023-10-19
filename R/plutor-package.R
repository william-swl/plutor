#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom baizer alias_arg
#' @importFrom dplyr bind_rows
#' @importFrom dplyr full_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom ggplot2 %+replace%
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 flip_data
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 GeomPoint
#' @importFrom ggplot2 GeomSegment
#' @importFrom ggplot2 GeomText
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 has_flipped_aes
#' @importFrom ggplot2 is.ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 margin
#' @importFrom ggplot2 Position
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_y_log10
#' @importFrom ggplot2 scale_y_reverse
#' @importFrom ggplot2 ScaleContinuousPosition
#' @importFrom ggplot2 Stat
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_set
#' @importFrom ggplot2 waiver
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom grid unit
#' @importFrom methods is
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr map_dbl
#' @importFrom purrr map_lgl
#' @importFrom purrr map2
#' @importFrom repr repr_option_defaults
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom rlang set_names
#' @importFrom stats sd
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_glue
#' @importFrom stringr str_replace
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom utils globalVariables
## usethis namespace: end
NULL



layer <- function(...) ggplot2::layer(...)

.onLoad <- function(...) {
  # https://github.com/teunbrand/ggh4x/blob/b80894538330ff259179159c3c39b444d7c627d4/R/themes.R #nolint
  ggplot2::register_theme_elements(
    ggh4x.facet.nestline = element_blank(),
    ggh4x.axis.nestline = element_line(),
    ggh4x.axis.nestline.x = element_line(),
    ggh4x.axis.nestline.y = element_line(),
    ggh4x.axis.nesttext.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    ggh4x.axis.nesttext.y = element_text(),
    ggh4x.axis.ticks.length.minor = ggplot2::rel(2 / 3),
    ggh4x.axis.ticks.length.mini = ggplot2::rel(1 / 3),
    element_tree = list(
      ggh4x.facet.nestline = ggplot2::el_def("element_line", "line"),
      ggh4x.axis.nestline = ggplot2::el_def("element_line", "axis.ticks"),
      ggh4x.axis.nestline.x = ggplot2::el_def(
        "element_line",
        "ggh4x.axis.nestline"
      ),
      ggh4x.axis.nestline.y = ggplot2::el_def(
        "element_line",
        "ggh4x.axis.nestline"
      ),
      ggh4x.axis.nesttext.x = ggplot2::el_def("element_text", "axis.text.x"),
      ggh4x.axis.nesttext.y = ggplot2::el_def("element_text", "axis.text.y"),
      ggh4x.axis.ticks.length.minor = ggplot2::el_def(c("rel")),
      ggh4x.axis.ticks.length.mini = ggplot2::el_def(c("rel"))
    )
  )
}



repr_option_defaults <- repr_option_defaults
