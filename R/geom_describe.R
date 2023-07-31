#' @export
StatDescribe <- ggproto("StatDescribe", Stat,
  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params,
      main_is_orthogonal = TRUE,
      group_has_equal = TRUE,
      main_is_optional = TRUE
    )
    return(params)
  },
  compute_group = function(data, scales, na.rm, flipped_aes,
                           center_func, low_func, high_func) {
    data <- flip_data(data, flipped_aes)
    data <- data %>%
      dplyr::summarise(
        x = mean(x, na.rm = TRUE),
        ylow = low_func(y, na.rm = na.rm),
        yhigh = high_func(y, na.rm = na.rm),
        y = center_func(y, na.rm = na.rm)
      )
    data$flipped_aes <- flipped_aes
    data <- flip_data(data, flipped_aes)
    return(data)
  },
  required_aes = c("x", "y")
)


#' @export
GeomDescribe <- ggproto("GeomDescribe", Geom,
  required_aes = c("y|x", "ylow|xlow", "yhigh|xhigh"),
  default_aes = aes(
    colour = "black", linewidth = .5, fill = NA, alpha = NA,
    size = 2, linetype = 1, shape = 19, stroke = 1
  ),
  setup_params = StatDescribe$setup_params,
  draw_group = function(data, panel_params, coord, flipped_aes,
                        show_error, center_width,
                        error_width, center_symbol, ...) {
    data <- flip_data(data, flipped_aes)
    center <- flip_data(data, flipped_aes)
    center_bar <- dplyr::mutate(data,
      x = x - center_width / 2,
      xend = x + center_width, y = y, yend = y
    ) %>%
      flip_data(flipped_aes)
    error <- dplyr::bind_rows(
      dplyr::mutate(data, xend = x, yend = ylow),
      dplyr::mutate(data, xend = x, yend = yhigh),
      dplyr::mutate(data,
        x = x - error_width / 2,
        xend = x + error_width, y = ylow, yend = ylow
      ),
      dplyr::mutate(data,
        x = x - error_width / 2,
        xend = x + error_width, y = yhigh, yend = yhigh
      )
    )
    error <- flip_data(error, flipped_aes)
    grid::gList(
      if (center_symbol == "point") {
        GeomPoint$draw_panel(data, panel_params, coord, ...)
      },
      if (center_symbol == "bar") {
        GeomSegment$draw_panel(center_bar, panel_params, coord, ...)
      },
      if (show_error == TRUE) {
        GeomSegment$draw_panel(error, panel_params, coord, ...)
      }
    )
  }
)

#' Description values plot
#'
#' The describe geom is used to create description values plot, including
#' center symbol and error symbol. The center symbol can be mean, median or
#' other custom functions, the error symbol can be sd, quantile or other custom
#' functions.
#'
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to `layer()`. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `colour = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#' @param show_error show error symbol
#' @param center_symbol one of `point, bar`
#' @param center_width if `center_symbol='bar'`, the width of the bar
#' @param error_width the width of the error bar
#' @param center_func the center function, `mean` as default
#' @param low_func the low error function, `mean` minus `sd` as default
#' @param high_func the high error function, `mean` plus `sd` as default
#'
#' @export
#'
#' @examples
#'
#' mini_diamond %>% ggplot(aes(x = cut, y = price)) +
#'   geom_point() +
#'   geom_describe()
#'
geom_describe <- function(mapping = NULL, data = NULL,
                          stat = "describe", position = "identity",
                          na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,
                          show_error = TRUE, center_symbol = "bar",
                          center_width = 0.4, error_width = 0.2,
                          center_func = mean,
                          low_func = function(x, na.rm) {
                            mean(x, na.rm = na.rm) - sd(x, na.rm = na.rm)
                          },
                          high_func = function(x, na.rm) {
                            mean(x, na.rm = na.rm) + sd(x, na.rm = na.rm)
                          },
                          ...) {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomDescribe,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, show_error = show_error,
      center_width = center_width, error_width = error_width,
      center_func = center_func, center_symbol = center_symbol,
      low_func = low_func, high_func = high_func, ...
    )
  )
}

#' @export
geom2trace.GeomDescribe <- function(data, params, plot) { # nolint
}