#' PositionFloatyPL
#' @export
#'
PositionFloatyPL <- ggproto("PositionFloatyPL", ggplot2::Position,
  required_aes = c("x", "y"),
  setup_params = function(self, data) {
    params <- list(
      float = self$float,
      cycle = self$cycle
    )
    return(params)
  },
  compute_layer = function(data, params, panel) {
    y_range <- panel$panel_scales_y[[1]]$range$range
    float_step <- (y_range[2] - y_range[1]) * params$float
    float_value <- baizer::broadcast_vector(
      float_step * seq(0, params$cycle - 1),
      n = nrow(data)
    )
    data <- data %>% dplyr::mutate(y = y + float_value)
    return(data)
  }
)
#' a new Position object to create float y position
#'
#' @param float float range, a ratio according to the whole panel height
#' @param cycle float cycle
#'
#' @return Position object
#' @export
#'
position_floatyPL <- function(float = -0.05, cycle = 2) { # nolint
  ggproto(NULL, PositionFloatyPL, float = float, cycle = cycle)
}

#' PositionFloatxPL
#' @export
#'
PositionFloatxPL <- ggproto("PositionFloatxPL", ggplot2::Position,
  required_aes = c("x", "y"),
  setup_params = function(self, data) {
    params <- list(
      float = self$float,
      cycle = self$cycle
    )
    return(params)
  },
  compute_layer = function(data, params, panel) {
    x_range <- panel$panel_scales_x[[1]]$range$range
    float_step <- (x_range[2] - x_range[1]) * params$float
    float_value <- baizer::broadcast_vector(
      float_step * seq(0, params$cycle - 1),
      n = nrow(data)
    )
    data <- data %>% dplyr::mutate(x = x + float_value)
    return(data)
  }
)

#' a new Position object to create float x position
#'
#' @param float float range, a ratio according to the whole panel height
#' @param cycle float cycle
#'
#' @return Position object
#' @export
#'
position_floatxPL <- function(float = -0.05, cycle = 2) { # nolint
  ggproto(NULL, PositionFloatxPL, float = float, cycle = cycle)
}
