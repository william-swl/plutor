set_sec_axis <- function(sec.axis, scale) {
  if (!inherits(sec.axis, "waiver")) {
    if (inherits(sec.axis, "formula")) {
      sec.axis <- ggplot2::sec_axis(sec.axis)
    }
    if (!inherits(sec.axis, "AxisSecondary")) {
      cli::cli_abort("Secondary axes must be specified using {.fn sec_axis}")
    }
    scale$secondary.axis <- sec.axis
  }
  return(scale)
}


#' A variant of `scale_y_log10()` to show axis minor breaks
#' and better axis labels
#' @inheritParams ggplot2::scale_y_log10
#' @param expand use `expansion()` to dismiss the blank between y axis low limit
#' and x axis
#' @param oob use `scales::oob_keep` instead of `scales::oob_censor`, which
#' will always consider the data points out of the limits
#' @param show_minor_breaks show minor breaks or not
#'
#' @return scale object
#'
#' @export
#'
scale_y_log10_pl <- function(name = waiver(), breaks = NULL,
                             minor_breaks = NULL, n.breaks = NULL,
                             labels = NULL, limits = NULL,
                             expand = ggplot2::expansion(),
                             oob = scales::oob_keep,
                             na.value = NA_real_, trans = scales::log10_trans(),
                             guide = ggh4x::guide_axis_minor(),
                             position = "left",
                             sec.axis = waiver(), show_minor_breaks = TRUE) {
  if (is.null(limits)) {
    y_lim <- c(10^-10, 10^10)
  } else {
    y_lim <- limits
  }
  if (is.null(breaks)) {
    breaks <- 10^c(floor(log10(y_lim[1])):ceiling(log10(y_lim[2])))
  }
  if (is.null(labels)) {
    labels <- scales::trans_format("log10", scales::math_format(~ 10^.x)) # nolint
  }
  if (is.null(minor_breaks) && show_minor_breaks) {
    minor_breaks <- baizer::adjacent_div(
      10^c(floor(log10(y_lim[1])):ceiling(log10(y_lim[2]))),
      .unique = TRUE
    )
  }

  sc <- ggplot2::continuous_scale(
    aesthetics = c(
      "y", "ymin", "ymax", "yend", "yintercept",
      "ymin_final", "ymax_final", "lower", "middle", "upper", "y0"
    ),
    scale_name = "scale_y_log10_pl",
    palette = identity,
    name = name, breaks = breaks, n.breaks = n.breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = guide, position = position, super = ScaleContinuousPosition
  )

  set_sec_axis(sec.axis, sc)
}





#' A variant of `scale_y_continuous()` to show axis minor breaks
#'
#' @inheritParams ggplot2::scale_y_continuous
#' @param expand use `expansion()` to dismiss the blank between y axis low limit
#' and x axis
#' @param oob use `scales::oob_keep` instead of `scales::oob_censor`, which
#' will always consider the data points out of the limits
#' @param show_minor_breaks show minor breaks or not
#' @param minor_break_step the step of minor breaks
#'
#' @return scale object
#'
#' @export
#'
scale_y_continuous_pl <- function(name = waiver(), breaks = waiver(),
                                  minor_breaks = NULL, n.breaks = NULL,
                                  labels = waiver(), limits = NULL,
                                  expand = ggplot2::expansion(),
                                  oob = scales::oob_keep,
                                  na.value = NA_real_, trans = "identity",
                                  guide = ggh4x::guide_axis_minor(),
                                  position = "left",
                                  sec.axis = waiver(), show_minor_breaks = TRUE,
                                  minor_break_step = NULL) {
  if (is.null(limits)) {
    y_lim <- c(10^-10, 10^10)
  } else {
    y_lim <- limits
  }
  if (is.null(minor_breaks) &&
    show_minor_breaks && !is.null(minor_break_step)) {
    start <- y_lim[1] - y_lim[1] %% minor_break_step
    end <- y_lim[2] - y_lim[2] %% minor_break_step + minor_break_step
    minor_breaks <- seq(start, end, minor_break_step)
  }

  sc <- ggplot2::continuous_scale(
    aesthetics = c(
      "y", "ymin", "ymax", "yend", "yintercept",
      "ymin_final", "ymax_final", "lower", "middle", "upper", "y0"
    ),
    scale_name = "scale_y_continuous_pl",
    palette = identity,
    name = name, breaks = breaks, n.breaks = n.breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = guide, position = position, super = ScaleContinuousPosition
  )

  set_sec_axis(sec.axis, sc)
}


#' A variant of `scale_x_log10()` to show axis minor breaks and
#' better axis labels
#'
#' @inheritParams ggplot2::scale_x_log10
#' @param expand use `expansion()` to dismiss the blank between x axis low limit
#' and y axis
#' @param oob use `scales::oob_keep` instead of `scales::oob_censor`, which
#' will always consider the data points out of the limits
#' @param show_minor_breaks show minor breaks or not
#'
#' @return scale object
#'
#' @export
#'
scale_x_log10_pl <- function(name = waiver(), breaks = NULL,
                             minor_breaks = NULL, n.breaks = NULL,
                             labels = NULL, limits = NULL,
                             expand = ggplot2::expansion(),
                             oob = scales::oob_keep,
                             na.value = NA_real_, trans = scales::log10_trans(),
                             guide = ggh4x::guide_axis_minor(),
                             position = "bottom",
                             sec.axis = waiver(), show_minor_breaks = TRUE) {
  if (is.null(limits)) {
    x_lim <- c(10^-10, 10^10)
  } else {
    x_lim <- limits
  }
  if (is.null(breaks)) {
    breaks <- 10^c(floor(log10(x_lim[1])):ceiling(log10(x_lim[2])))
  }
  if (is.null(labels)) {
    labels <- scales::trans_format("log10", scales::math_format(~ 10^.x)) # nolint
  }
  if (is.null(minor_breaks) && show_minor_breaks) {
    minor_breaks <- baizer::adjacent_div(
      10^c(floor(log10(x_lim[1])):ceiling(log10(x_lim[2]))),
      .unique = TRUE
    )
  }

  sc <- ggplot2::continuous_scale(
    aesthetics = c(
      "x", "xmin", "xmax", "xend", "xintercept",
      "xmin_final", "xmax_final", "lower", "middle", "upper", "x0"
    ),
    scale_name = "scale_x_log10_pl",
    palette = identity,
    name = name, breaks = breaks, n.breaks = n.breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = guide, position = position, super = ScaleContinuousPosition
  )

  set_sec_axis(sec.axis, sc)
}


#' A variant of `scale_x_continuous()` to show axis minor breaks
#'
#' @inheritParams ggplot2::scale_x_continuous
#' @param expand use `expansion()` to dismiss the blank between x axis low limit
#' and y axis
#' @param oob use `scales::oob_keep` instead of `scales::oob_censor`, which
#' will always consider the data points out of the limits
#' @param show_minor_breaks show minor breaks or not
#' @param minor_break_step the step of minor breaks
#'
#' @return scale object
#'
#' @export
#'
scale_x_continuous_pl <- function(name = waiver(), breaks = waiver(),
                                  minor_breaks = NULL, n.breaks = NULL,
                                  labels = waiver(), limits = NULL,
                                  expand = ggplot2::expansion(),
                                  oob = scales::oob_keep,
                                  na.value = NA_real_, trans = "identity",
                                  guide = ggh4x::guide_axis_minor(),
                                  position = "bottom",
                                  sec.axis = waiver(), show_minor_breaks = TRUE,
                                  minor_break_step = NULL) {
  if (is.null(limits)) {
    x_lim <- c(10^-10, 10^10)
  } else {
    x_lim <- limits
  }
  if (is.null(minor_breaks) &&
    show_minor_breaks && !is.null(minor_break_step)) {
    start <- x_lim[1] - x_lim[1] %% minor_break_step
    end <- x_lim[2] - x_lim[2] %% minor_break_step + minor_break_step
    minor_breaks <- seq(start, end, minor_break_step)
  }

  sc <- ggplot2::continuous_scale(
    aesthetics = c(
      "x", "xmin", "xmax", "xend", "xintercept",
      "xmin_final", "xmax_final", "lower", "middle", "upper", "x0"
    ),
    scale_name = "scale_x_continuous_pl",
    palette = identity,
    name = name, breaks = breaks, n.breaks = n.breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = guide, position = position, super = ScaleContinuousPosition
  )

  set_sec_axis(sec.axis, sc)
}
