#' save plot, support save into a blank canvas
#'
#' @param plot ggplot object
#' @param filename filename
#' @param width plot width
#' @param height plot height
#' @param units units, 'in' for inch as default. Can be 'in', 'cm'
#' @param canvas `NULL` as default, pass character to use
#' built-in canvas ('A4', 'A4v'), or pass a numeric vector in 'c(width, heigh)'
#' form
#' @param canvas_pos_x from 0 to 1, the horizontal position of plot in canvas
#' @param canvas_pos_y from 0 to 1, the vertical position of plot in canvas
#' @param ... other arguments from `ggsave`
#'
#' @return no return value
#' @export
#'
pl_save <- function(plot, filename, width, height, units = "in", canvas = NULL,
                    canvas_pos_x = 0.5, canvas_pos_y = 0.1, ...) {
  if (units == "inch") units <- "in"

  if (is.null(canvas)) {
    # without canvas
    ggplot2::ggsave(filename, plot,
      width = width,
      heigh = height, units = units, ...
    )
  } else {
    if (length(canvas) == 1 &&
      (length(intersect(canvas, names(canvas_size[[1]]))) > 0)) {
      # use bulit-in canvas
      canvas_width <- canvas_size[[units]][[canvas]][1]
      canvas_height <- canvas_size[[units]][[canvas]][2]
    } else if (length(canvas) == 2 && is.numeric(canvas)) {
      # use custom canvas
      canvas_width <- canvas[1]
      canvas_height <- canvas[2]
    } else {
      stop("unsupported canvas type!")
    }
    blank_margin <- c(
      (canvas_height - height) * canvas_pos_y,
      (canvas_width - width) * (1 - canvas_pos_x),
      (canvas_height - height) * (1 - canvas_pos_y),
      (canvas_width - width) * canvas_pos_x
    )
    if (any(blank_margin < 0)) {
      stop("plot size larger than canvas!")
    }

    plot <- plot + theme(plot.margin = margin(blank_margin, unit = units))
    ggplot2::ggsave(filename, plot,
      width = canvas_width,
      heigh = canvas_height, units = units, ...
    )
  }
}


# used by test
pl_svg <- function(plot, file, title = "") {
  pl_save(plot, file, width = 4, height = 3)
}
