#' select colors from `RColorBrewer` package presets
#'
#' @param name presets name
#' @param n number of colors
#' @param ... other arguments of `RColorBrewer::brewer.pal`
#'
#' @return colors
#' @export
#'
#' @examples brewer_colors("Blues", 5)
brewer_colors <- function(name, n = 3, ...) {
  RColorBrewer::brewer.pal(n, name, ...)
}



#' generate gradient colors
#'
#' @param x colors
#' @param n number of colors to output
#'
#' @return gradient colors
#' @export
#'
#' @examples gradient_colors(c("blue", "red"), 10)
gradient_colors <- function(x, n) {
  grDevices::colorRampPalette(x)(n)
}


#' show colors
#'
#' @param x color values
#' @param show_name use vector names as label
#' @param ncol color number of each row
#'
#' @return ggplot object
#' @export
#'
#' @examples plot_colors(gradient_color(c("blue", "red"), 10))
plot_colors <- function(x, ncol = 10, show_name = TRUE) {
  df <- as_tibble(x, rownames = "key") %>%
    dplyr::mutate(row = floor((seq_len(dplyr::n()) - 1) / ncol) + 1) %>%
    dplyr::mutate(col = seq_len(dplyr::n()), .by = row)
  p <- ggplot(df) +
    geom_rect(aes(
      xmin = col, xmax = col + 1,
      ymin = row, ymax = row + 1,
      fill = .data[["value"]]
    )) +
    scale_y_reverse() +
    scale_fill_manual(
      values = dplyr::pull(df, .data[["value"]], .data[["value"]])
    ) +
    coord_fixed(ratio = 1) +
    theme_pl0()
  if (show_name == TRUE) {
    p <- p + geom_text(aes(
      x = col + 0.5, y = row + 0.5,
      label = .data[["key"]]
    ), size = tpt(5))
  } else {
    p <- p + geom_text(aes(
      x = col + 0.5, y = row + 0.5,
      label = .data[["value"]]
    ), size = tpt(5))
  }

  return(p)
}
