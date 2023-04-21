#' select most distant colors among a color spectrum
#'
#' @param n number of colors
#' @param color_set selection from
#' @param type color_set type, 'munsell' as default
#'
#' @return hex value of selected colors
#' @export
#'
#' @examples select_color(5)
select_color <- function(n, color_set = base_color, type = "munsell") {
  res <- baizer::fps_vector(color_set, n)
  if (type == "munsell") {
    return(munsell::mnsl(res))
  } else {
    return(res)
  }
}


#' generate gradient colors
#'
#' @param x colors
#' @param n number of colors to output
#'
#' @return gradient colors
#' @export
#'
#' @examples gradient_color(c("blue", "red"), 10)
gradient_color <- function(x, n) {
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
#' @examples plot_col(gradient_color(c("blue", "red"), 10))
plot_col <- function(x, ncol = 10, show_name = TRUE) {
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
