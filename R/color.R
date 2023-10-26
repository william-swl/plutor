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
  rev(RColorBrewer::brewer.pal(n, name, ...))
}


#' select colors from `ggsci` package presets
#'
#' @param name presets name
#' @param n number of colors
#' @param alpha alpha
#'
#' @return colors
#' @export
#'
#' @examples sci_colors("npg", 5)
sci_colors <- function(name, n = 3, alpha = 1) {
  if (name == "npg") {
    res <- ggsci::pal_npg("nrc", alpha)(n)
  } else if (name == "nejm") {
    res <- ggsci::pal_nejm("default", alpha)(n)
  } else if (name == "lancet") {
    res <- ggsci::pal_lancet("lanonc", alpha)(n)
  } else if (name == "d3") {
    res <- ggsci::pal_d3("category20", alpha)(n)
  }

  return(res)
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


#' plot colors
#'
#' @param x color values
#' @param show_name use vector names as label, `FALSE` to show the color value
#' @param ncol color number of each row
#'
#' @return ggplot object
#' @export
#'
#' @examples plot_colors(gradient_colors(c("blue", "red"), 10))
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



#' assign colors by a column in a tibble, for the convenience to
#' use `scale_color_identity()`
#'
#' @param df tibble
#' @param by assign colors according to this column
#' @param colors a vector of color values
#' @param na if colors are not enough, fill na values
#'
#' @return tibble
#' @export
#'
#' @examples assign_colors(mini_diamond, cut, colors = sci_colors("nejm", 8))
assign_colors <- function(df, by, colors = sci_colors("npg", 10),
                          na = "#F5F5F5") {
  by <- rlang::enquo(by)
  by_col <- dplyr::pull(df, {{ by }})
  # factor
  v_names <- levels(by_col)
  # non-factor
  if (is.null(v_names)) {
    v_names <- unique(sort(by_col))
  }

  if (length(colors) < length(v_names)) {
    warning(str_glue("input colors are not enough, fill na items by {na}"))
    colors <- c(colors, rep(na, times = length(v_names) - length(colors)))
  }

  names(colors) <- v_names

  assigned_colors <- colors[by_col]
  res <- df %>% dplyr::mutate(assigned_colors = assigned_colors)
  return(res)
}
