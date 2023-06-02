#' set colors for echarts plot
#'
#' @param e echarts4r object
#' @param x color character vector
#' @param rep replicate the colors if needed
#' @param default default color
#'
#' @export
#'
#' @examples
#'
#' mini_diamond %>%
#'   dplyr::group_by(cut) %>%
#'   e_charts(x, width = 500, height = 500) %>%
#'   e_scatter(y, symbol_size = 10) %>%
#'   pe_color(c("red", "yellow"), default = "black")
#'
pe_color <- function(e, x, rep = FALSE, default = "transparent") {
  if (is(e) != "echarts4r") {
    stop("e should be an echarts4r object!")
  }

  all_series <- e[["x"]][["opts"]][["series"]]

  if (length(all_series) == 1) {
    # set colors in series, e.g. pie plot
    names_items <- all_series[[1]][["data"]] %>% map_chr(~ .x[["name"]])
  } else {
    # set colors for series, e.g. grouped scatter plot
    names_items <- all_series %>% map_chr(~ .x[["name"]])
  }

  if (is.null(names_items)) {
    print("No items to set color!")
    return(e)
  }


  colors_items <- rep(default, length(names_items))
  names(colors_items) <- names_items

  # adapt from input
  if (!is.null(names(x))) {
    colors_items[names(x)] <- x
  } else {
    colors_items[seq_along(x)] <- x
    # fill colors
    if (rep == TRUE) {
      colors_items <- rep(
        x, floor(length(colors_items) / length(x))
      )[seq_along(colors_items)]
    }
  }

  # trans to json string
  color_string <- list(color = colors_items) %>%
    toJSON() %>%
    as.character()

  e <- e_theme_custom(e, color_string)

  return(e)
}
