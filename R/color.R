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
