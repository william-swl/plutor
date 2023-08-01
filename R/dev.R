#' revert the position scale transformation
#'
#' @param s ScaleContinuousPosition object, e.g. scales$y in compute_group()
#'
#' @return function
#' @export
#'
revert_pos_scale <- function(s) {
  if (!is(s, "ScaleContinuousPosition")) {
    stop("Please input ScaleContinuousPosition object,
         e.g. scales$y in compute_group()")
  }
  trans <- s$trans$name
  if (trans == "log-10") {
    func <- function(i) 10^i
  } else if (trans == "log-2") {
    func <- function(i) 2^i
  } else if (trans %in% c("identity", "reverse")) {
    func <- function(i) i
  } else {
    stop(str_glue("unknown transformation: {trans}"))
  }

  return(func)
}


#' perform the position scale transformation
#'
#' @param s ScaleContinuousPosition object, e.g. scales$y in compute_group()
#'
#' @return function
#' @export
#'
trans_pos_scale <- function(s) {
  if (!is(s, "ScaleContinuousPosition")) {
    stop("Please input ScaleContinuousPosition object,
         e.g. scales$y in compute_group()")
  }
  func <- s$trans$transform

  return(func)
}
