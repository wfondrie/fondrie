#' Emulate ggplot2 colors
#'
#' @description
#' Creates a vector of n colors spread evenly across hues, thereby emulating
#' the default color scheme of ggplot2.
#'
#' @param n Number of colors
#' @return A vector of length n containing hexidecimal color codes.
#' @examples
#' ggcolor(2)
#' @references
#' http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
#' @export

ggcolor <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
