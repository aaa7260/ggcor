#' Remove Axis Elements.
#' @description A simple wrapper of the `theme()` function to quickly 
#'     remove axis elements.
#' @param index the index ("all", "x", "y") of axis will be removed.
#' @return The theme.
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @export
remove_axis <- function(index = c("all", "x", "y")) {
  index <- match.arg(index)
  thm_mv_x <- ggplot2::theme(
    axis.title.x = element_blank(),
    axis.title.x.top = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.text.x = element_blank(),
    axis.text.x.top = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.x.top = element_blank(),
    axis.line.x.bottom = element_blank()
  )
  thm_mv_y <- ggplot2::theme(
    axis.title.y = element_blank(),
    axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.y = element_blank(),
    axis.text.y.left = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.line.y = element_blank(),
    axis.line.y.left = element_blank(),
    axis.line.y.right = element_blank()
  )
  if(index == "all") {
    thm_mv_x + thm_mv_y
  } else if (index == "x"){
    thm_mv_x
  } else {
    thm_mv_y
  }
}