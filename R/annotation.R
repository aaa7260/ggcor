#' Annotation for correlation matrix plot
#' @title Annotation for correlation matrix plot
#' @param index one of "all", "row" or "col".
#' @param colour,color colour of segments.
#' @param size width of segments.
#' @param linetype line type of segments.
#' @return anno_tree object.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
anno_tree <- function(index = "all",
                      colour = NULL,
                      size = NULL,
                      linetype = NULL,
                      color) {
  if(!missing(color))
    colour <- color
  index <- match.arg(index, c("all", "row", "col"))
  structure(.Data = list(index = index, colour = colour,
                         size = size, linetype = linetype),
            class = "anno_tree")
}
