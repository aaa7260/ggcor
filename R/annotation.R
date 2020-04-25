#' Annotation for correlation matrix plot
#' @title Annotation for correlation matrix plot
#' @param index one of "all", "row" or "col".
#' @param bcols branch colours.
#' @param row.height,col.height height of row/columns tree.
#' @param colour,color colour of segments.
#' @param size width of segments.
#' @param linetype line type of segments.
#' @return anno_tree object.
#' @rdname anno_tree
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
anno_tree <- function(index = "all",
                      bcols = NULL,
                      row.height = NULL,
                      col.height = NULL,
                      colour = NULL,
                      size = NULL,
                      linetype = NULL,
                      color) {
  if(!missing(color))
    colour <- color
  index <- match.arg(index, c("all", "row", "col"))
  structure(.Data = list(index = index, bcols = bcols, row.height = row.height,
                         col.height = col.height, colour = colour, size = size,
                         linetype = linetype), class = "anno_tree")
}

#' Special annotation function for correlation link plot
#' @description a set of custom layer functions that quickly add
#' layers of curves, nodes, and labels.
#' @param mapping aesthetic mappings parameters.
#' @param data a data frame.
#' @param geom one of "text", "label" or "image".
#' @param nudge_x horizontal adjustment to nudge labels by.
#' @param layout.params parameters passing to layout function.
#' @param is.start NULL (default), TRUE or FALSE.
#' @param ... extra parameters passing to layer function.
#' @return a ggplot layer.
#' @rdname anno_link
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
anno_link <- function(mapping = NULL,
                      data,
                      start.var = NULL,
                      end.var = NULL,
                      stretch = FALSE,
                      ...)
{
  start.var <- rlang::enquo(start.var)
  end.var <- rlang::enquo(end.var)
  structure(.Data = list(mapping = mapping, data = data, start.var = start.var, end.var = end.var,
                         stretch = stretch, params = list(...)), class = "anno_link")
}

#' @rdname anno_link
#' @export
anno_link_label <- function(mapping = NULL,
                            nudge_x = 0.1,
                            geom = "text",
                            is.start = TRUE,
                            ...)
{
  structure(.Data = list(mapping = mapping, nudge_x = nudge_x, geom = geom,
                         is.start = is.start, params = list(...)),
            class = "anno_link_label")
}

#' Square annotation
#' @title Square annotation
#' @description Draw the cluster square mark on the correlation matrix plot.
#' @param k an integer scalar or vector with the desired number of groups.
#' @param fill NA (default) or the fill colour of square.
#' @param colour,color the colour of square boder.
#' @param size size of square boder line.
#' @return square layer.
#' @rdname anno_hc_rect
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
anno_hc_rect <- function(k = 2,
                         fill = NA,
                         colour = "black",
                         size = 2,
                         color)
{
  if(!missing(color))
    colour <- color
  structure(.Data = list(k = k, fill = fill, colour = colour,
                         size = size), class = "anno_hc_rect")
}
