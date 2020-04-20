#' Special layer function for correlation link plot
#' @description A set of custom layer functions that quickly add
#' layers of curves, nodes, and labels.
#' @param mapping aesthetic mappings parameters.
#' @param data a data frame.
#' @param geom one of "text", "label" or "image".
#' @param nudge_x horizontal adjustment to nudge labels by.
#' @param layout.params parameters passing to layout function.
#' @param is.start NULL (default), TRUE or FALSE.
#' @param ... extra parameters passing to layer function.
#' @return a ggplot layer.
#' @rdname geom_links
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_links <- function(mapping = NULL,
                       data,
                       layout.params = list(),
                       ...)
{
  structure(.Data = list(mapping = mapping, data = data, layout.params = layout.params,
                         params = list(...)), class = "geom_links")
}

#' @rdname geom_links
#' @export
geom_links_label <- function(mapping = NULL,
                             nudge_x = 0.1,
                             geom = "text",
                             is.start = TRUE,
                             ...)
{
  structure(.Data = list(mapping = mapping, nudge_x = nudge_x, geom = geom,
                         is.start = is.start, params = list(...)),
            class = "geom_links_label")
}
