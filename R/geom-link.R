#' Special layer function for correlation link plot
#' @description A set of custom layer functions that quickly add
#' layers of curves, nodes, and labels.
#' @param mapping aesthetic mappings parameters.
#' @param data a data frame.
#' @param geom one of "text", "label" or "image".
#' @param nudge_x horizontal adjustment to nudge labels by.
#' @param curvature a numeric value giving the amount of curvature.
#' @param layout one of "triangle" or "parallel".
#' @param layout.params parameters passing to layout function.
#' @param is.start NULL (default), TRUE or FALSE.
#' @param ... extra parameters passing to layer function.
#' @return a ggplot layer.
#' @importFrom ggplot2 aes_string geom_curve geom_point geom_text
#' @importFrom dplyr filter
#' @rdname geom_links
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_links <- function(mapping = NULL,
                       data,
                       curvature = 0,
                       layout = NULL,
                       layout.params = list(),
                       ...)
{
  mapping <- aes_modify(
    aes_string(x = "x", y = "y", xend = "xend", yend = "yend"), mapping
  )
  layout.params <- modifyList(list(data = data), layout.params)
  params <- list(mapping = mapping, curvature = curvature,
                 inherit.aes = FALSE)
  params <- modifyList(params, list(...))
  structure(.Data = list(params = params, layout = layout, layout.params = layout.params),
            class = "geom_links")
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
