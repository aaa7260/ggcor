#' Special layer function for correlation link plot
#' @description A set of custom layer functions that quickly add
#' layers of curves, nodes, and labels.
#' @param mapping aesthetic mappings parameters.
#' @param  curvature a numeric value giving the amount of curvature.
#' @param ... extra parameters passing to layer function.
#' @return a ggplot layer.
#' @importFrom ggplot2 aes_string geom_curve geom_point geom_text
#' @importFrom dplyr filter
#' @rdname geom_links2
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_links2 <- function(mapping = NULL,
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

#' @rdname geom_links2
#' @export
geom_links_label <- function(mapping = NULL, geom = "text", ...)
{
  geom <- match.arg(geom, c("text", "label", "image"))
  mapping <- if(geom == "image") {
    aes_modify(aes_string(x = "x", y = "y"), mapping)
  } else {
    aes_modify(aes_string(x = "x", y = "y", label = "label"), mapping)
  }
  params <- modifyList(list(mapping = mapping, inherit.aes = FALSE),
                       list(...))
  structure(.Data = params, geom = geom, class = "geom_links_label")
}
