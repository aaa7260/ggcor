#' @export
extra_params <- function(  group.label = text_params(),
                           group.point = point_params(fill = "blue"),
                           corrmat.point = point_params(fill = "grey60"),
                           link.params = link_params())
{
  if(!inherits(group.label, "text_params") && !is.null(group.label))
    stop("Element group.point.text must be a text_params object or NULL.", call. = FALSE)
  if(!inherits(group.point, "point_params") && is.null(group.point))
    stop("Element group.point must be a point_params object or NULL.", call. = FALSE)
  if(!inherits(corrmat.point, "point_params") && is.null(corrmat.point))
    stop("Element corrmat.point must be a point_params object or NULL.", call. = FALSE)
  if(!inherits(link.params, "link_params"))
    stop("Element link.params must be a link_params object.", call. = FALSE)
  structure(list(group.label = group.label,
                 group.point = group.point,
                 corrmat.point = corrmat.point,
                 link.params = link.params
  ),
  class = "extra_params")
}

#' @export
text_params <- function(colour = "black",
                        size = 3.88,
                        angle = 0,
                        hjust = NULL,
                        vjust = 0.5,
                        alpha = NA,
                        family = "",
                        fontface = 1,
                        color = NULL)
{
  if (!is.null(color))
    colour <- color
  structure(list(colour = colour, size = size, angle = angle, hjust = hjust,
                 vjust = vjust, alpha = alpha, family = family, fontface = fontface),
            class = "text_params")
}

#' @export
point_params <- function(alpha = NA,
                         colour = "black",
                         fill = NA,
                         shape = 21,
                         size = 1,
                         stroke = 0.5,
                         color = NULL)
{
  if (!is.null(color))
    colour <- color
  structure(list(alpha = alpha, colour = colour, fill = fill,
                 shape = shape, size = size, stroke = stroke),
            class = "point_params")
}

#' @export
link_params <- function(  corrmat.point.hjust = NULL,
                          corrmat.point.vjust = NULL,
                          group.point.hjust    = NULL,
                          group.point.vjust    = NULL)
{
  structure(list(corrmat.point.hjust = corrmat.point.hjust,
                 corrmat.point.vjust = corrmat.point.vjust,
                 group.point.hjust = group.point.hjust,
                 group.point.vjust = group.point.vjust),
            class = "link_params")
}