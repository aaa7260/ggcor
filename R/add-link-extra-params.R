#' @export
extra_params <- function(spec.label = text_params(),
                         spec.point = point_params(fill = "blue"),
                         env.point = point_params(fill = "grey60"),
                         link.params = link_params())
{
  if(!inherits(spec.label, "text_params") && !is.null(spec.label))
    stop("Element 'spec.label' must be a text_params object or NULL.", call. = FALSE)
  if(!inherits(spec.point, "point_params") && is.null(spec.point))
    stop("Element 'spec.point' must be a point_params object or NULL.", call. = FALSE)
  if(!inherits(env.point, "point_params") && is.null(env.point))
    stop("Element 'env.point' must be a point_params object or NULL.", call. = FALSE)
  if(!inherits(link.params, "link_params"))
    stop("Element 'link.params' must be a link_params object.", call. = FALSE)
  structure(list(spec.label = spec.label,
                 spec.point = spec.point,
                 env.point = env.point,
                 link.params = link.params
  ),
  class = "extra_params")
}

#' @export
text_params <- function(colour = "black", size = 3.88, angle = 0, hjust = NULL,
                        vjust = 0.5, alpha = NA, family = "", fontface = 1, color = NULL)
{
  if (!is.null(color)) colour <- color
  structure(list(colour = colour, size = size, angle = angle, hjust = hjust,
                 vjust = vjust, alpha = alpha, family = family, fontface = fontface),
            class = "text_params")
}

#' @export
point_params <- function(alpha = NA, colour = "black", fill = NA, shape = 21,
                         size = 1, stroke = 0.5, color = NULL)
{
  if (!is.null(color)) colour <- color
  structure(list(alpha = alpha, colour = colour, fill = fill,
                 shape = shape, size = size, stroke = stroke),
            class = "point_params")
}

#' @export
link_params <- function(env.point.hjust = NULL, env.point.vjust = NULL,
                        spec.point.hjust = NULL, spec.point.vjust = NULL)
{
  structure(list(env.point.hjust = env.point.hjust,
                 env.point.vjust = env.point.vjust,
                 spec.point.hjust = spec.point.hjust,
                 spec.point.vjust = spec.point.vjust),
            class = "link_params")
}
