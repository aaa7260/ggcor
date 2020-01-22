#' Extra params for add_link
#' @description This function is used to control the details
#'     of the link, including the location, shape, size, color
#'     of points, and font size, font color, and so on of the
#'     text label.
#' @param spec.label NULL or "text_params" object producing by
#'     \code{\link[ggcor]{text_params}}.
#' @param spec.point NULL or "point_params" object producing by
#'     \code{\link[ggcor]{point_params}}.
#' @param env.point NULL or "point_params" object producing by
#'     \code{\link[ggcor]{point_params}}.
#' @param link.params "point_params" object producing by \code{\link[ggcor]{point_params}}.
#' @rdname extra_params
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
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

#' Extra text label params
#' @description This is mainly used in the add_link function to set the group label.
#' @param colour,color colour of text.
#' @param size font size of text.
#' @param angle angle to rotate the text.
#' @param hjust,vjust a numeric vector specifying horizontal/vertical justification.
#' @param alpha alpha channel for transparency.
#' @param family the font family.
#' @param fontface the font face.
#' @rdname text_params
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
text_params <- function(colour = "black", size = 3.88, angle = 0, hjust = NULL,
                        vjust = 0.5, alpha = NA, family = "", fontface = 1, color = NULL)
{
  if (!is.null(color)) colour <- color
  structure(list(colour = colour, size = size, angle = angle, hjust = hjust,
                 vjust = vjust, alpha = alpha, family = family, fontface = fontface),
            class = "text_params")
}

#' Extra points params
#' @description This is mainly used in the add_link function to control points style.
#' @param alpha alpha channel for transparency.
#' @param colour,color colour of points.
#' @param fill fill colour of points.
#' @param shape shape of points.
#' @param size size of points.
#' @param stroke stroke of points.
#' @rdname point_params
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
point_params <- function(alpha = NA, colour = "black", fill = NA, shape = 21,
                         size = 1, stroke = 0.5, color = NULL)
{
  if (!is.null(color)) colour <- color
  structure(list(alpha = alpha, colour = colour, fill = fill,
                 shape = shape, size = size, stroke = stroke),
            class = "point_params")
}

#' Control the points position of link
#' @description This is mainly used in the add_link function to control points position.
#' @param env.point.hjust,env.point.vjust a numeric vector is used to set the distance that
#'     points (close to the correlation matrix) moves horizontally or vertically.
#' @param spec.point.hjust,spec.point.vjust a numeric vector is used to set the distance that
#'     points (away from the correlation matrix) moves horizontally or vertically.
#' @rdname link_params
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
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
