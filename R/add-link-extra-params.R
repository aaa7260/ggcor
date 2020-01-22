#' Extra params for add_link
<<<<<<< HEAD
#' @description This function is similar to the \code{theme} function in ggplot2,
#'     used to set the shape, size, colour, fill of spec and env marker points,
#'     size, colour, family, and so on of spec labels.
#' @param spec.label text_params object (\code{\link[ggcor]{text_params}}).
#' @param spec.point,env.point point_params object (\code{\link[ggcor]{point_params}}).
#' @param link.params link_params object (\code{\link[ggcor]{link_params}}).
#' @rdname extra_params
#' @seealso \code{\link[ggcor]{text_params}}, \code{\link[ggcor]{point_params}},
#'     \code{\link[ggcor]{link_params}}.
=======
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
>>>>>>> dev1.0.0
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
<<<<<<< HEAD
#' Extra params elements
#' @description In conjunction with the \code{\link[ggcor]{extra_params}}.
#' @param fill Fill colour.
#' @param colour,color Line/border colour. Color is an alias for colour.
#' @param size Line/border size in mm; text size in pts.
#' @param alpha alpha level in [0,1].
#' @param angle angle (in [0, 360]).
#' @param hjust horizontal justification (in [0, 1]).
#' @param vjust vertical justification (in [0, 1]).
#' @param family font family.
#' @param fontface font face.
#' @param shape point shape.
#' @param stroke point stroke.
#' @param spec.point.hjust,env.point.hjust numeric vector, adjust horizontal position of points.
#' @param spec.point.vjust,env.point.vjust numeric vector, adjust vertical position of points.
#' @rdname extra_params_elements
#' @seealso \code{\link[ggcor]{extra_params}}.
=======

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
>>>>>>> dev1.0.0
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
<<<<<<< HEAD
#' @rdname extra_params_elements
=======

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
>>>>>>> dev1.0.0
#' @export
point_params <- function(alpha = NA, colour = "black", fill = NA, shape = 21,
                         size = 1, stroke = 0.5, color = NULL)
{
  if (!is.null(color)) colour <- color
  structure(list(alpha = alpha, colour = colour, fill = fill,
                 shape = shape, size = size, stroke = stroke),
            class = "point_params")
}
<<<<<<< HEAD
#' @rdname extra_params_elements
=======

#' Control the points position of link
#' @description This is mainly used in the add_link function to control points position.
#' @param env.point.hjust,env.point.vjust a numeric vector is used to set the distance that
#'     points (close to the correlation matrix) moves horizontally or vertically.
#' @param spec.point.hjust,spec.point.vjust a numeric vector is used to set the distance that
#'     points (away from the correlation matrix) moves horizontally or vertically.
#' @rdname link_params
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
>>>>>>> dev1.0.0
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
