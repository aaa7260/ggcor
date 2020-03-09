#' Link Geom
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_curve
#' @section Aesthetics:
#' \code{geom_link()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \strong{\code{xend}}
#'       \item \strong{\code{yend}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{group}
#'       \item \code{linetype}
#'       \item \code{size}
#'   }
#' @importFrom ggplot2 layer ggproto GeomCurve GeomPoint draw_key_path
#' @importFrom grid gTree
#' @rdname geom_link2
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_link2 <- function(mapping = NULL,
                      data = NULL,
                      stat = "identity",
                      position = "identity",
                      ...,
                      curvature = 0,
                      angle = 90,
                      ncp = 5,
                      arrow = NULL,
                      arrow.fill = NULL,
                      lineend = "butt",
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLink2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      curvature = curvature,
      angle = angle,
      ncp = ncp,
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_link2
#' @format NULL
#' @usage NULL
#' @export
GeomLink2 <- ggproto(
  "GeomLink2", GeomCurve,
  draw_panel = function(self, data, panel_params, coord, start.point.shape = 21,
                        end.point.shape = 21, start.point.colour = NULL,
                        end.point.colour = NULL, start.point.fill = NULL,
                        end.point.fill = NULL, start.point.size = 2,
                        end.point.size = 2, curvature = 0, angle = 90,
                        ncp = 5, arrow = NULL, arrow.fill = NULL, lineend = "butt",
                        na.rm = FALSE) {
    aesthetics <- setdiff(names(data), c("x", "y", "xend", "yend", "colour",
                                         "fill", "size", "linetype"))
    start.colour <- start.point.colour %||% data$colour
    end.colour <- end.point.colour %||% data$colour
    start.data <- new_data_frame(
      list(x = data$x,
           y = data$y,
           colour = start.colour,
           fill = start.point.fill %||% start.colour,
           shape = start.point.shape,
           size = start.point.size %||% data$size * 4,
           stroke = 0.5))
    end.data <- new_data_frame(
      list(x = data$xend,
           y = data$yend,
           colour = end.colour,
           fill = end.point.fill %||% end.colour,
           shape = end.point.shape,
           size = end.point.size %||% data$size * 4,
           stroke = 0.5))
    ggname(
      "geom_link",
      grid::gTree(
        children = grid::gList(
          GeomCurve$draw_panel(data, panel_params, coord, curvature = curvature,
                               angle = angle, ncp = ncp, arrow = arrow,
                               arrow.fill = arrow.fill, lineend = lineend,
                               na.rm = na.rm),
          GeomPoint$draw_panel(cbind(start.data, data[aesthetics]), panel_params, coord),
          GeomPoint$draw_panel(cbind(end.data, data[aesthetics]), panel_params, coord)
        )
      )
    )
  },
  draw_key = draw_key_path
)
