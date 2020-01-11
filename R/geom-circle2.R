#' Circle Geom
#'
#' @param n the number of circle path points.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @section Aesthetics:
#'     \code{geom_circle2()} understands the following aesthetics (required
#'     aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{r0}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{linetype}
#'       \item \code{size}
#'    }
#' @importFrom ggplot2 layer ggproto GeomPolygon
#' @importFrom grid grobTree
#' @rdname geom_circle2
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_circle2 <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         ...,
                         n = 60,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCircle2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_circle2
#' @format NULL
#' @usage NULL
#' @export
GeomCircle2 <- ggproto(
  "GeomCircle2", GeomPolygon,
  default_aes = aes(r0 = 1, colour = "grey35", fill = NA, size = 0.25, linetype = 1,
                    alpha = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord, n = 60, linejoin = "mitre") {
    aesthetics <- setdiff(names(data), c("x", "y"))
    polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
      circle <- point_to_circle(row$x, row$y, row$r0, n)
      aes <- new_data_frame(row[aesthetics])[rep(1, n), ]
      GeomPolygon$draw_panel(cbind(circle, aes), panel_params, coord)
    })
    ggplot2:::ggname("geom_circle", do.call("grobTree", polys))
  },
  draw_key = draw_key_circle
)

#' @noRd

point_to_circle <- function(x, y, r0, n = 60) {
  t <- seq(0, 2*pi, length.out = n)
  r0 <- 0.5 * sign(r0) * sqrt(abs(r0))
  xx <- r0 * cos(t) + x
  yy <- r0 * sin(t) + y
  new_data_frame(list(
    x = xx,
    y = yy
  ))
}


