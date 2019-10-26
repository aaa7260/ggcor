#' Circles based on center and radius
#'
#'
#' @eval rd_aesthetics("geom", "circle2")
#' @param r0 the radius of circle, defualt value is 0.48.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @rdname geom_circle2
#' @export
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 GeomPolygon
#' @importFrom ggplot2 draw_key_polygon
#' @importFrom grid grobTree
geom_circle2 <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         ...,
                         linejoin = "mitre",
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
      linejoin = linejoin,
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
  "GeomCircle2", Geom,
  default_aes = aes(r = 1, colour = "grey35", fill = NA, size = 0.25, linetype = 1,
                    alpha = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord, r0 = 0.48, linejoin = "mitre") {
    aesthetics <- setdiff(names(data), c("x", "y", "r"))
    polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
      circle <- point_to_circle(row$x, row$y, row$r, r0)
      aes <- new_data_frame(row[aesthetics])[rep(1,60), ]
      GeomPolygon$draw_panel(cbind(circle, aes), panel_params, coord)
    })
    ggplot2:::ggname("circle", do.call("grobTree", polys))
  },
  draw_key = draw_key_polygon
)

#' @noRd

point_to_circle <- function(x, y, r, r0, n = 60) {
  rr <- r0 * sqrt(abs(r))
  t <- seq(0, 2*pi, length.out = n)
  xx <- rr * cos(t) + x
  yy <- rr * sin(t) + y
  new_data_frame(list(
    x = xx,
    y = yy
  ))
}


