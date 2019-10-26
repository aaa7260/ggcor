#' Ellipse based on center, radius and ovality.
#'
#'
#' @eval rd_aesthetics("geom", "ellipse2")
#' @param r0 the radius of circle, defualt value is 0.48.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @rdname geom_ellipse2
#' @export
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 GeomPolygon
#' @importFrom ggplot2 draw_key_polygon
#' @importFrom grid grobTree
geom_ellipse2 <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         linejoin = "mitre",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomEllipse2,
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

#' @rdname geom_ellipse2
#' @format NULL
#' @usage NULL
#' @export
GeomEllipse2 <- ggproto(
  "GeomEllipse2", Geom,
  default_aes = aes(colour = "grey35", fill = NA, size = 0.25, linetype = 1,
                    alpha = NA),
  required_aes = c("x", "y", "r"),
  draw_panel = function(self, data, panel_params, coord,
                        r0 = 0.48, linejoin = "mitre") {
    aesthetics <- setdiff(names(data), c("x", "y", "r"))
    polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
      ell <- point_to_ellipse(row$x, row$y, row$r, r0)
      aes <- new_data_frame(row[aesthetics])[rep(1,60), ]
      GeomPolygon$draw_panel(cbind(ell, aes), panel_params, coord)
    })
    ggplot2:::ggname("ellipse", do.call("grobTree", polys))
  },
  draw_key = draw_key_polygon
)

#' @noRd
point_to_ellipse <- function(x, y, r, r0 = 0.48, length = 60) {
  t <- seq(0, 2 * pi, length = length)
  xx <- r0 * cos(t + acos(r) / 2)  + x
  yy <- r0 * cos(t - acos(r) / 2)  + y
  new_data_frame(list(
    x = xx,
    y = yy
  ))
}
