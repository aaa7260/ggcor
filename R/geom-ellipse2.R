#' Ellipse Geom
#'
#'
#' @eval rd_aesthetics("geom", "ellipse2")
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
                         n = 60,
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
      n = n,
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
  default_aes = aes(r0 = 1, colour = "grey35", fill = NA, size = 0.25, linetype = 1,
                    alpha = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord,
                        n = 60, linejoin = "mitre") {
    aesthetics <- setdiff(names(data), c("x", "y"))
    polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
      ell <- point_to_ellipse(row$x, row$y, row$r0, n)
      aes <- new_data_frame(row[aesthetics])[rep(1,n), ]
      GeomPolygon$draw_panel(cbind(ell, aes), panel_params, coord)
    })
    ggplot2:::ggname("geom_ellipse", do.call("grobTree", polys))
  },
  draw_key = draw_key_ellipse
)

#' @noRd
point_to_ellipse <- function(x, y, r0, n = 60) {
  t <- seq(0, 2 * pi, length = n)
  xx <- 0.5 * cos(t + acos(r0) / 2)  + x
  yy <- 0.5 * cos(t - acos(r0) / 2)  + y
  new_data_frame(list(
    x = xx,
    y = yy
  ))
}
