#' Square Geom
#'
#'
#' @eval rd_aesthetics("geom", "square")
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @rdname geom_square
#' @export
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 GeomPolygon
#' @importFrom ggplot2 draw_key_polygon
#' @importFrom grid grobTree
geom_square <- function(mapping = NULL, data = NULL,
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
    geom = GeomSquare,
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

#' @rdname geom_square
#' @format NULL
#' @usage NULL
#' @export
GeomSquare <- ggproto(
  "GeomSquare", Geom,
  default_aes = aes(r0 = 0.5, colour = "grey35", fill = NA, size = 0.25, linetype = 1,
                    alpha = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
    aesthetics <- setdiff(names(data), c("x", "y"))
    polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
      square <- point_to_square(row$x, row$y, row$r0)
      aes <- new_data_frame(row[aesthetics])[rep(1, 5), ]
      GeomPolygon$draw_panel(cbind(square, aes), panel_params, coord)
    })

    ggplot2:::ggname("geom_square", do.call("grobTree", polys))
  },

  draw_key = draw_key_square
)

#' @noRd
point_to_square <- function(x, y, r0) {
  r0 <- 0.5 * sign(r0) * sqrt(abs(r0))
  xmin <- - r0 + x
  xmax <- r0 + x
  ymin <- - r0 + y
  ymax <- r0 + y
  new_data_frame(list(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  ))
}

