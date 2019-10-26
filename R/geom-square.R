#' Square based on center and radius
#'
#'
#' @eval rd_aesthetics("geom", "square")
#' @param r0 the radius of circle, defualt value is 0.48.
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
  default_aes = aes(colour = "grey35", fill = NA, size = 0.25, linetype = 1,
                    alpha = NA),
  required_aes = c("x", "y", "r"),
  draw_panel = function(self, data, panel_params, coord, r0 = 0.48, linejoin = "mitre") {
    aesthetics <- setdiff(names(data), c("x", "y", "r"))
    polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
      square <- point_to_square(row$x, row$y, row$r, r0)
      aes <- new_data_frame(row[aesthetics])[rep(1, 5), ]
      GeomPolygon$draw_panel(cbind(square, aes), panel_params, coord)
    })

    ggplot2:::ggname("square", do.call("grobTree", polys))
  },

  draw_key = draw_key_polygon
)

#' @noRd
point_to_square <- function(x, y, r, r0 = 0.48) {
  rr <- r0 * sqrt(abs(r))
  xmin <- - rr + x
  xmax <- rr + x
  ymin <- - rr + y
  ymax <- rr + y
  new_data_frame(list(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  ))
}

