#' Square Geom
#'
#' @eval rd_aesthetics("geom", "square")
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @importFrom ggplot2 layer ggproto GeomPolygon
#' @importFrom grid grobTree
#' @rdname geom_square
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_square <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
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
  "GeomSquare", GeomPolygon,
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

