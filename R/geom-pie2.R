#' Pie Geom
#'
#'
#' @eval rd_aesthetics("geom", "pie2")
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @rdname geom_pie2
#' @export
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 GeomPolygon
#' @importFrom ggplot2 GeomLine
#' @importFrom ggplot2 draw_key_polygon
#' @importFrom grid grobTree
geom_pie2 <- function(mapping = NULL, data = NULL,
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
    geom = GeomPie2,
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

#' @rdname geom_pie2
#' @format NULL
#' @usage NULL
#' @export
GeomPie2 <- ggproto(
  "GeomPie2", Geom,
  default_aes = aes(r = 1, colour = "grey60", fill = NA, size = 0.25, linetype = 1,
                    alpha = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord, n = 60, linejoin = "mitre") {
    aesthetics <- setdiff(names(data), c("x", "y"))
    polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
      s <- point_to_sector(x = row$x, y = row$y, r = row$r, n)
      l <- point_to_line(x = row$x, y = row$y, r = row$r, n - 2)
      if(row$r == 0) {
        aes_sector <- new_data_frame(row[aesthetics])[rep(1, 3), ]
        sector <- GeomPolygon$draw_panel(cbind(s, aes_sector), panel_params, coord)
      } else {
        aes_sector <- new_data_frame(row[aesthetics])[rep(1, n), ]
        sector <- GeomPolygon$draw_panel(cbind(s, aes_sector), panel_params, coord)
      }
      if(row$r == 1) {
        line <- grid::nullGrob()
      } else {
        aes_line <- new_data_frame(row[aesthetics])[rep(1, n), ]
        line <- GeomLine$draw_panel(cbind(l, aes_line), panel_params, coord)
      }
      grid::gList(sector, line)
    })

    ggplot2:::ggname("pie", do.call("grobTree", polys))
  },

  draw_key = draw_key_pie
)
#' @noRd
point_to_sector <- function(x, y, r, n = 58) {
  if(r == 0) {
    xx <- c(x, x, x)
    yy <- c(y, y + 0.5, y)
  } else {
    t <- seq(pi / 2, 2 * r * pi + pi / 2, length.out = n)
    xx <- c(x, 0.5 * cos(t) + x, x)
    yy <- c(y, 0.5 * sin(t) + y, y)
  }
  new_data_frame(list(
    x = xx,
    y = yy
  ))
}

#' @noRd
point_to_line <- function(x, y, r, n = 60) {
  sign <- sign(r)
  t <- seq(2 * r * pi + pi / 2, sign * 2 * pi + pi/2, length.out = n)
  xx <- 0.5 * cos(t) + x
  yy <- 0.5 * sin(t) + y
  new_data_frame(list(
    x = xx,
    y = yy
  ))
}

