#' Shade Geom
#'
#'
#' @eval rd_aesthetics("geom", "shade")
#' @param sign scalar numeric value. If less than 0, add shade on cells with
#'     negtive \code{r} values. If larger than 0, add shade on cells with positive
#'     \code{r} values. If equals 0, add shade on cells except where \code{r}  values
#'     equals 0.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_line
#' @rdname geom_shade
#' @export
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 GeomLine
#' @importFrom ggplot2 draw_key_blank
#' @importFrom grid grobTree
geom_shade <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        sign = 1,
                        linejoin = "mitre",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomShade,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      sign = sign,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_shade
#' @format NULL
#' @usage NULL
#' @export
GeomShade <- ggproto(
  "GeomShade", Geom,
  default_aes = aes(colour = "white", fill = NA, size = 0.25, linetype = 1,
                    alpha = NA),
  required_aes = c("x", "y", "r"),
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre",
                        sign = 1) {
    aesthetics <- setdiff(names(data), c("x", "y", "r"))
    polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
      if(sign < 0) {
        if(row$r >= 0) return(grid::nullGrob())
        shade <- point_to_shade(row$x, row$y, row$r)
        aes <- new_data_frame(row[aesthetics])[rep(1, 3), ]
        return(GeomSegment$draw_panel(cbind(shade, aes), panel_params, coord))
      }
      if(sign > 0) {
        if(row$r <= 0) return(grid::nullGrob())
        shade <- point_to_shade(row$x, row$y, row$r)
        aes <- new_data_frame(row[aesthetics])[rep(1, 3), ]
        return(GeomSegment$draw_panel(cbind(shade, aes), panel_params, coord))
      }
      if(sign == 0) {
        if(row$r == 0) return(grid::nullGrob())
        shade <- point_to_shade(row$x, row$y, row$r)
        aes <- new_data_frame(row[aesthetics])[rep(1, 3), ]
        return(GeomSegment$draw_panel(cbind(shade, aes), panel_params, coord))
      }
    })

    ggplot2:::ggname("geom_shade", do.call("grobTree", polys))
  },
  draw_key = draw_key_blank
)


#' @noRd

point_to_shade <- function(x, y, sign)
{
  if(sign > 0) {
    xx <- c(x - 0.5, x - 0.5, x)
    xend <- c(x, x + 0.5, x + 0.5)
    yy <- c(y, y + 0.5, y + 0.5)
    yend <- c(y - 0.5, y - 0.5, y)
  }
  if(sign < 0) {
    xx <- c(x, x - 0.5, x - 0.5)
    xend <- c(x + 0.5, x + 0.5, x)
    yy <- c(y - 0.5, y - 0.5, y)
    yend <- c(y, y + 0.5, y + 0.5)
  }
  new_data_frame(list(
    x = xx,
    y = yy,
    xend = xend,
    yend = yend
  ))
}

