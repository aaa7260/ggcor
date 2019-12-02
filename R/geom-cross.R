#' Cross Geom
#'
#'
#' @eval rd_aesthetics("geom", "cross")
#' @param r0 the radius of outer circle, defualt value is 0.6.
#' @param conf.level confidence level.
#' @param labels labels for the levels of different confidence levels.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_segment
#' @rdname geom_cross
#' @export
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 GeomSegment
#' @importFrom ggplot2 draw_key_blank
#' @importFrom grid grobTree


geom_cross <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       conf.level = 0.05,
                       r0 = 0.6,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCross,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      conf.level = conf.level,
      r0 = r0,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_cross
#' @format NULL
#' @usage NULL
#' @export
GeomCross <- ggproto(
  "GeomCross", Geom,
  default_aes = aes(colour = "red", fill = NA,
                    size = 0.5, linetype = 1, alpha = NA),
  required_aes = c("x", "y", "p.value"),

  draw_panel = function(self, data, panel_params, coord, conf.level = 0.05, r0 = 0.6) {
    data <- dplyr::filter(data, p.value >= conf.level)
    dd <- point_to_cross(data$x, data$y, r0)
    aesthetics <- setdiff(names(data), c("x", "y", "p.value"))
    GeomSegment$draw_panel(cbind(dd, data[rep(1:nrow(data), each = 2), aesthetics]),
                           panel_params, coord)
  },
  draw_key = draw_key_blank
)

#' @noRd
point_to_cross <- function(x, y, r0 = 0.6) {
  xx <- c(x - 0.5 * r0, x - 0.5 * r0)
  xend <- c(x + 0.5 * r0, x + 0.5 * r0)
  yy <- c(y - 0.5 * r0, y + 0.5 * r0)
  yend <- c(y + 0.5 * r0, y - 0.5 * r0)

  new_data_frame(list(
    x = xx,
    y = yy,
    xend = xend,
    yend = yend
  ))
}

