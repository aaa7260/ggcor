#' Cross Geom
#'
#'
#' @eval rd_aesthetics("geom", "cross")
#' @param conf.level confidence level.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @rdname geom_cross
#' @export
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 GeomPoint
#' @importFrom ggplot2 draw_key_point


geom_cross <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       conf.level = 0.05,
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
  "GeomCross", GeomPoint,
  default_aes = aes(shape = 4, colour = "red", size = 5, fill = NA,
                    alpha = NA, stroke = 0.5),
  required_aes = c("x", "y", "p.value"),

  draw_panel = function(self, data, panel_params, coord, conf.level = 0.05) {
    data <- dplyr::filter(data, p.value >= conf.level)
    data$p.value <- NULL
    GeomPoint$draw_panel(data, panel_params, coord)
  },
  draw_key = draw_key_point
)
