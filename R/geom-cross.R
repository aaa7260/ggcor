#' Cross Geom
#'
#'
#' @eval rd_aesthetics("geom", "cross")
#' @param r0 the radius of an outer circle, defualt value is sqrt(2)/2.
#' @param sig.level significance threshold.
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
                       sig.level = 0.05,
                       linejoin = "mitre",
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
      sig.level = sig.level,
      linejoin = linejoin,
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
  required_aes = c("x", "y", "p"),

  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre",
                        sig.level = 0.05, r0 = 0.6) {
    if (!coord$is_linear()) {
      warning("geom_cross is not implemented for non-linear coordinates",
              call. = FALSE)
    }
    aesthetics <- setdiff(names(data), c("x", "y", "p"))
    data <- dplyr::filter(data, p > sig.level)
    dd <- point_to_cross(data$x, data$y, r0)
    GeomSegment$draw_panel(cbind(dd, data[, aesthetics]), panel_params, coord)
  },
  draw_key = draw_key_point
)

#' @noRd
point_to_cross <- function(x, y, r = 0.6) {
  xx <- c(x - 0.5 * r, x - 0.5 * r)
  xend <- c(x + 0.5 * r, x + 0.5 * r)
  yy <- c(y - 0.5 * r, y + 0.5 * r)
  yend <- c(y + 0.5 * r, y - 0.5 * r)

  new_data_frame(list(
    x = xx,
    y = yy,
    xend = xend,
    yend = yend
  ))
}
