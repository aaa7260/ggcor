#' @export
draw_key_star <- function(data, params, size) {
  starGrob(0.5, 0.5, params$n, data$r0 %||% 0.5, params$ratio %||% 0.618,
              gp = gpar(
                col = scales::alpha(data$colour %||% "grey50"),
                fill = scales::alpha(data$fill %||% "grey90", data$alpha),
                lty = data$linetype %||% 1,
                lwd = (data$size %||% 0.5) * .pt
              ))
}

#' @noRd
starGrob <- function(x = 0.5, y = 0.5, n = 5, r0 = 0.5, ratio = 0.618,
                        gp = gpar(colour = "grey50", fill = "grey90"),
                        default.units = "native", ...) {
  dd <- point_to_star(x, y, n, r0, ratio)
  grid::polygonGrob(dd$x, dd$y, default.units = default.units, gp = gp, ...)
}

#' @export
draw_key_circle <- function(data, params, size) {
  grid::circleGrob(0.5, 0.5, r = 0.5 * (data$r0 %||% 0.5),
                   gp = gpar(
                     col = alpha(data$colour %||% "grey50", data$alpha),
                     fill = alpha(data$fill %||% "grey90", data$alpha),
                     lty = data$linetype %||% 1,
                     lwd = (data$size %||% 0.5) * .pt
                   ))
}
#' @export
draw_key_ellipse <- function(data, params, size) {
  ellipseGrob(0.5, 0.5, data$rho %||% 0,
              gp = gpar(
                col = scales::alpha(data$colour %||% "grey50", data$alpha),
                fill = scales::alpha(data$fill %||% "grey90", data$alpha),
                lty = data$linetype %||% 1,
                lwd = (data$size %||% 0.25) * .pt
              ))
}
#' @noRd
ellipseGrob <- function(x = 0.5, y = 0.5, rho = 1, n = 100,
                        gp = gpar(colour = "grey50", fill = "grey90"),
                        default.units = "native", ...) {
  }
  dd <- point_to_ellipse(x, y, rho, n)
  grid::polygonGrob(dd$x, dd$y, default.units = default.units, gp = gp, ...)
}

#' @export
draw_key_square <- function(data, params, size) {
  grid::rectGrob(0.5, 0.5,
                 width = abs(data$r0),
                 height = abs(data$r0),
                 gp = grid::gpar(
                   col = scales::alpha(data$colour, data$alpha),
                   fill = scales::alpha(data$fill %||% "grey90", data$alpha),
                   lwd = (data$size %||% 0.25) * ggplot2::.pt,
                   linetype = data$linetype %||% 1
                 ))
}
#' @export
draw_key_pie <- function(data, params, size){
  pieGrob(0.5, 0.5, data$r %||% 0, params$n %||% 100,
          gp = gpar(
            col = scales::alpha(data$colour, data$alpha),
            fill = scales::alpha(data$fill %||% "grey90", data$alpha),
            lwd = (data$size %||% 0.25) * ggplot2::.pt,
            linetype = data$linetype %||% 1
          ))
}
#' @noRd
pieGrob <- function(x = 0.5, y = 0.5, r = 0.5, n = 100,
                    gp = gpar(colour = "grey50", fill = "grey90"),
                    default.units = "npc", ...) {
  line <- point_to_line(x, y, r, n)
  sector <- point_to_sector(x, y, r, n - 2)
  grid::grobTree(
    grid::linesGrob(line$x, line$y, default.units = default.units, gp = gp, ...),
    grid::polygonGrob(sector$x, sector$y, default.units = default.units, gp = gp, ...)
  )
}
