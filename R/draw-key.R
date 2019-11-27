#' @export
draw_key_square <- function(data, params, size) {
  grid::rectGrob(0.5, 0.5,
                 width = 2 * abs(data$r),
                 height = 2 * abs(data$r),
                 gp = gpar(
                   col = scales::alpha(data$colour, data$alpha),
                   fill = scales::alpha(data$fill, data$alpha),
                   lwd = (data$size %||% 0.5) * ggplot2::.pt,
                   linetype = data$linetype %||% 1
                 ))
}
#' @export
draw_key_pie <- function(data, params, size){
  pieGrob(0.5, 0.5, data$r %||% 0, params$n %||% 100)
}
#' @noRd
pieGrob <- function(x = 0.5, y = 0.5, r = 0.5, n = 100,
                    gp = gpar(colour = "grey50", fill = "grey90"),
                    default.units = "npc", ...) {
  line <- point_to_line(x, y, r, n)
  sector <- point_to_sector(x, y, r, n)
  grid::grobTree(
    grid::linesGrob(line$x, line$y, default.units = default.units, gp = gp, ...),
    grid::polygonGrob(sector$x, sector$y, default.units = default.units, gp = gp, ...)
  )
}
#' @export
draw_key_cross <- function(data, params, size) {
  grid::segmentsGrob(c(0.15, 0.15), c(0.15, 0.85), c(0.85, 0.85), c(0.85, 0.15),
                     gp = grid::gpar(
                       col = data$colour %||% "red",
                       lwd = (data$size %||% 0.5) * ggplot2::.pt,
                       lty = data$linetype %||% 1
                     ))
}
