#' @export
draw_key_cross <- function(data, params, size) {
  grid::segmentsGrob(c(0.15, 0.15), c(0.15, 0.85), c(0.85, 0.85), c(0.85, 0.15),
                     gp = grid::gpar(
                       col = data$colour %||% "red",
                       lwd = (data$size %||% 0.5) * ggplot2::.pt,
                       lty = data$linetype %||% 1
                     ))
}
