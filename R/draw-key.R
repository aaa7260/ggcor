#' @noRd
draw_key_square <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  grid::rectGrob(
    x = 0.5,
    y = 0.5,
    width = unit(data$r0, "npc") - unit(data$size, "mm"),
    height = unit(data$r0, "npc") - unit(data$size, "mm"),
    gp = grid::gpar(col = data$colour %||% NA,
                    fill = scales::alpha(data$fill %||% "grey40", data$alpha),
                    lty = data$linetype %||% 1,
                    lwd = data$size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre",
                    lineend = if (identical(params$linejoin, "round")) "round" else "square"))
}

#' @noRd
draw_key_circle <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  grid::circleGrob(
    x = 0.5,
    y = 0.5,
    r = unit(0.5 * data$r0, "npc") - unit(data$size, "mm"),
    gp = grid::gpar(col = data$colour %||% NA,
                    fill = scales::alpha(data$fill %||% "grey40", data$alpha),
                    lty = data$linetype %||% 1,
                    lwd = data$size * ggplot2::.pt))
}

#' @noRd
draw_key_ellipse <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  data$zoom <- 1 - data$size / 5
  ellipseGrob(
    x = 0.5,
    y = 0.5,
    r0 = data$r0,
    zoom = data$zoom,
    gp = grid::gpar(col = data$colour %||% NA,
                    fill = scales::alpha(data$fill %||% "grey40", data$alpha),
                    lty = data$linetype %||% 1,
                    lwd = data$size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre"))
}

#' @noRd
ellipseGrob <- function(x = 0.5, y = 0.5, r0 = 0.5, zoom = 0.98, gp = grid::gpar()) {
  xy <- point_to_ellipse(0, 0, r0)
  px <- zoom * xy$x + x
  py <- zoom * xy$y + y
  grid::polygonGrob(px, py, gp = gp)
}

#' @noRd
draw_key_pie <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  data$zoom <- 1 - data$size / 5
  pieGrob(
    x = 0.5,
    y = 0.5,
    r0 = data$r0,
    zoom = data$zoom,
    gp = grid::gpar(col = data$colour %||% NA,
                    fill = scales::alpha(data$fill %||% "grey40", data$alpha),
                    lty = data$linetype %||% 1,
                    lwd = data$size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre"))
}
#' @noRd
pieGrob <- function(x = 0.5, y = 0.5, r0 = 0.5, zoom = 0.98, gp = grid::gpar()) {
  xy1 <- point_to_sector(0, 0, r0)
  xy2 <- point_to_line(0, 0, r0)
  px1 <- zoom * xy1$x + x
  py1 <- zoom * xy1$y + y
  px2 <- zoom * xy2$x + x
  py2 <- zoom * xy2$y + y
  grid::grobTree(
    grid::polygonGrob(px1, py1),
    grid::linesGrob(px2, py2),
    gp = gp
  )
}

#' @noRd
draw_key_star <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  data$zoom <- 1 - data$size / 5
  starGrob(
    x = 0.5,
    y = 0.5,
    n = data$n %||% 5,
    r0 = data$r0 %||% 0.5,
    ratio = data$ratio %||% 0.618,
    zoom = data$zoom,
    gp = grid::gpar(col = data$colour %||% "grey25",
                    fill = scales::alpha(data$fill %||% "grey40", data$alpha),
                    lty = data$linetype %||% 1,
                    lwd = data$size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre"))
}
#' @noRd
starGrob <- function(x = 0.5, y = 0.5, n = 5, r0 = 1,
                     ratio = 0.618, zoom = 0.98, gp = grid::gpar()) {
  xy <- point_to_star(0, 0, n, r0, ratio)
  px <- zoom * xy$x + x
  py <- zoom * xy$y + y
  grid::polygonGrob(px, py, gp = gp)
}
