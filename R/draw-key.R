#' @noRd
draw_key_square <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  grid::rectGrob(
    x = 0.5,
    y = 0.5,
    width = grid::unit(data$r0, "npc") - grid::unit(data$size, "mm"),
    height = grid::unit(data$r0, "npc") - grid::unit(data$size, "mm"),
    gp = grid::gpar(col = data$colour %||% NA,
                    fill = scales::alpha(data$fill %||% "grey40", data$alpha),
                    lty = data$linetype %||% 1,
                    lwd = data$size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre",
                    lineend = if (identical(params$linejoin, "round")) "round" else "square"))
}

#' @noRd
draw_key_upper_square <- function(data, params, size) {
  if (is.null(data$upper_size)) {
    data$upper_size <- 0.5
  }
  grid::rectGrob(
    x = 0.5,
    y = 0.5,
    width = grid::unit(data$upper_r0, "npc") - grid::unit(data$upper_size, "mm"),
    height = grid::unit(data$upper_r0, "npc") - grid::unit(data$upper_size, "mm"),
    gp = grid::gpar(col = data$upper_colour %||% NA,
                    fill = scales::alpha(data$upper_fill %||% "grey40", data$upper_alpha),
                    lty = data$upper_linetype %||% 1,
                    lwd = data$upper_size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre",
                    lineend = if (identical(params$linejoin, "round")) "round" else "square"))
}

#' @noRd
draw_key_lower_square <- function(data, params, size) {
  if (is.null(data$lower_size)) {
    data$lower_size <- 0.5
  }
  grid::rectGrob(
    x = 0.5,
    y = 0.5,
    width = grid::unit(data$lower_r0, "npc") - grid::unit(data$lower_size, "mm"),
    height = grid::unit(data$lower_r0, "npc") - grid::unit(data$lower_size, "mm"),
    gp = grid::gpar(col = data$lower_colour %||% NA,
                    fill = scales::alpha(data$lower_fill %||% "grey40", data$lower_alpha),
                    lty = data$lower_linetype %||% 1,
                    lwd = data$lower_size * ggplot2::.pt,
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
    r = grid::unit(0.5 * data$r0, "npc") - grid::unit(data$size, "mm"),
    gp = grid::gpar(col = data$colour %||% NA,
                    fill = scales::alpha(data$fill %||% "grey40", data$alpha),
                    lty = data$linetype %||% 1,
                    lwd = data$size * ggplot2::.pt))
}

#' @noRd
draw_key_upper_circle <- function(data, params, size) {
  if (is.null(data$upper_size)) {
    data$upper_size <- 0.5
  }
  grid::circleGrob(
    x = 0.5,
    y = 0.5,
    r = grid::unit(0.5 * data$upper_r0, "npc") - grid::unit(data$upper_size, "mm"),
    gp = grid::gpar(col = data$upper_colour %||% NA,
                    fill = scales::alpha(data$upper_fill %||% "grey40", data$upper_alpha),
                    lty = data$upper_linetype %||% 1,
                    lwd = data$upper_size * ggplot2::.pt))
}

#' @noRd
draw_key_lower_circle <- function(data, params, size) {
  if (is.null(data$lower_size)) {
    data$lower_size <- 0.5
  }
  grid::circleGrob(
    x = 0.5,
    y = 0.5,
    r = grid::unit(0.5 * data$lower_r0, "npc") - grid::unit(data$lower_size, "mm"),
    gp = grid::gpar(col = data$lower_colour %||% NA,
                    fill = scales::alpha(data$lower_fill %||% "grey40", data$lower_alpha),
                    lty = data$lower_linetype %||% 1,
                    lwd = data$lower_size * ggplot2::.pt))
}

#' @noRd
draw_key_upper_colour <- function(data, params, size) {
  if (is.null(data$upper_size)) {
    data$upper_size <- 0.5
  }

  lwd <- min(data$upper_size, min(size) / 4)

  grid::rectGrob(
    width = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
    height = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
    gp = grid::gpar(
      col = data$upper_colour %||% NA,
      fill = scales::alpha(data$upper_fill %||% "grey20", data$upper_alpha),
      lty = data$upper_linetype %||% 1,
      lwd = lwd * ggplot2::.pt,
      linejoin = params$linejoin %||% "mitre",
      lineend = if (identical(params$linejoin, "round")) "round" else "square"
    ))
}

#' @noRd
draw_key_lower_colour <- function(data, params, size) {
  if (is.null(data$lower_size)) {
    data$lower_size <- 0.5
  }

  lwd <- min(data$lower_size, min(size) / 4)

  grid::rectGrob(
    width = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
    height = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
    gp = grid::gpar(
      col = data$lower_colour %||% NA,
      fill = scales::alpha(data$lower_fill %||% "grey20", data$lower_alpha),
      lty = data$lower_linetype %||% 1,
      lwd = lwd * ggplot2::.pt,
      linejoin = params$linejoin %||% "mitre",
      lineend = if (identical(params$linejoin, "round")) "round" else "square"
    ))
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
draw_key_upper_ellipse <- function(data, params, size) {
  if (is.null(data$upper_size)) {
    data$upper_size <- 0.5
  }
  data$upper_zoom <- 1 - data$upper_size / 5
  ellipseGrob(
    x = 0.5,
    y = 0.5,
    r0 = data$upper_r0,
    zoom = data$upper_zoom,
    gp = grid::gpar(col = data$upper_colour %||% NA,
                    fill = scales::alpha(data$upper_fill %||% "grey40", data$upper_alpha),
                    lty = data$upper_linetype %||% 1,
                    lwd = data$upper_size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre"))
}

#' @noRd
draw_key_lower_ellipse <- function(data, params, size) {
  if (is.null(data$lower_size)) {
    data$lower_size <- 0.5
  }
  data$lower_zoom <- 1 - data$lower_size / 5
  ellipseGrob(
    x = 0.5,
    y = 0.5,
    r0 = data$lower_r0,
    zoom = data$lower_zoom,
    gp = grid::gpar(col = data$lower_colour %||% NA,
                    fill = scales::alpha(data$lower_fill %||% "grey40", data$lower_alpha),
                    lty = data$lower_linetype %||% 1,
                    lwd = data$lower_size * ggplot2::.pt,
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
draw_key_ring <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  data$zoom <- 1 - data$size / 5
  fill <- c(scales::alpha(data$fill %||% "grey40", data$alpha),
            params$remain.fill %||% NA)
  ringGrob(
    x = 0.5,
    y = 0.5,
    r0 = data$r0,
    start.radius = params$start.radius %||% 0.25,
    end.radius = params$end.radius %||% 0.5,
    zoom = data$zoom,
    gp = grid::gpar(col = data$colour %||% NA,
                    fill = fill,
                    lty = data$linetype %||% 1,
                    lwd = data$size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre"))
}

#' @noRd
draw_key_upper_ring <- function(data, params, size) {
  if (is.null(data$upper_size)) {
    data$upper_size <- 0.5
  }
  data$upper_zoom <- 1 - data$upper_size / 5
  fill <- c(scales::alpha(data$upper_fill %||% "grey40", data$upper_alpha),
            params$remain.fill %||% NA)
  ringGrob(
    x = 0.5,
    y = 0.5,
    r0 = data$upper_r0,
    start.radius = params$start.radius %||% 0.25,
    end.radius = params$end.radius %||% 0.5,
    zoom = data$upper_zoom,
    gp = grid::gpar(col = data$upper_colour %||% NA,
                    fill = fill,
                    lty = data$upper_linetype %||% 1,
                    lwd = data$upper_size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre"))
}

#' @noRd
draw_key_lower_ring <- function(data, params, size) {
  if (is.null(data$lower_size)) {
    data$lower_size <- 0.5
  }
  data$lower_zoom <- 1 - data$lower_size / 5
  fill <- c(scales::alpha(data$lower_fill %||% "grey40", data$lower_alpha),
            params$remain.fill %||% NA)
  ringGrob(
    x = 0.5,
    y = 0.5,
    r0 = data$lower_r0,
    start.radius = params$start.radius %||% 0.25,
    end.radius = params$end.radius %||% 0.5,
    zoom = data$lower_zoom,
    gp = grid::gpar(col = data$lower_colour %||% NA,
                    fill = fill,
                    lty = data$lower_linetype %||% 1,
                    lwd = data$lower_size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre"))
}

#' @noRd
ringGrob <- function(x = 0.5, y = 0.5, r0 = 0.5, start.radius = 0.25,
                     end.radius = 0.5, zoom = 0.98, gp = grid::gpar()) {
  xy <- point_to_ring(0, 0, r0, start.radius, end.radius, steps = 0.1)
  x <- xy$x * zoom + x
  y <- xy$y * zoom + y
  grid::polygonGrob(x, y, xy$group, gp = gp)
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
draw_key_upper_star <- function(data, params, size) {
  if (is.null(data$upper_size)) {
    data$upper_size <- 0.5
  }
  data$upper_zoom <- 1 - data$upper_size / 5
  starGrob(
    x = 0.5,
    y = 0.5,
    n = data$upper_n %||% 5,
    r0 = data$upper_r0 %||% 0.5,
    ratio = data$upper_ratio %||% 0.618,
    zoom = data$upper_zoom,
    gp = grid::gpar(col = data$upper_colour %||% "grey25",
                    fill = scales::alpha(data$upper_fill %||% "grey40", data$upper_alpha),
                    lty = data$upper_linetype %||% 1,
                    lwd = data$upper_size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre"))
}

#' @noRd
draw_key_lower_star <- function(data, params, size) {
  if (is.null(data$lower_size)) {
    data$lower_size <- 0.5
  }
  data$lower_zoom <- 1 - data$lower_size / 5
  starGrob(
    x = 0.5,
    y = 0.5,
    n = data$lower_n %||% 5,
    r0 = data$lower_r0 %||% 0.5,
    ratio = data$lower_ratio %||% 0.618,
    zoom = data$lower_zoom,
    gp = grid::gpar(col = data$lower_colour %||% "grey25",
                    fill = scales::alpha(data$lower_fill %||% "grey40", data$lower_alpha),
                    lty = data$lower_linetype %||% 1,
                    lwd = data$lower_size * ggplot2::.pt,
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
