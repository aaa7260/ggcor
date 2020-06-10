#' Quadratic Bezier Spline
#' @title Quadratic Bezier Spline

#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#' @param abs if TRUE, the height will be the ratio of the segment length.
#' @param n a positive integer value.
#' @section Aesthetics:
#' \code{geom_segment0()} understands the following aesthetics (required
#' aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{edge_alpha}
#'       \item \code{edge_colour}
#'       \item \code{edge_linetype}
#'       \item \code{edge_width}
#'       \item \code{height}
#'       \item \code{adjust}
#'    }
#' @rdname geom_segment0
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_quad_line <- function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           ...,
                           abs = FALSE,
                           n = 100,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomQuadLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      abs = abs,
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_segment0
#' @format NULL
#' @usage NULL
#' @export
GeomSegment0 <- ggproto(
  "GeomSegment0", GeomSegment,
  default_aes = aes(edge_colour = "grey35", edge_width = 0.5, edge_linetype = 1,
                    edge_alpha = NA, height = 0.5, adjust = 0, abs = FALSE),
  required_aes = c("x", "y", "xend", "yend"),
  non_missing_aes = c("edge_linetype", "edge_width", "height", "adjust"),
  draw_panel = function(data, panel_params, coord, n = 100, lineend = "butt") {
    if(empty(data)) {
      return(ggplot2::zeroGrob())
    }
    len <- nrow(data)
    data <- segment_to_path(data, abs = abs)
    data <- coord$transform(data, panel_params)
    
    grobs <- lapply(seq_len(len), function(.row) {
      quadGrob <- get_function("gridBezier", "quadGrob")
      nSteps <- get_function("gridBezier", "nSteps")
      d <- subset(data, subset = c(group = .row))
      first_row <- d[1, , drop = FALSE]
      quadGrob(
        d$x, d$y, default.units = "native", open = TRUE, stepFn = nSteps(n),
        gp = gpar(
          col = scales::alpha(first_row$edge_colour, first_row$edge_alpha),
          lwd = first_row$edge_width * ggplot2::.pt,
          lty = first_row$edge_linetype,
          lineend = lineend)
      )
    })
    ggname("geom_segment0", do.call("grobTree", polys))
  },
  
  draw_key = draw_key_path2
)

#' @noRd
calc_ctrl <- function(x, 
                      y, 
                      xend, 
                      yend, 
                      height = 0.2, 
                      adjust = 0,
                      abs = FALSE) {
  if(!is.finite(adjust))
    adjust <- 0
  
  if(x > xend) {
    xtemp <- x
    ytemp <- y
    x <- xend
    y <- yend
    xend <- xtemp
    yend <- ytemp
  }
  
  d <- sqrt((yend - y) ^ 2 + (xend - x) ^ 2)
  k <- (yend - y) / (xend - x)
  if(isFALSE(abs)) {
    height <- d * height
  }
  theata <- sign(k) * atan(abs(k))
  m <- matrix(c(cos(theata), sin(theata), -sin(theata), cos(theata)), nrow = 2)
  o <- m %*% c(0.5 * adjust * d, height) + c((x + xend) / 2, (y + yend) / 2)
  
  new_data_frame(list(
    x = c(x, o[1], xend),
    y = c(y, o[2], yend),
    .index = 1:3))
}

#' @noRd
segment_to_path <- function(data, abs = FALSE) {
  id <- c("x", "y", "xend", "yend", "height", "adjust")
  data$group <- seq_len(nrow(data))
  ctrls <- purrr::pmap_dfr(data[id], calc_ctrl, abs = abs)
  cbind(ctrls, data[rep(seq_len(nrow(data)), each = 3), setdiff(names(data), id)])
}
