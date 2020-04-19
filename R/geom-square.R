#' Square Geom
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_rect
#' @section Aesthetics:
#' \code{geom_square()}, \code{geom_upper_square()} and \code{geom_lower_square()}
#' understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{r0}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{linetype}
#'       \item \code{size}
#'       \item \code{upper_r0}
#'       \item \code{upper_alpha}
#'       \item \code{upper_colour}
#'       \item \code{upper_fill}
#'       \item \code{upper_linetype}
#'       \item \code{upper_size}
#'       \item \code{lower_r0}
#'       \item \code{lower_alpha}
#'       \item \code{lower_colour}
#'       \item \code{lower_fill}
#'       \item \code{lower_linetype}
#'       \item \code{lower_size}
#'    }
#' @importFrom ggplot2 layer ggproto GeomRect aes
#' @importFrom grid grobTree
#' @rdname geom_square
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_square <- function(mapping = NULL,
                        data = NULL,
                        stat = "identity",
                        position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSquare,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_square
#' @export
geom_upper_square <- function(mapping = NULL, data = get_data(type = "upper"),
                              stat = "identity", position = "identity",
                              ...,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomUpperSquare,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = aes_short_to_long(
      list(
        na.rm = na.rm,
        ...
      ), prefix = "upper", short_aes
    )
  )
}

#' @rdname geom_square
#' @export
geom_lower_square <- function(mapping = NULL, data = get_data(type = "lower"),
                              stat = "identity", position = "identity",
                              ...,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLowerSquare,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = aes_short_to_long(
      list(
        na.rm = na.rm,
        ...
      ), prefix = "lower", short_aes
    )
  )
}

#' @rdname geom_square
#' @format NULL
#' @usage NULL
#' @export
GeomSquare <- ggproto(
  "GeomSquare", GeomRect,
  default_aes = aes(r0 = 0.5, colour = "grey35", fill = NA, size = 0.25, linetype = 1,
                    alpha = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
    aesthetics <- setdiff(names(data), c("x", "y", "xmin", "ymin", "xmax", "ymax"))
    dd <- point_to_square(data$x, data$y, data$r0)
    data <- cbind(dd, data[, aesthetics, drop = FALSE])
    GeomRect$draw_panel(data, panel_params, coord)
  },

  draw_key = draw_key_square
)

#' @rdname geom_square
#' @format NULL
#' @usage NULL
#' @export
GeomUpperSquare <- ggproto(
  "GeomUpperSquare", GeomSquare,
  default_aes = aes(upper_r0 = 0.5, upper_colour = "grey35", upper_fill = NA,
                    upper_size = 0.25, upper_linetype = 1, upper_alpha = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
    data <- remove_short_aes(data, short_aes)
    data <- aes_long_to_short(data, "upper", long_aes_upper)
    GeomSquare$draw_panel(data, panel_params, coord)
  },

  draw_key = draw_key_upper_square
)

#' @rdname geom_square
#' @format NULL
#' @usage NULL
#' @export
GeomLowerSquare <- ggproto(
  "GeomLowSquare", GeomSquare,
  default_aes = aes(lower_r0 = 0.5, lower_colour = "grey35", lower_fill = NA,
                    lower_size = 0.25, lower_linetype = 1, lower_alpha = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
    data <- remove_short_aes(data, short_aes)
    data <- aes_long_to_short(data, "lower", long_aes_lower)
    GeomSquare$draw_panel(data, panel_params, coord)
  },

  draw_key = draw_key_lower_square
)

#' @noRd
point_to_square <- function(x, y, r0) {
  r0 <- 0.5 * sign(r0) * sqrt(abs(r0))
  n <- length(x)
  new_data_frame(list(
    xmin = - r0 + x,
    xmax = r0 + x,
    ymin = - r0 + y,
    ymax = r0 + y))
}

