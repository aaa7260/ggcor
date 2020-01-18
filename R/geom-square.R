#' Square Geom
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @section Aesthetics:
#'     \code{geom_square()} understands the following aesthetics (required
#'     aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{r0}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{linetype}
#'       \item \code{size}
#'    }
#' @importFrom ggplot2 layer ggproto GeomPolygon
#' @importFrom grid grobTree
#' @rdname geom_square
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_square <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
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
#' @format NULL
#' @usage NULL
#' @export
GeomSquare <- ggproto(
  "GeomSquare", GeomPolygon,
  default_aes = aes(r0 = 0.5, colour = "grey35", fill = NA, size = 0.25, linetype = 1,
                    alpha = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
    aesthetics <- setdiff(names(data), c("x", "y", "group", "subgroup"))
    dd <- point_to_square(data$x, data$y, data$r0)
    aes <- aes <- data[rep(1:nrow(data), 5) , aesthetics, drop = FALSE]
    GeomPolygon$draw_panel(cbind(dd, aes), panel_params, coord)
  },

  draw_key = draw_key_square
)

#' @noRd
point_to_square <- function(x, y, r0) {
  r0 <- 0.5 * sign(r0) * sqrt(abs(r0))
  n <- length(x)
  xmin <- - r0 + x
  xmax <- r0 + x
  ymin <- - r0 + y
  ymax <- r0 + y
  new_data_frame(list(
    x = c(xmin, xmax, xmax, xmin, xmin),
    y = c(ymax, ymax, ymin, ymin, ymax),
    group = rep(1:n, 5)
  ))
}

