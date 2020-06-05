#' Path2 layer
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#' @section Aesthetics:
#' \code{geom_path2()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{linetype}
#'       \item \code{size}
#'    }
#' @importFrom ggplot2 layer ggproto geom_path GeomPath aes draw_key_path
#' @rdname geom_path2
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_path2 <- function(mapping = NULL, 
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
    geom = GeomPath2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_path2
#' @format NULL
#' @usage NULL
#' @export
GeomPath2 <- ggproto(
  "GeomPath2", GeomPath,
  required_aes = c("x", "y"),
  non_missing_aes = c("linetype", "size", "shape"),
  default_aes = aes(
    colour = "black", size = 0.5, linetype = 1, alpha = NA
  ),
  
  draw_panel = function(data, panel_params, coord, lineend = "butt", 
                        linejoin = "round", na.rm = FALSE) {
    coord <- coord$transform(data, panel_params)
    ggname("geom_path2",
           grid::pathGrob(coord$x, coord$y, coord$group,
                    default.units = "native",
                    gp = grid::gpar(
                      col = scales::alpha(coord$colour, coord$alpha),
                      lwd = coord$size * ggplot2::.pt,
                      lty = coord$linetype,
                      lineend = lineend,
                      linejoin = linejoin
                      )
                    )
           )
    },
  draw_key = draw_key_path
)