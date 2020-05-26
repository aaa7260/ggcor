#' Annotation tile layer
#' @title Annotation tile layer
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_tile
#' @section Aesthetics:
#' \code{geom_anno_tile} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill0}
#'       \item \code{fill2}
#'       \item \code{linetype}
#'       \item \code{size}
#'    }
#' @importFrom ggplot2 layer ggproto GeomTile aes
#' @importFrom grid grobTree
#' @rdname geom_anno_tile
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_anno_tile <- function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  params <- list(na.rm = na.rm, ...)
  name <- names(params)
  if("fill" %in% name) {
    name[which(name == "fill")] <- "fill0"
  }
  names(params) <- name
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAnnoTile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname geom_anno_tile
#' @export
geom_anno_tile2 <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  params <- list(na.rm = na.rm, ...)
  name <- names(params)
  if("fill" %in% name) {
    name[which(name == "fill")] <- "fill2"
  }
  names(params) <- name
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAnnoTile2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname geom_anno_tile
#' @format NULL
#' @usage NULL
#' @export
GeomAnnoTile <- ggproto(
  "GeomAnnoTile", GeomTile,
  default_aes = aes(colour = NA, fill0 = "grey20", width = 1,
                    height = 1, size = 0.1, linetype = 1, alpha = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord) {
    data <- dplyr::rename(data, fill = fill0)
    GeomTile$draw_panel(data, panel_params, coord)
  },
  draw_key = draw_anno_tile
)

#' @rdname geom_anno_tile
#' @format NULL
#' @usage NULL
#' @export
GeomAnnoTile2 <- ggproto(
  "GeomAnnoTile2", GeomTile,
  default_aes = aes(colour = NA, fill2 = "grey20", width = 1,
                    height = 1, size = 0.1, linetype = 1, alpha = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord) {
    data <- dplyr::rename(data, fill = fill2)
    GeomTile$draw_panel(data, panel_params, coord)
  },
  draw_key = draw_anno_tile2
)

