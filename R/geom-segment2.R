#' Segment2 layer
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_segment
#' @section Aesthetics:
#' \code{geom_segment2()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \strong{\code{xend}}
#'       \item \strong{\code{yend}}
#'       \item \code{edge_alpha}
#'       \item \code{edge_colour}
#'       \item \code{edge_linetype}
#'       \item \code{edge_width}
#'    }
#' @importFrom ggplot2 layer ggproto geom_segment GeomSegment aes
#' @rdname geom_segment2
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_segment2 <- function(mapping = NULL, 
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
    geom = GeomSegment2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = short_to_long(
      list(
        na.rm = na.rm,
        ...
      )
    )
  )
}

#' @rdname geom_segment2
#' @format NULL
#' @usage NULL
#' @export
GeomSegment2 <- ggproto(
  "GeomSegment2", GeomSegment,
  default_aes = aes(edge_colour = "grey35", edge_width = 0.5, edge_linetype = 1,
                    edge_alpha = NA),
  required_aes = c("x", "y", "xend", "yend"),
  draw_panel = function(data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE) {
    if(empty(data)) {
      return(ggplot2::zeroGrob())
    }
    coords <- coord$transform(data, panel_params)
    arrow.fill <- arrow.fill %||% coords$edge_colour
    x <- y <- xend <- yend <- NULL
    ends <- dplyr::rename(data[setdiff(names(data), c("x", "y"))], x = xend, y = yend)
    ends <- coord$transform(ends, panel_params)
    ends <- dplyr::rename(ends, xend = x, yend = y)
    coords <- cbind(coords[setdiff(names(coords), c("xend", "yend"))], ends[c("xend", "yend")])
    ggname("geom_segment2",
           grid::segmentsGrob(coords$x, coords$y, coords$xend, coords$yend,
                 default.units = "native",
                 gp = grid::gpar(
                   col = scales::alpha(coords$edge_colour, coords$edge_alpha),
                   fill = scales::alpha(arrow.fill, coords$edge_alpha),
                   lwd = coords$edge_width * ggplot2::.pt,
                   lty = coords$edge_linetype,
                   lineend = lineend,
                   linejoin = linejoin
                 ),
                 arrow = arrow
                 )
           )
  },
  draw_key = draw_key_path2
)

#' @noRd
short_to_long <- function(params) {
  nm <- names(params)
  if("color" %in% nm) {
    nm[which(nm == "color")] <- "colour"
  }
  if("size" %in% nm) {
    nm[which(nm == "size")] <- "width"
  }
  id <- nm %in% c("colour", "width", "linetype", "size")
  nm[id] <- paste0("edge_", nm[id])
  names(params) <- nm
  params
}
