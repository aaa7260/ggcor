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
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{linetype}
#'       \item \code{size}
#'    }
#' @importFrom ggplot2 layer ggproto geom_segment aes draw_key_path
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
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_segment2
#' @format NULL
#' @usage NULL
#' @export
GeomSegment2 <- ggproto(
  "GeomSegment2", GeomPath2,
  required_aes = c("x", "y", "xend", "yend"),
  non_missing_aes = c("linetype", "size", "shape"),
  default_aes = aes(
    colour = "black", size = 0.5, linetype = 1, alpha = NA
  ),
  
  draw_panel = function(data, panel_params, coord, lineend = "butt", 
                        linejoin = "round", na.rm = FALSE) {
    data$group <- 1:nrow(data)
    starts <- data[setdiff(names(data), c("xend", "yend"))]
    xend <- yend <- NULL
    ends <- dplyr::rename(data[setdiff(names(data), c("x", "y"))], x = xend, y = yend)
    
    data <- rbind(starts, ends)
    coord <- coord$transform(data, panel_params)
    GeomPath2$draw_panel(data, panel_params, coord)
  },
  draw_key = draw_key_path
)