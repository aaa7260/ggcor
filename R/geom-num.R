#' Formats texts based on center.
#'
#'
#' @eval rd_aesthetics("geom", "num")
#' @param digits integer indicating the number of decimal places (round) or
#'     significant digits (signif) to be used, the default value is 2.
#' @param nsmall the minimum number of digits to the right of the decimal
#'     point in formatting real/complex numbers in non-scientific formats,
#'     the default value is 2.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @rdname geom_num
#' @export
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 position_nudge
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 GeomText
#' @importFrom ggplot2 draw_key_text
geom_num <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      nudge_x = 0,
                      nudge_y = 0,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomNum,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_num
#' @format NULL
#' @usage NULL
#' @export
GeomNum <- ggproto("GeomNum",
                   GeomText,
                   required_aes = c("x", "y", "num"),

                   default_aes = aes(
                     colour = "black", size = 3.88, angle = 0, hjust = 0.5,
                     vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2),

                   draw_panel = function(data, panel_params, coord, digits = 2, nsmall = 2,
                                         na.rm = FALSE) {
                     data$label <- format_number(data$num, digits, nsmall)
                     GeomText$draw_panel(data, panel_params, coord)
                    },

                    draw_key = draw_key_text
)



