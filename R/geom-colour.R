#' Colour Geom
#' @description This is the encapsulation of \code{geom_tile()}
#' function, and some fine-tuning has been done.
#' @param colour the colour of tile boder.
#' @inheritParams ggplot2::geom_tile
#' @importFrom ggplot2 geom_tile
#' @section Aesthetics:
#' \code{geom_colour()}, \code{geom_upper_colour()} and \code{geom_lower_colour()}
#' understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{linetype}
#'       \item \code{size}
#'       \item \code{width}
#'       \item \code{height}
#'       \item \code{upper_alpha}
#'       \item \code{upper_colour}
#'       \item \code{upper_fill}
#'       \item \code{upper_linetype}
#'       \item \code{upper_size}
#'       \item \code{upper_width}
#'       \item \code{upper_height}
#'       \item \code{lower_alpha}
#'       \item \code{lower_colour}
#'       \item \code{lower_fill}
#'       \item \code{lower_linetype}
#'       \item \code{lower_size}
#'       \item \code{lower_width}
#'       \item \code{lower_height}
#'    }
#' @rdname geom_colour
#' @examples
#' quickcor(mtcars) + geom_colour()
#' @seealso \code{\link[ggplot2]{geom_tile}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_colour <- function(mapping = NULL,
                        data = NULL,
                        colour = "grey60",
                        ...)
{
  geom_tile(mapping = mapping,
            data = data,
            colour = colour,
            ...)
}

#' @rdname geom_colour
#' @export
geom_color <- geom_colour

#' @rdname geom_colour
#' @export
geom_upper_colour <- function(mapping = NULL, data = get_data(type = "upper"),
                              stat = "identity", position = "identity",
                              ...,
                              colour = "grey60",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomUpperColour,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = aes_short_to_long(
      list(
        colour = colour,
        na.rm = na.rm,
        ...
      ), prefix = "upper", short_aes
    )
  )
}

#' @rdname geom_colour
#' @export
geom_upper_color <- geom_upper_colour

#' @rdname geom_colour
#' @export
geom_lower_colour <- function(mapping = NULL, data = get_data(type = "lower"),
                              stat = "identity", position = "identity",
                              ...,
                              colour = "grey60",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLowerColour,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = aes_short_to_long(
      list(
        colour = colour,
        na.rm = na.rm,
        ...
      ), prefix = "lower", short_aes
    )
  )
}

#' @rdname geom_colour
#' @export
geom_lower_color <- geom_lower_colour

#' @importFrom ggplot2 GeomTile
#' @rdname geom_colour
#' @format NULL
#' @usage NULL
#' @export
GeomUpperColour <- ggproto(
  "GeomUpperColour", GeomTile,
  default_aes = aes(upper_colour = NA, upper_fill = "grey20", upper_size = 0.1,
                    upper_linetype = 1, upper_alpha = NA, upper_width = NA,
                    upper_height = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
    data <- remove_short_aes(data, short_aes)
    data <- aes_long_to_short(data, "upper", long_aes_upper)
    GeomTile$draw_panel(data, panel_params, coord)
  },

  draw_key = draw_key_upper_colour
)

#' @importFrom ggplot2 GeomTile
#' @rdname geom_colour
#' @format NULL
#' @usage NULL
#' @export
GeomLowerColour <- ggproto(
  "GeomLowColour", GeomTile,
  default_aes = aes(lower_colour = NA, lower_fill = "grey20", lower_size = 0.1,
                    lower_linetype = 1, lower_alpha = NA, lower_width = NA,
                    lower_height = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
    data <- remove_short_aes(data, short_aes)
    data <- aes_long_to_short(data, "lower", long_aes_lower)
    GeomTile$draw_panel(data, panel_params, coord)
  },

  draw_key = draw_key_lower_colour
)
