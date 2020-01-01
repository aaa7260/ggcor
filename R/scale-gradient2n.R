#' Gradient n colour scales
#'
#' `scale_*_gradient2n` creates a n colour gradient (low-high), mixed some
#' features of `scale_*_gradient2` and `scale_*_gradientn`.
#'
#' @inheritParams scales::seq_gradient_pal
#' @inheritParams ggplot2::scale_fill_gradient
#' @seealso [scales::seq_gradient_pal()] for details on underlying
#'   palette
#' @importFrom ggplot2 continuous_scale
#' @rdname scale_colour_gradient2n
#' @export
#' @examples
#' df <- data.frame(x = rep(1:10, 10),
#'                  y = rep(1:10, each = 10),
#'                  z = runif(100, -1, 1))
#' library(ggplot2)
#' ggplot(df, aes(x, y, fill = z)) +
#'        geom_tile( ) +
#'        scale_fill_gradient2n( )
#'
#' ggplot(df, aes(x, y, colour = z)) +
#'        geom_point(size = 4) +
#'        scale_colour_gradient2n( )
scale_colour_gradient2n <- function(...,
                                    colours,
                                    midpoint = 0,
                                    limits  = NULL,
                                    space = "Lab",
                                    values = NULL,
                                    na.value = "grey50",
                                    guide = "colourbar",
                                    aesthetics = "colour",
                                    colors = NULL) {
  colours <- if (missing(colours)) colors else colours
  colours <- colours %||% red_blue()
  continuous_scale(aesthetics,
                   "gradient2n",
                   scales::gradient_n_pal(colours, values, space),
                   na.value = na.value,
                   guide = guide,
                   ...,
                   limits = limits,
                   rescaler = mid_rescaler(mid = midpoint))
}

#' @rdname scale_colour_gradient2n
#' @export
scale_color_gradient2n <- scale_colour_gradient2n

#' @rdname scale_colour_gradient2n
#' @export
scale_fill_gradient2n <- function(...,
                                  colours,
                                  midpoint = 0,
                                  limits = NULL,
                                  space = "Lab",
                                  values = NULL,
                                  na.value = "grey50",
                                  guide = "colourbar",
                                  aesthetics = "fill",
                                  colors = NULL) {
  colours <- if (missing(colours)) colors else colours
  colours <- colours %||% red_blue()
  continuous_scale(aesthetics,
                   "gradient2n",
                   scales::gradient_n_pal(colours, values, space),
                   na.value = na.value,
                   guide = guide,
                   ...,
                   limits = limits,
                   rescaler = mid_rescaler(mid = midpoint))
}

#' @noRd
mid_rescaler <- function(mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}

