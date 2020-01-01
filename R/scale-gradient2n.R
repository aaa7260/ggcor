#' Gradient n colour scales
#'
#' `scale_*_gradient2n` creates a n colour gradient (low-high), mixed some
#' features of `scale_*_gradient2` and `scale_*_gradientn`.
#'
#' Default colours are `c("#67001F", "#B2182B", "#D6604D", "#F4A582","#FDDBC7",
#' "#FFFFFF", "#D1E5F0", "#92C5DE","#4393C3", "#2166AC", "#053061")` , borrowed
#' from \pkg{corrplot}.
#'
#' @inheritParams scales::seq_gradient_pal
#' @inheritParams ggplot2::scale_colour_hue
#' @param guide Type of legend. Use `"colourbar"` for continuous
#'   colour bar, or `"legend"` for discrete colour legend.
#' @inheritDotParams ggplot2::continuous_scale -na.value -guide -aesthetics
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

#' #' @rdname scale_colour_gradient2n
#' #' @export
#' scale_fill_steps2n <- function(...,
#'                                colours,
#'                                midpoint = 0,
#'                                values = NULL,
#'                                space = "Lab",
#'                                limits = c(-1, 1),
#'                                na.value = "grey50",
#'                                guide = "coloursteps",
#'                                aesthetics = "fill",
#'                                colors = NULL)
#' {
#'   colours <- if (missing(colours)) colors else colours
#'   colours <- colours %||% red_blue()
#'   ggplot2:::binned_scale(aesthetics, "steps2n", scales::gradient_n_pal(colours, values, space),
#'                          limits = limits, na.value = na.value, guide = guide,
#'                          rescaler = mid_rescaler(mid = midpoint), ...)
#' }
#'
#' #' @rdname scale_colour_gradient2n
#' #' @export
#' scale_colour_steps2n <- function(...,
#'                                  colours,
#'                                  midpoint = 0,
#'                                  values = NULL,
#'                                  space = "Lab",
#'                                  limits = c(-1, 1),
#'                                  na.value = "grey50",
#'                                  guide = "coloursteps",
#'                                  aesthetics = "colour",
#'                                  colors = NULL)
#' {
#'   colours <- if (missing(colours)) colors else colours
#'   colours <- colours %||% red_blue()
#'   ggplot2:::binned_scale(aesthetics, "steps2", scales::gradient_n_pal(colours, values, space),
#'                          limits = limits, na.value = na.value, guide = guide,
#'                          rescaler = mid_rescaler(mid = midpoint), ...)
#' }
#'
#' #' @rdname scale_colour_gradient2n
#' #' @export
#' scale_color_steps2n <- scale_colour_steps2n
#' @noRd
mid_rescaler <- function(mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}

