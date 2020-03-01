#' Triangle colour scales
#'
#' This set of scales defines new fill scales for triangel geoms equivalent to the
#' ones already defined by ggplot2.
#'
#' @return A ggproto object inheriting from `Scale`
#' @inheritParams ggplot2::scale_fill_gradient
#' @param low,high Colours for low and high ends of the gradient.
#' @inheritParams ggplot2::scale_fill_hue
#'
#' @rdname scale_upper_fill
#' @importFrom scales hue_pal
#' @export
scale_upper_fill_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1, na.value = 'grey50') {
  discrete_scale('upper_fill', 'hue', hue_pal(h, c, l, h.start, direction),
                 na.value = na.value, ...
  )
}

#' @rdname scale_upper_fill
#'
#' @importFrom scales seq_gradient_pal
#' @importFrom ggplot2 continuous_scale
#' @export
scale_upper_fill_gradient <- function(..., low = "#132B43", high = "#56B1F7",
                                      space = "Lab", na.value = "grey50", guide = "colourbar2") {
  continuous_scale("upper_fill", "gradient", seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...
  )
}
#' @rdname scale_upper_fill
#'
#' @inheritParams ggplot2::scale_fill_gradient2
#'
#' @importFrom scales div_gradient_pal muted
#' @importFrom ggplot2 continuous_scale
#' @export
scale_upper_fill_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                       midpoint = 0, space = "Lab", guide = "colourbar2", na.value = "grey50") {
  continuous_scale("upper_fill", "gradient2",
                   div_gradient_pal(low, mid, high, space),
                   na.value = na.value, ..., guide = guide,
                   rescaler = mid_rescaler(mid = midpoint)
  )
}
#' @rdname scale_upper_fill
#'
#' @inheritParams ggplot2::scale_fill_gradientn
#' @param colours,colors Vector of colours to use for n-colour gradient.
#'
#' @importFrom scales gradient_n_pal
#' @importFrom ggplot2 continuous_scale
#' @export
scale_upper_fill_gradientn <- function(..., colours, values = NULL, guide = "colourbar2",
                                       space = "Lab", na.value = "grey50", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale("upper_fill", "gradientn",
                   gradient_n_pal(colours, values, space),
                   na.value = na.value, guide = guide, ...
  )
}

#' @rdname scale_upper_fill
#'
#' @inheritParams ggplot2::scale_fill_identity
#'
#' @importFrom scales identity_pal
#' @importFrom ggplot2 discrete_scale ScaleDiscreteIdentity
#' @export
scale_upper_fill_identity <- function(...) {
  sc <- discrete_scale("upper_fill", "identity", identity_pal(), ...,
                       super = ScaleDiscreteIdentity
  )
  sc
}
#' @rdname scale_upper_fill
#'
#' @inheritParams ggplot2::scale_fill_manual
#' @export
scale_upper_fill_manual <- function(..., values, aesthetics = "upper_fill") {
  manual_scale(aesthetics, values, ...)
}

#' @rdname scale_upper_fill
#'
#' @inheritParams ggplot2::scale_fill_gradient
#' @inheritParams ggplot2::scale_fill_hue
#' @importFrom scales hue_pal
#' @export
scale_lower_fill_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1, na.value = 'grey50') {
  discrete_scale('upper_fill', 'hue', hue_pal(h, c, l, h.start, direction),
                 na.value = na.value, ...
  )
}
#' @importFrom scales seq_gradient_pal
#' @importFrom ggplot2 continuous_scale
#' @rdname scale_upper_fill
#' @export
scale_lower_fill_gradient <- function(..., low = "#132B43", high = "#56B1F7",
                                      space = "Lab", guide = "colourbar2", na.value = "grey50") {
  continuous_scale("lower_fill", "gradient", seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...
  )
}
#' @rdname scale_upper_fill
#'
#' @inheritParams ggplot2::scale_fill_gradient2
#'
#' @importFrom scales div_gradient_pal muted
#' @importFrom ggplot2 continuous_scale
#' @export
scale_lower_fill_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                       midpoint = 0, space = "Lab", guide = "colourbar2", na.value = "grey50") {
  continuous_scale("lower_fill", "gradient2",
                   div_gradient_pal(low, mid, high, space),
                   na.value = na.value, guide = guide, ...,
                   rescaler = mid_rescaler(mid = midpoint)
  )
}
#' @rdname scale_upper_fill
#'
#' @inheritParams ggplot2::scale_fill_gradientn
#'
#' @importFrom scales gradient_n_pal
#' @importFrom ggplot2 continuous_scale
#' @export
scale_lower_fill_gradientn <- function(..., colours, values = NULL, space = "Lab",
                                       guide = "colourbar2", na.value = "grey50", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale("lower_fill", "gradientn",
                   gradient_n_pal(colours, values, space),
                   na.value = na.value, guide = guide, ...
  )
}

#' @rdname scale_upper_fill
#'
#' @inheritParams ggplot2::scale_fill_identity
#'
#' @importFrom scales identity_pal
#' @importFrom ggplot2 discrete_scale ScaleDiscreteIdentity
#' @export
scale_lower_fill_identity <- function(...) {
  sc <- discrete_scale("lower_fill", "identity", identity_pal(), ...,
                       super = ScaleDiscreteIdentity
  )
  sc
}

#' @rdname scale_upper_fill
#'
#' @inheritParams ggplot2::scale_fill_manual
#' @export
scale_lower_fill_manual <- function(..., values, aesthetics = "lower_fill") {
  manual_scale(aesthetics, values, ...)
}

#' @rdname scale_upper_fill
#' @export
scale_upper_fill_continuous <- scale_upper_fill_gradient

#' @rdname scale_upper_fill
#' @export
scale_upper_fill_discrete <- scale_upper_fill_hue

#' @rdname scale_upper_fill
#' @export
scale_lower_fill_continuous <- scale_lower_fill_gradient

#' @rdname scale_upper_fill
#' @export
scale_lower_fill_discrete <- scale_lower_fill_hue


#' Triangle colour scales
#'
#' This set of scales defines new colour scales for triangel geoms equivalent to the
#' ones already defined by ggplot2.
#'
#' @return A ggproto object inheriting from `Scale`
#' @inheritParams ggplot2::scale_colour_gradient
#' @inheritParams ggplot2::scale_colour_hue
#' @param low,high Colours for low and high ends of the gradient.
#' @rdname scale_upper_colour
#' @importFrom scales hue_pal
#' @export
scale_upper_colour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1, na.value = 'grey50') {
  discrete_scale('upper_colour', 'hue', hue_pal(h, c, l, h.start, direction),
                 na.value = na.value, ...
  )
}

#' @importFrom scales seq_gradient_pal
#' @importFrom ggplot2 continuous_scale
#' @rdname scale_upper_colour
#' @export
scale_upper_colour_gradient <- function(..., low = "#132B43", high = "#56B1F7",
                                        space = "Lab", na.value = "grey50", guide = "colourbar2") {
  continuous_scale("upper_colour", "gradient", seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...
  )
}
#' @rdname scale_upper_colour
#'
#' @inheritParams ggplot2::scale_colour_gradient2
#'
#' @importFrom scales div_gradient_pal muted
#' @importFrom ggplot2 continuous_scale
#' @export
scale_upper_colour_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                         midpoint = 0, space = "Lab", guide = "colourbar2", na.value = "grey50") {
  continuous_scale("upper_colour", "gradient2",
                   div_gradient_pal(low, mid, high, space),
                   na.value = na.value, ..., guide = guide,
                   rescaler = mid_rescaler(mid = midpoint)
  )
}
#' @rdname scale_upper_colour
#'
#' @inheritParams ggplot2::scale_colour_gradientn
#' @param colours,colors Vector of colours to use for n-colour gradient.
#'
#' @importFrom scales gradient_n_pal
#' @importFrom ggplot2 continuous_scale
#' @export
scale_upper_colour_gradientn <- function(..., colours, values = NULL, guide = "colourbar2",
                                         space = "Lab", na.value = "grey50", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale("upper_colour", "gradientn",
                   gradient_n_pal(colours, values, space),
                   na.value = na.value, guide = guide, ...
  )
}

#' @rdname scale_upper_colour
#'
#' @inheritParams ggplot2::scale_colour_identity
#'
#' @importFrom scales identity_pal
#' @importFrom ggplot2 discrete_scale ScaleDiscreteIdentity
#' @export
scale_upper_colour_identity <- function(...) {
  sc <- discrete_scale("upper_colour", "identity", identity_pal(), ...,
                       super = ScaleDiscreteIdentity
  )
  sc
}
#' @rdname scale_upper_colour
#'
#' @inheritParams ggplot2::scale_colour_manual
#' @export
scale_upper_colour_manual <- function(..., values, aesthetics = "upper_colour") {
  manual_scale(aesthetics, values, ...)
}

#' @rdname scale_upper_colour
#' @importFrom scales hue_pal
#' @export

scale_lower_colour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1, na.value = 'grey50') {
  discrete_scale('lower_colour', 'hue', hue_pal(h, c, l, h.start, direction),
                 na.value = na.value, ...
  )
}

#' @rdname scale_upper_colour
#'
#' @inheritParams ggplot2::scale_colour_gradient
#'
#' @importFrom scales seq_gradient_pal
#' @importFrom ggplot2 continuous_scale
#' @export
scale_lower_colour_gradient <- function(..., low = "#132B43", high = "#56B1F7",
                                        space = "Lab", guide = "colourbar2", na.value = "grey50") {
  continuous_scale("lower_colour", "gradient", seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...
  )
}
#' @rdname scale_upper_colour
#'
#' @inheritParams ggplot2::scale_colour_gradient2
#'
#' @importFrom scales div_gradient_pal muted
#' @importFrom ggplot2 continuous_scale
#' @export
scale_lower_colour_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                         midpoint = 0, space = "Lab", guide = "colourbar2", na.value = "grey50") {
  continuous_scale("lower_colour", "gradient2",
                   div_gradient_pal(low, mid, high, space),
                   na.value = na.value, guide = guide, ...,
                   rescaler = mid_rescaler(mid = midpoint)
  )
}
#' @rdname scale_upper_colour
#'
#' @inheritParams ggplot2::scale_colour_gradientn
#'
#' @importFrom scales gradient_n_pal
#' @importFrom ggplot2 continuous_scale
#' @export
scale_lower_colour_gradientn <- function(..., colours, values = NULL, space = "Lab",
                                         guide = "colourbar2", na.value = "grey50", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale("lower_colour", "gradientn",
                   gradient_n_pal(colours, values, space),
                   na.value = na.value, guide = guide, ...
  )
}

#' @rdname scale_upper_colour
#'
#' @inheritParams ggplot2::scale_colour_identity
#'
#' @importFrom scales identity_pal
#' @importFrom ggplot2 discrete_scale ScaleDiscreteIdentity
#' @export
scale_lower_colour_identity <- function(...) {
  sc <- discrete_scale("lower_colour", "identity", identity_pal(), ...,
                       super = ScaleDiscreteIdentity
  )
  sc
}

#' @rdname scale_upper_colour
#'
#' @inheritParams ggplot2::scale_colour_manual
#' @export
scale_lower_colour_manual <- function(..., values, aesthetics = "lower_colour") {
  manual_scale(aesthetics, values, ...)
}

#' @rdname scale_upper_colour
#' @export
scale_upper_colour_continuous <- scale_upper_colour_gradient

#' @rdname scale_upper_colour
#' @export
scale_upper_colour_discrete <- scale_upper_colour_hue

#' @rdname scale_upper_colour
#' @export
scale_lower_colour_continuous <- scale_lower_colour_gradient

#' @rdname scale_upper_colour
#' @export
scale_lower_colour_discrete <- scale_lower_colour_hue

#' @noRd
#' @importFrom ggplot2 discrete_scale
manual_scale <- function (aesthetic, values = NULL, ...)
{
  if (missing(values)) {
    values <- NULL
  }
  else {
    force(values)
  }
  pal <- function(n) {
    if (n > length(values)) {
      stop("Insufficient values in manual scale. ", n,
           " needed but only ", length(values), " provided.",
           call. = FALSE)
    }
    values
  }
  ggplot2::discrete_scale(aesthetic, "manual", pal, ...)
}

