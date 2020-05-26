#' Row fill scales
#' @title Row fill scales
#' This set of scales defines stuff scales for equivalent to the
#' ones already defined by ggplot2.
#' @param type one of ""gradient" or "viridis".
#' @inheritParams ggplot2::scale_fill_gradient
#' @param low,high Colours for low and high ends of the gradient.
#' @inheritParams ggplot2::scale_fill_hue
#' @return A ggproto object inheriting from `Scale`
#' @rdname scale_fill0
#' @importFrom scales hue_pal
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
scale_fill0_continuous <- function (..., type = "gradient")
{
  switch(type,
         gradient = scale_fill0_gradient(...),
         viridis = ggplot2::scale_fill_viridis_c(...),
         rlang::abort("Unknown scale type"))
}

#' @rdname scale_fill0
#' @export
scale_fill0_discrete <- function (...,
                                  h = c(0, 360) + 15,
                                  c = 100, l = 65,
                                  h.start = 0,
                                  direction = 1,
                                  na.value = "grey50",
                                  aesthetics = "fill0")
{
  discrete_scale(aesthetics, "hue",
                 hue_pal(h, c, l, h.start, direction), na.value = na.value, ...)
}

#' @rdname scale_fill0
#' @export
scale_fill0_hue <- function(...,
                            h = c(0, 360) + 15,
                            c = 100, l = 65,
                            h.start = 0,
                            direction = 1,
                            na.value = 'grey50') {
  discrete_scale("fill0", "hue", hue_pal(h, c, l, h.start, direction),
                 na.value = na.value, ...
  )
}

#' @importFrom scales seq_gradient_pal
#' @importFrom ggplot2 continuous_scale
#' @rdname scale_fill0
#' @export
scale_fill0_gradient <- function(...,
                                 low = "#132B43",
                                 high = "#56B1F7",
                                 space = "Lab",
                                 na.value = "grey50",
                                 guide = "colourbar2") {
  continuous_scale("fill0", "gradient", seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...
  )
}

#' @inheritParams ggplot2::scale_fill_gradient2
#'
#' @importFrom scales div_gradient_pal muted
#' @importFrom ggplot2 continuous_scale
#' @rdname scale_fill0
#' @export
scale_fill0_gradient2 <- function(...,
                                  low = muted("red"),
                                  mid = "white",
                                  high = muted("blue"),
                                  midpoint = 0,
                                  space = "Lab",
                                  guide = "colourbar2",
                                  na.value = "grey50") {
  continuous_scale("fill0", "gradient2",
                   div_gradient_pal(low, mid, high, space),
                   na.value = na.value, ..., guide = guide,
                   rescaler = mid_rescaler(mid = midpoint)
  )
}

#' @inheritParams ggplot2::scale_fill_gradientn
#' @param colours,colors Vector of colours to use for n-colour gradient.
#' @importFrom scales gradient_n_pal
#' @importFrom ggplot2 continuous_scale
#' @rdname scale_fill0
#' @export
scale_fill0_gradientn <- function(...,
                                  colours,
                                  values = NULL,
                                  guide = "colourbar2",
                                  space = "Lab",
                                  na.value = "grey50",
                                  colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale("fill0", "gradientn",
                   gradient_n_pal(colours, values, space),
                   na.value = na.value, guide = guide, ...
  )
}

#' @inheritParams ggplot2::scale_fill_identity
#' @importFrom scales identity_pal
#' @importFrom ggplot2 discrete_scale ScaleDiscreteIdentity
#' @rdname scale_fill0
#' @export
scale_fill0_identity <- function(...) {
  sc <- discrete_scale("fill0", "identity", identity_pal(), ...,
                       super = ScaleDiscreteIdentity
  )
  sc
}

#' @inheritParams ggplot2::scale_fill_manual
#' @rdname scale_fill0
#' @export
scale_fill0_manual <- function(...,
                               values,
                               aesthetics = "fill0") {
  manual_scale(aesthetics, values, ...)
}

#' Column fill scales
#' @title Column fill scales
#' This set of scales defines stuff scales for equivalent to the
#' ones already defined by ggplot2.
#' @param type one of "gradient" or "viridis".
#' @inheritParams ggplot2::scale_fill_gradient
#' @param low,high Colours for low and high ends of the gradient.
#' @inheritParams ggplot2::scale_fill_hue
#' @return A ggproto object inheriting from `Scale`
#' @rdname scale_fill2
#' @importFrom scales hue_pal
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
scale_fill2_continuous <- function (..., type = "gradient")
{
  switch(type,
         gradient = scale_fill2_gradient(...),
         viridis = ggplot2::scale_fill_viridis_c(...),
         rlang::abort("Unknown scale type"))
}

#' @rdname scale_fill2
#' @export
scale_fill2_discrete <- function (...,
                                  h = c(0, 360) + 15,
                                  c = 100, l = 65,
                                  h.start = 0,
                                  direction = 1,
                                  na.value = "grey50",
                                  aesthetics = "fill2")
{
  discrete_scale(aesthetics, "hue",
                 hue_pal(h, c, l, h.start, direction), na.value = na.value, ...)
}

#' @rdname scale_fill2
#' @export
scale_fill2_hue <- function(...,
                            h = c(0, 360) + 15,
                            c = 100, l = 65,
                            h.start = 0,
                            direction = 1,
                            na.value = 'grey50') {
  discrete_scale("fill2", "hue", hue_pal(h, c, l, h.start, direction),
                 na.value = na.value, ...
  )
}

#' @importFrom scales seq_gradient_pal
#' @importFrom ggplot2 continuous_scale
#' @rdname scale_fill2
#' @export
scale_fill2_gradient <- function(...,
                                 low = "#132B43",
                                 high = "#56B1F7",
                                 space = "Lab",
                                 na.value = "grey50",
                                 guide = "colourbar2") {
  continuous_scale("fill2", "gradient", seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...
  )
}

#' @inheritParams ggplot2::scale_fill_gradient2
#'
#' @importFrom scales div_gradient_pal muted
#' @importFrom ggplot2 continuous_scale
#' @rdname scale_fill2
#' @export
scale_fill2_gradient2 <- function(...,
                                  low = muted("red"),
                                  mid = "white",
                                  high = muted("blue"),
                                  midpoint = 0,
                                  space = "Lab",
                                  guide = "colourbar2",
                                  na.value = "grey50") {
  continuous_scale("fill2", "gradient2",
                   div_gradient_pal(low, mid, high, space),
                   na.value = na.value, ..., guide = guide,
                   rescaler = mid_rescaler(mid = midpoint)
  )
}

#' @inheritParams ggplot2::scale_fill_gradientn
#' @param colours,colors Vector of colours to use for n-colour gradient.
#' @importFrom scales gradient_n_pal
#' @importFrom ggplot2 continuous_scale
#' @rdname scale_fill2
#' @export
scale_fill2_gradientn <- function(...,
                                  colours,
                                  values = NULL,
                                  guide = "colourbar2",
                                  space = "Lab",
                                  na.value = "grey50",
                                  colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale("fill2", "gradientn",
                   gradient_n_pal(colours, values, space),
                   na.value = na.value, guide = guide, ...
  )
}

#' @inheritParams ggplot2::scale_fill_identity
#' @importFrom scales identity_pal
#' @importFrom ggplot2 discrete_scale ScaleDiscreteIdentity
#' @rdname scale_fill2
#' @export
scale_fill2_identity <- function(...) {
  sc <- discrete_scale("fill2", "identity", identity_pal(), ...,
                       super = ScaleDiscreteIdentity
  )
  sc
}

#' @inheritParams ggplot2::scale_fill_manual
#' @rdname scale_fill2
#' @export
scale_fill2_manual <- function(...,
                               values,
                               aesthetics = "fill2") {
  manual_scale(aesthetics, values, ...)
}

#' @noRd
manual_scale <- function (aesthetic, values, ...)
{
  pal <- function(n) {
    if (n > length(values)) {
      stop("Insufficient values in manual scale. ", n,
           " needed but only ", length(values), " provided.",
           call. = FALSE)
    }
    values
  }
  discrete_scale(aesthetic, "manual", pal, ...)
}
