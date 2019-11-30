#' @export
scale_abs_area_radius <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                  limits = NULL, range = c(-0.5, 0.5), mid = 0,
                                  trans = "identity", guide = NULL) {
  continuous_scale("r0", "radius", abs_area_pal(range), name = name, rescaler = mid_rescale2(mid),
                   breaks = breaks, labels = labels, limits = limits, trans = trans,
                   guide = guide)
}

scale_area_radius <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                  limits = NULL, max = 1, mid = 0,
                                  trans = "identity", guide = NULL) {
  continuous_scale("r", "radius", area_pal(max), name = name, rescaler = mid_rescale2(mid),
                   breaks = breaks, labels = labels, limits = limits, trans = trans,
                   guide = guide)
}

#' @export
scale_rho <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                      limits = NULL, range = c(-1, 1), mid = 0,
                      trans = "identity", guide = NULL) {
  continuous_scale("rho", "radius", rescale_pal2(range), 
                   name = name, rescaler = mid_rescale2(mid),
                   breaks = breaks, labels = labels, limits = limits, trans = trans,
                   guide = guide)
}

#' @noRd
abs_area_pal <- function (range = c(-0.5, 0.5)) 
{
  force(range)
  function(x) {
    scales::rescale(sign(x) * sqrt(abs(x)), range, c(-1, 1))
  }
}

#' @noRd
area_pal <- function (max = 0.5) 
{
  force(max)
  function(x) {
    scales::rescale(sign(x) * sqrt(abs(x)), c(0, max), c(-1, 1))
  }
}

#' @noRd
mid_rescale2 <- function(mid) {
  function(x, to = c(-1, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}

#' @noRd
rescale_pal2 <- function (range = c(-1, 1)) 
{
  force(range)
  function(x) {
    scales::rescale(x, range, c(-1, 1))
  }
}
