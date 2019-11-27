#' @export
scale_radius_continuous <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                    limits = NULL, range = c(-1, 1),
                                    trans = "identity", guide = NULL) {
  continuous_scale("r", "radius", abs_area_pal(range), name = name, rescaler = rescale2(),
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
rescale2 <- function(from) {
  function(x, to = c(-1, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid = 0)
  }
}