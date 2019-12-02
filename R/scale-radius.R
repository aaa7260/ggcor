#' @export
scale_abs_area_radius <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                  limits = NULL, range = c(-1, 1), mid = 0,
                                  trans = "identity", guide = NULL) {
  continuous_scale("r0", "abs_area_radius", abs_area_pal(range), name = name, rescaler = mid_rescale2(mid),
                   breaks = breaks, labels = labels, limits = limits, trans = trans,
                   guide = guide)
}

scale_area_radius <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                  limits = NULL, max = 1, mid = 0,
                                  trans = "identity", guide = NULL) {
  continuous_scale("r0", "area_radius", area_pal(max), name = name, rescaler = mid_rescale2(mid),
                   breaks = breaks, labels = labels, limits = limits, trans = trans,
                   guide = guide)
}

#' @export
scale_rho <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                      limits = NULL, range = c(-1, 1), mid = 0,
                      trans = "identity", guide = NULL) {
  continuous_scale("rho", "rho", rescale_pal2(range), 
                   name = name, rescaler = mid_rescale2(mid),
                   breaks = breaks, labels = labels, limits = limits, trans = trans,
                   guide = guide)
}

#' @export
scale_confidence <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                      limits = NULL, max = 0.6, conf.level = 0.05, trans = "identity", 
                      guide = NULL) {
  di("p.value", "confidence", conf_pal(max = max, conf.level = conf.level), name = name, 
                   breaks = breaks, labels = labels, limits = limits, trans = trans, 
                   guide = guide)
}

#' @noRd
abs_area_pal <- function (range = c(-1, 1)) 
{
  force(range)
  function(x) {
    scales::rescale(sign(x) * sqrt(abs(x)), range, c(-1, 1))
  }
}

#' @noRd
area_pal <- function (max = 1) 
{
  force(max)
  function(x) {
    scales::rescale(sign(x) * sqrt(abs(x)), c(0, max), c(-1, 1))
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

#' @noRd
conf_pal <- function (max = 0.6, conf.level = 0.05) 
{
  force(max)
  force(conf.level)
  function(x) {
    ifelse(x >= conf.level, max, 0)
  }
}

#' @noRd
mid_rescale2 <- function(mid) {
  function(x, to = c(-1, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}

