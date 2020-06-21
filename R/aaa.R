#' @noRd
get_function <- function(pkg, fun) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    stop(pkg, " package has not been installed", call. = FALSE)
  }
  eval(parse(text = paste0(pkg, "::", fun)))
}

#' @noRd
new_scales <- function(scales) {
  new_scale <- get_function("ggnewscale", "new_scale")
  if(length(scales) == 1) {
    new_scale(scales)
  } else {
    lapply(scales, new_scale)
  }
}