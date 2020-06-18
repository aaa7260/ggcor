#' Set the style of correlation matrix plot
#' @title Set style
#' @param colours NULL or colours.
#' @param type a scale function or character of scale name (should be one of 
#' "gradient", "viridis" or "gradient2n").
#' @param legend.title a string of colour bar title.
#' @param nbin a scala integer.
#' @param ... extra parameters for scale function.
#' @importFrom ggplot2 guides guide_colorbar scale_fill_gradient scale_fill_viridis_c
#' @rdname set_style
#' @examples 
#' set_style()
#' quickcor(mtcars) + geom_square()
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
set_style <- function(colours = NULL,
                      type = "gradient2n",
                      legend.title = "Corr",
                      nbin = 40,
                      ...
                      ) {
  if(!is.function(type)) {
    if(!type %in% c("gradient", "viridis", "gradient2n")) {
      stop("Unknown scale type.", call. = FALSE)
    }
  }
  if(is.null(colours)) {
    colours <- red_blue()
  }
  scale <- if(is.function(type)) {
    do.call(type, list(...))
  } else if(identical(type, "gradient")) {
    scale_fill_gradient(...)
  } else if(identical(type, "viridis")) {
    scale_fill_viridis_c(...)
  } else {
    scale_fill_gradient2n(colours = colours, ...)
  }
  guide <- if(inherits(scale, "ScaleBinned")) {
    guides(fill = guide_colorsteps(title = legend.title,
                                            nbin  = nbin))
  } else {
    guides(fill = ggplot2::guide_colorsteps(title = legend.title))
  }
  options(ggcor.style = list(scale, guide))
}

#' @rdname set_style
#' @export
reset_style <- function() {
  options(ggcor.style = NULL)
}