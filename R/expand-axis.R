#' @export
expand_axis <- function(x = NULL, y = NULL)
{
  reset_axis_lim <- function(p) {
    if(is.null(x) && is.null(y)) return(p)
    if(!is.null(x)) {
      if(!is.numeric(x))
        stop("'x' must be a numeric vector.", call. = FALSE)
      xlim <- p$coordinates$limits$x
      p$coordinates$limits$x <- c(min(xlim, x, na.rm = TRUE),
                                  max(xlim, x, na.rm = TRUE))
    }
    if(!is.null(y)) {
      if(!is.numeric(y))
        stop("'y' must be a numeric vector.", call. = FALSE)
      ylim <- p$coordinates$limits$y
      p$coordinates$limits$y <- c(min(ylim, y, na.rm = TRUE),
                                  max(ylim, y, na.rm = TRUE))
    }
    p
  }
  class(reset_axis_lim) <- c("expand_axis", class(reset_axis_lim))
  reset_axis_lim
}

#' @export
ggplot_add.expand_axis <- function(object, plot, object_name) {
  object(plot)
}