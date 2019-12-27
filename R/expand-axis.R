#' Expand axis limits
#' @description Force to extend the coordinate range of the ggplot object.
#' @param x,y NULL (default) or numeric vector.
#' @rdname expand_axis
#' @examples
#' quickcor(mtcars) + geom_square() + expand_axis(x = -3)
#' quickcor(mtcars) + geom_square() + expand_axis(y = 16)
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
expand_axis <- function(x = NULL, y = NULL)
{
  reset_axis_lim <- function(p) {
    if(!is.null(x) && !is.numeric(x)) x <- NULL
    if(!is.null(y) && !is.numeric(y)) y <- NULL
    if(is.null(x) && is.null(y)) return(p)
    xlim <- p$coordinates$limits$x
    ylim <- p$coordinates$limits$y
    if(!is.null(x)) {
      p$coordinates$limits$x <- c(min(xlim, x, na.rm = TRUE),
                                  max(xlim, x, na.rm = TRUE))
    }
    if(!is.null(y)) {
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
