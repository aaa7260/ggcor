#' Create a correlation plot
#' @description This function is the equivalent of \code{\link[ggplot2]{ggplot}}
#'     in ggplot2. It takes care of setting up the position of axis and legend for
#'     the plot based on the plot type.
#' @param data cor_tbl object.
#' @param mapping NULL (default) or a list of aesthetic mappings to use for plot,
#'     see Details.
#' @param axis.x.position,axis.y.position the position of the axis. 'auto' (default)
#'     is set according to the plot type, 'bottom' or 'top' for x axes, 'left' or 'right'
#'     for y axes.
#' @param axis.label.drop logical value (default is TRUE). When type of plot is 'upper'
#'     or 'lower' and 'show.diag' is FALSE, do you need to remove the blank coordinate
#'     label.
#' @return an object of class gg onto which layers, scales, etc. can be added.
#' @rdname ggcor
#' @examples
#' df <- fortify_cor(mtcars)
#' ggcor(df)
#' df01 <- fortify_cor(mtcars, type = "lower", show.diag = FALSE)
#' ggcor(df01, axis.label.drop = TRUE)
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
ggcor <- function(data,
                  mapping = NULL,
                  axis.x.position = "auto",
                  axis.y.position = "auto",
                  axis.label.drop = TRUE)
{
  if(!is_cor_tbl(data))
    stop("'data' needs a cor_tbl.", call. = FALSE)
  type <- cor_tbl_type(data)
  show.diag <- cor_tbl_showdiag(data)
  xname <- cor_tbl_xname(data)
  yname <- cor_tbl_yname(data)
  base_map <- aes_string("x", "y")
  mapping <- if(is.null(mapping)) base_map else modifyList(base_map, mapping)
  # handle axis setting
  axis.x.position <- match.arg(axis.x.position, c("auto", "bottom", "top"))
  axis.y.position <- match.arg(axis.y.position, c("auto", "left", "right"))
  if(axis.x.position == "auto") {
    axis.x.position <- switch (type,
                               full = "bottom",
                               lower = "bottom",
                               upper = "top")
  }
  if(axis.y.position == "auto") {
    axis.y.position <- switch (type,
                               full = "left",
                               lower = "left",
                               upper = "right")
  }
  axis.x.breaks <- 1:length(xname)
  axis.x.labels <- xname
  axis.y.breaks <- 1:length(yname)
  axis.y.labels <- yname
  if(axis.label.drop) {
    if(isFALSE(show.diag)) {
      if(type == "upper") {
        axis.x.breaks <- axis.x.breaks[-1]
        axis.x.labels <- axis.x.labels[-1]
        axis.y.breaks <- axis.y.breaks[-1]
        axis.y.labels <- axis.y.labels[-1]
      }
      if(type == "lower") {
        axis.x.breaks <- axis.x.breaks[-length(xname)]
        axis.x.labels <- axis.x.labels[-length(xname)]
        axis.y.breaks <- axis.y.breaks[-length(yname)]
        axis.y.labels <- axis.y.labels[-length(yname)]
      }
    }
  }
  p <- ggplot(data = data, mapping = mapping, environment = parent.frame()) +
    scale_x_continuous(breaks = axis.x.breaks, labels = axis.x.labels,
                       position = axis.x.position)+
    scale_y_continuous(breaks = axis.y.breaks, labels = axis.y.labels,
                       position = axis.y.position)
  class(p) <- c("ggcor", class(p))
  p
}
