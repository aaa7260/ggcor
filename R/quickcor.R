#' Plot Correlation Matrix Quickly
#' @description quickcor is convenient wrapper for creating a number of different types
#' of correlation matrix plots because of adding some extra settings by default.
#' @param x,y matrix or data frame.
#' @param mapping NULL (default) or a list of aesthetic mappings to use for plot.
#' @param fill.colours NULL (default) or a vector of colours to use for n-colour gradient.
#' @param fill.bin logical value (default is FALSE). if TRUE, If TRUE, use the function
#'     \code{ggcor::scale_fill_steps2n}, otherwise use the function
#'     \code{ggcor::scale_fill_gradient2n} for fill colour scale.
#' @param grid.colour colour of grid lines.
#' @param grid.size size of grid lines.
#' @param axis.x.position,axis.y.position the position of the axis. 'auto' (default)
#'     is set according to the plot type, 'bottom' or 'top' for x axes, 'left' or 'right'
#'     for y axes.
#' @param axis.label.drop logical value (default is TRUE). When type of plot is 'upper'
#'     or 'lower' and 'show.diag' is FALSE, do you need to remove the blank coordinate
#'     label.
#' @param legend.title title of colour bar.
#' @param legend.position position of legend.
#' @param legend.breaks breaks of colour bar.
#' @param legend.labels labels of colour bar.
#' @param ... extra params for \code{\link[ggcor]{fortify_cor}}.
#' @importFrom ggplot2 aes_string ggplot ggplot_add scale_x_continuous scale_y_continuous guides
#'     guide_colorsteps guide_colourbar coord_fixed
#' @rdname quick_cor
#' @examples
#' quickcor(mtcars)
#' quickcor(mtcars, type = "upper")
#' quickcor(mtcars, type = "lower", show.diag = FALSE)
#' quickcor(mtcars) + geom_colour()
#' quickcor(mtcars, type = "upper") + geom_circle2()
#' quickcor(mtcars, type = "lower", show.diag = FALSE) + geom_ellipse2()
#' quickcor(mtcars, cluster = TRUE) + geom_square()
#' quickcor(mtcars, cor.test = TRUE) + geom_confbox()
#' quickcor(mtcars, cor.test = TRUE) + geom_colour() + geom_cross()
#' quickcor(mtcars, cor.test = TRUE) + geom_star(n = 5)
#' quickcor(mtcars, cor.test = TRUE) + geom_colour() + geom_number(aes(num = r))
#' quickcor(mtcars, cor.test = TRUE) +
#'   geom_square(data = get_data(type = "lower", show.diag = FALSE)) +
#'   geom_mark(data = get_data(type = "upper", show.diag = FALSE)) +
#'   geom_abline(slope = -1, intercept = 12)
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
quickcor <- function(x,
                     y = NULL,
                     mapping = NULL,
                     fill.colours = NULL,
                     fill.bin = FALSE, # if TRUE, using scale_fill_steps2n(), else scale_fill_gradient2n()
                     grid.colour = "grey50",
                     grid.size = 0.25,
                     axis.x.position = "auto",
                     axis.y.position = "auto",
                     axis.label.drop = TRUE,
                     legend.title = "corr",
                     legend.position = "auto",
                     legend.breaks = NULL,
                     legend.labels = NULL,
                     ...)
{
  data <- fortify_cor(x, y, ...)
  type <- cor_tbl_type(data)
  show.diag <- cor_tbl_showdiag(data)
  xname <- cor_tbl_xname(data)
  yname <- cor_tbl_yname(data)
  name <- names(data)
  # handle mapping setting
  map_base <- aes_string(x = "x", y = "y", r = "r", r0 = "r", fill = "r")
  if("p.value" %in% name)
    map_base <- modifyList(map_base, aes_string(p.value = "p.value"))
  if(all (c("lower.ci", "upper.ci") %in% name))
    map_base <- modifyList(map_base, aes_string(lower.ci = "lower.ci", upper.ci = "upper.ci"))
  if(is.null(mapping)) {
    mapping <- map_base
  } else {
    mapping <- modifyList(map_base, mapping)
  }
  # handle axis setting
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
  # handle legend setting
  if(legend.position == "auto")
    legend.position <- switch (type,
                               full = "right",
                               lower = "left",
                               upper = "right")
  if(is.null(legend.breaks))
    legend.breaks <- seq(-1, 1, length.out = 5)
  if(is.null(legend.labels))
    legend.labels <- legend.breaks
  envir <- parent.frame()
  p <- ggplot(data = data, mapping = mapping, environment = envir) +
    add_grid(grid.colour, grid.size) +
    scale_x_continuous(breaks = axis.x.breaks, labels = axis.x.labels,
                       position = axis.x.position)+
    scale_y_continuous(breaks = axis.y.breaks, labels = axis.y.labels,
                       position = axis.y.position)
  # add colour scale
  ## handle colours setting
  if(fill.bin) {
    p <- p + scale_fill_steps2n(breaks = legend.breaks,
                                labels = legend.labels,
                                expand = TRUE,
                                colours = fill.colours %||% red_blue(),
                                limits = c(-1, 1)) +
      guides(fill = guide_colorsteps(even.steps = FALSE,
                                     show.limits = FALSE,
                                     title = legend.title))
  } else {
    p <- p + scale_fill_gradient2n(breaks = legend.breaks,
                                   labels = legend.labels,
                                   expand = TRUE,
                                   colours = fill.colours %||% red_blue(),
                                   limits = c(-1, 1)) +
      guides(fill = guide_colourbar(title = legend.title,
                                    nbin  = 40))
  }
  # add theme and coord
  xlim <- c(0.5, length(xname) + 0.5)
  ylim <- c(0.5, length(yname) + 0.5)
  p <- p +
    coord_fixed(expand = FALSE, xlim = xlim, ylim = ylim) +
    theme_cor(legend.position = legend.position)
  class(p) <- c("quickcor", class(p))
  p
}
