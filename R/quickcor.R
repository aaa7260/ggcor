#' Plot Correlation Matrix Quickly
#' @description quickcor is convenient wrapper for creating a number of different types
#' of correlation matrix plots because of adding some extra settings by default.
#' @param x,y matrix or data frame.
#' @param mapping NULL (default) or a list of aesthetic mappings to use for plot.
#' @param fill.colours NULL (default) or a vector of colours to use for n-colour gradient.
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
#' @param legend.breaks breaks of colourbar.
#' @param legend.labels labels of colourbar.
#' @param ... extra params for \code{\link[ggcor]{fortify_cor}}.
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
#' @seealso \code{\link[ggcor]{fortify_cor}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
quickcor <- function(x,
                     y = NULL,
                     mapping = NULL,
                     fill.colours = NULL,
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
  type <- get_type(data)
  show.diag <- get_show_diag(data)
  name <- names(data)
  # handle mapping setting
  base.aes <- aes_string(".col.id", ".row.id", r0 = "r", r = "r", fill = "r")
  if("p.value" %in% name)
    base.aes <- modifyList(base.aes, aes_string(p.value = "p.value"))
  if(all (c("lower.ci", "upper.ci") %in% name))
    base.aes <- modifyList(base.aes, aes_string(lower.ci = "lower.ci", upper.ci = "upper.ci"))
  if(is.null(mapping)) {
    mapping <- base.aes
  } else {
    mapping <- modifyList(base.aes, mapping)
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

  p <- ggcor(data, mapping = mapping, axis.x.position = axis.x.position,
             axis.y.position = axis.y.position, axis.label.drop = axis.label.drop) +
    add_grid(grid.colour, grid.size)
  # add colour scale
  ## handle colours setting
  p <- p + scale_fill_gradient2n(breaks = legend.breaks,
                                 labels = legend.labels,
                                 expand = TRUE,
                                 colours = fill.colours %||% red_blue(),
                                 limits = c(-1, 1)) +
    guides(fill = guide_colourbar(title = legend.title,
                                  nbin  = 40))
  # add theme and coord
  p <- p + coord_fixed() + theme_cor(legend.position = legend.position)
  class(p) <- c("quickcor", class(p))
  p
}
