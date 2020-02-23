#' Plot Correlation Matrix Quickly
#' @description quickcor is convenient wrapper for creating a number of different types
#' of correlation matrix plots because of adding some extra settings by default.
#' @param x,y matrix or data frame.
#' @param mapping NULL (default) or a list of aesthetic mappings to use for plot.
#' @param grid.colour colour of grid lines.
#' @param grid.size size of grid lines.
#' @param axis.x.position,axis.y.position the position of the axis. 'auto' (default)
#'     is set according to the plot type, 'bottom' or 'top' for x axes, 'left' or 'right'
#'     for y axes.
#' @param axis.label.drop logical value (default is TRUE). When type of plot is 'upper'
#'     or 'lower' and 'show.diag' is FALSE, do you need to remove the blank coordinate
#'     label.
#' @param legend.position position of legend.
#' @param ... extra params for \code{\link[ggcor]{fortify_cor}}.
#' @importFrom ggplot2 ggplot_add guides guide_colourbar coord_fixed
#' @rdname quick_cor
#' @examples
#' require(ggplot2, quietly = TRUE)
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
                     grid.colour = "grey50",
                     grid.size = 0.25,
                     axis.x.position = "auto",
                     axis.y.position = "auto",
                     axis.label.drop = TRUE,
                     legend.position = "auto",
                     ...)
{
  data <- fortify_cor(x, y, ...)
  type <- get_type(data)
  n <- length(get_row_name(data))
  m <- length(get_col_name(data))
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
  p <- ggcor(data, mapping = mapping, axis.x.position = axis.x.position,
             axis.y.position = axis.y.position, axis.label.drop = axis.label.drop) +
    add_grid(grid.colour, grid.size)

  # add theme and coord
  xlim <- c(0.5 - 0.002 * m, m + 0.5 + 0.002 * m)
  ylim <- c(0.5 - 0.002 * n, n + 0.5 + 0.002 * n)
  p <- p + coord_fixed(xlim = xlim, ylim = ylim) +
    theme_cor(legend.position = legend.position)
  class(p) <- c("quickcor", class(p))
  p
}

#' @importFrom ggplot2 ggplot_add
#' @importFrom grid grid.draw
#' @method print quickcor
#' @export
print.quickcor <- function(x, title = "corr", nbin = 40, ...)
{
  fill.scale <- x$scales$get_scales("fill")
  if(is.null(fill.scale)) {
    x <- x + scale_fill_gradient2n(colours = getOption("ggcor.fill.continuous"),
                                   breaks = c(-1, -0.5, 0, 0.5, 1),
                                   labels = c(-1, -0.5, 0, 0.5, 1),
                                   limits = c(-1, 1)) +
      guides(fill = guide_colourbar(title = title,
                                    nbin  = nbin))
  }
  class(x) <- setdiff(class(x), "quickcor")
  grid::grid.draw(x, ...)
}

#' @importFrom graphics plot
#' @method print quickcor
#' @export
plot.quickcor <- print.quickcor
