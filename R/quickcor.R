#' Plot Correlation Matrix Quickly
#' @description quickcor is convenient wrapper for creating a number of different types
#' of correlation matrix plots because of adding some extra settings by default.
#' @param x,y matrix, data frame or quickcor object in \code{print()}.
#' @param mapping NULL (default) or a list of aesthetic mappings to use for plot.
#' @param circular logical, if TRUE will draw in polar coordinates.
#' @param open angle of opening (in degree).
#' @param inner,outer the ratio of inner circle and outer margin.
#' @param bcols colours of branchs.
#' @param paxis one of "all", "x", "y" or "none".
#' @param fixed.xy if TRUE (default), the coordinates will with fixed aspect ratio.
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
#' @importFrom ggplot2 ggplot_add
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_colourbar
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 coord_polar
#' @importFrom ggplot2 theme_void
#' @rdname quick_cor
#' @examples
#' require(ggplot2, quietly = TRUE)
#'
#' # Initialize the plot
#' quickcor(mtcars)
#'
#' # layer of tile
#' quickcor(mtcars) + geom_colour()
#'
#' # layer of circle and trim the lower triangle
#' quickcor(mtcars, type = "upper") + geom_circle2()
#'
#' # layer of ellipse and not show diagonal
#' quickcor(mtcars, type = "lower", show.diag = FALSE) + geom_ellipse2()
#'
#' # layer of square and reorder correlation matrix by cluster
#' quickcor(mtcars, cluster = TRUE) + geom_square()
#'
#' # layer of confidence box
#' quickcor(mtcars, cor.test = TRUE) + geom_confbox()
#'
#' # different layer of upper/lower triangle
#' quickcor(mtcars, cor.test = TRUE) +
#'   geom_square(data = get_data(type = "lower", show.diag = FALSE)) +
#'   geom_mark(data = get_data(type = "upper", show.diag = FALSE)) +
#'   geom_abline(slope = -1, intercept = 12)
#'
#' @seealso \code{\link[ggcor]{fortify_cor}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
quickcor <- function(x,
                     y = NULL,
                     mapping = NULL,
                     grid.colour = "grey50",
                     grid.size = 0.25,
                     circular = FALSE,
                     open = 90,
                     inner = 1,
                     outer = 0.3,
                     bcols = NULL,
                     paxis = "all",
                     fixed.xy = TRUE,
                     axis.x.position = "auto",
                     axis.y.position = "auto",
                     axis.label.drop = TRUE,
                     legend.position = "auto",
                     ...)
{
  data <- fortify_cor(x, y, ...)
  is.general <- is_general_cor_tbl(data)
  type <- get_type(data)
  n <- length(get_row_name(data))
  m <- length(get_col_name(data))
  show.diag <- get_show_diag(data)
  name <- names(data)
  # handle mapping setting
  base.aes <- if(is.general) {
    aes_string(x = ".col.id", y = ".row.id")
  } else {
    aes_string(x = ".col.id", y = ".row.id", r0 = "r", r = "r", fill = "r")
  }
  if("p.value" %in% name)
    base.aes <- modifyList(base.aes, aes_string(p.value = "p.value"))
  if(all(c("lower.ci", "upper.ci") %in% name))
    base.aes <- modifyList(base.aes, aes_string(lower.ci = "lower.ci", upper.ci = "upper.ci"))
  if(is.null(mapping)) {
    mapping <- base.aes
  } else {
    mapping <- modifyList(base.aes, mapping)
  }
  if(isTRUE(circular)) {
    polar.args <- calc_polar_params(data, open = open, inner = inner, outer = outer)
    p <- ggplot(data, mapping = mapping) +
      geom_panel_grid(colour = grid.colour, size = grid.size) +
      scale_x_continuous(limits = polar.args$xlim) +
      scale_y_continuous(limits = polar.args$ylim) +
      coord_polar(theta = "y", start = polar.args$start, direction = -1) +
      theme_void()
    p$plot_env$polar.args <- polar.args
    if(paxis == "all") {
      p <- p + set_p_xaxis(bcols = bcols) + set_p_yaxis(bcols = bcols)
    }
    if (paxis == "x") {
      p <- p + set_p_xaxis(bcols = bcols)
    }
    if(paxis == "y") {
      p <- p + set_p_yaxis(bcols = bcols)
    }
  } else {
    # handle legend setting
    if(legend.position == "auto")
      legend.position <- switch (type,
                                 full = "right",
                                 lower = "left",
                                 upper = "right")
    p <- ggcor(data, mapping = mapping, axis.x.position = axis.x.position,
               axis.y.position = axis.y.position, axis.label.drop = axis.label.drop) +
      geom_panel_grid(colour = grid.colour, size = grid.size)

    # add theme and coord
    xlim <- c(0.5 - 0.002 * m, m + 0.5 + 0.002 * m)
    ylim <- c(0.5 - 0.002 * n, n + 0.5 + 0.002 * n)

    if(isTRUE(fixed.xy)) {
      p <- p + coord_fixed(xlim = xlim, ylim = ylim) +
        theme_cor(legend.position = legend.position)
    } else {
      p <- p + coord_cartesian(xlim = xlim, ylim = ylim) +
        theme_cor(legend.position = legend.position)
    }
  }
  class(p) <- c("quickcor", class(p))
  p
}

#' Print method for quickcor object.
#' @param colours colour palette for filling.
#' @param style style of plot, one of "corrplot" (default) or "ggplot".
#' @param title guide title.
#' @param breaks breaks of guide_colourbar.
#' @param labels labels of guide_colourbar.
#' @param limits limits of guide_colourbar.
#' @param nbin a numeric specifying the number of bins for drawing the guide_colourbar.
#' @param ... ignore.
#' @importFrom ggplot2 ggplot_add scale_fill_gradientn
#' @importFrom grid grid.draw
#' @importFrom rlang quo_get_expr eval_tidy
#' @rdname quick_cor
#' @examples
#' ## print quickcor object
#' p <- quickcor(mtcars) + geom_colour()
#' col <- c("blue", "white", "red")
#' print(p, colours = col)
#' print(p, colours = col, title = "Pearson's r")
#' print(p, style = "ggplot")
#' @method print quickcor
#' @export
print.quickcor <- function(x,
                           colours = getOption("ggcor.fill.pal"),
                           style = getOption("ggcor.plot.style", "corrplot"),
                           title = "corr",
                           breaks = c(-1, -0.5, 0, 0.5, 1),
                           labels = c(-1, -0.5, 0, 0.5, 1),
                           limits = c(-1, 1),
                           nbin = 40,
                           ...)
{
  style <- switch (style,
                   corrplot = "corrplot",
                   "ggplot2"
  )
  if(style == "corrplot") {
    mapping <- unclass(x$mapping)
    if(!is.null(mapping$fill) && is.null(x$scales$get_scales("fill"))) {
      fill.var.name <- as.character(quo_get_expr(mapping$fill))
      fill.var <- eval_tidy(mapping$fill, x$data)
      if(!is_general_cor_tbl(x$data) && fill.var.name == "r" &&
         is.numeric(fill.var)) {
        x <- x + scale_fill_gradient2n(colours = colours,
                                       breaks = breaks,
                                       labels = labels,
                                       limits = limits) +
          guides(fill = guide_colourbar(title = title,
                                        nbin  = nbin))
      }
    }
  }
  class(x) <- setdiff(class(x), "quickcor")
  print(x, ...)
}

#' @importFrom graphics plot
#' @method print quickcor
#' @export
plot.quickcor <- print.quickcor
