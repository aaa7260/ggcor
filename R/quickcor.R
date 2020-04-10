#' Plot Correlation Matrix Quickly
#' @description quickcor is convenient wrapper for creating a number of different types
#' of correlation matrix plots because of adding some extra settings by default.
#' @param x,y matrix, data frame or quickcor object in \code{print()}.
#' @param mapping NULL (default) or a list of aesthetic mappings to use for plot.
#' @param fixed.xy if TRUE (default), the coordinates will with fixed aspect ratio.
#' @param grid.colour colour of grid lines.
#' @param grid.size size of grid lines.
#' @param drop logical value, if TRUE (default) will drop the unused factor levels.
#' @param legend.position position of legend.
#' @param ... extra params for \code{\link[ggcor]{fortify_cor}}.
#' @importFrom ggplot2 ggplot_add guides guide_colourbar coord_fixed
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
                     fixed.xy = TRUE,
                     drop = TRUE,
                     legend.position = "auto",
                     ...)
{
  data <- fortify_cor(x, y, ...)
  is.general <- is_general_cor_tbl(data)
  type <- get_type(data)
  name <- names(data)
  # handle mapping setting
  base.aes <- if(!is.general) {
    aes_string(r0 = "r", r = "r", fill = "r", p.value = "p.value",
               lower.ci = "lower.ci", upper.ci = "upper.ci")
  }
  base.aes <- aes_intersect(base.aes, ggplot2::aes_all(name), c("r", "r0", "fill"))
  mapping <- if(!is.null(mapping)) {
    aes_modify(base.aes, mapping)
  } else base.aes

  # handle legend setting
  if(legend.position == "auto")
    legend.position <- switch (type,
                               full = "right",
                               lower = "left",
                               upper = "right")
  p <- ggcor(data, mapping = mapping, drop = drop) +
    geom_panel_grid(colour = grid.colour, size = grid.size)

  if(isTRUE(fixed.xy)) {
    p <- p + coord_fixed() +
      theme_cor(legend.position = legend.position)
  } else {
    p <- p + theme_cor(legend.position = legend.position)
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
