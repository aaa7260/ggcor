#' @export
ggcor_ <- function(x, ...) {
  UseMethod("ggcor_")
}

#' @export
ggcor_.cor_tbl <- function(
  x,
  mapping = NULL,
  is.minimal = FALSE,
  fill.colours = NULL,
  fill.bin = FALSE, # if TRUE, using scale_fill_steps2n(), else scale_fill_gradient2n()
  panel.backgroud = NA,
  grid.colour = "grey50",
  grid.size = 0.25,
  grid.linetype = "solid",
  axis.x.position = c("auto", "bottom", "top"),
  axis.y.position = c("auto", "left", "right"),
  axis.label.drop = TRUE,
  legend.title = "corr",
  legend.position = "auto",
  legend.breaks = NULL,
  legend.labels = NULL,
  coord.fixed = TRUE,
  xlim = NULL,
  ylim = NULL,
  ...
)
{
  axis.x.position <- match.arg(axis.x.position)
  axis.y.position <- match.arg(axis.y.position)
  name <- names(x)
  type <- cor_tbl_type(x)
  show.diag <- cor_tbl_showdiag(x)
  xname <- cor_tbl_xname(x)
  yname <- cor_tbl_yname(x)

  # handle mapping setting
  map_base <- aes_string(x = "x", y = "y", r = "r", fill = "r")
  if("p" %in% name)
    map_base <- modifyList(map_base, aes_string(p = "p"))
  if(all (c("low", "upp") %in% name))
    map_base <- modifyList(map_base, aes_string(low = "low", upp = "upp"))
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

  p <- ggplot(data = x, mapping = mapping) +
    geom_tile(aes_string("x", "y"), data = get_grid_data(x),
              fill = panel.backgroud, colour = grid.colour,
              size = grid.size, linetype = grid.linetype, inherit.aes = FALSE) +
    scale_x_continuous(breaks = axis.x.breaks, labels = axis.x.labels,
                       position = axis.x.position)+
    scale_y_continuous(breaks = axis.y.breaks, labels = axis.y.labels,
                       position = axis.y.position)
  # add colour scale
  ## handle colours setting
  if(!is.minimal) {
    if(is.null(fill.colours))
      fill.colours <- .default_colors
    if(fill.bin) {
      p <- p + scale_fill_steps2n(breaks = legend.breaks,
                                  labels = legend.labels,
                                  expand = TRUE,
                                  colours = fill.colours,
                                  limits = c(-1, 1)) +
        guides(fill = guide_colorsteps(even.steps = FALSE,
                                       show.limits = FALSE,
                                       title = legend.title))
    } else {
      p <- p + scale_fill_gradient2n(breaks = legend.breaks,
                                     labels = legend.labels,
                                     expand = TRUE,
                                     colours = fill.colours,
                                     limits = c(-1, 1)) +
        guides(fill = guide_colourbar(title = legend.title,
                                      nbin  = 40))
    }
  }
  # add theme and coord
  if(is.null(xlim)) {
    xlim <- c(0.5, length(xname) + 0.5)
  }
  if(is.null(ylim)) {
    ylim <- c(0.5, length(yname) + 0.5)
  }
  if(coord.fixed) {
    p <- p + coord_fixed(expand = FALSE, xlim = xlim, ylim = ylim)
  } else {
    p <- p + coord_cartesian(expand = FALSE, xlim = xlim, ylim = ylim)
  }
  p <- p + theme_cor(legend.position = legend.position)
  class(p) <- c("ggcor_", class(p))
  p
}

#' @export
ggcor_.cor_tbl_fct <- function(
  x,
  mapping = NULL,
  is.minimal = FALSE,
  fill.colours = NULL,
  fill.bin = FALSE,
  axis.x.position = c("auto", "bottom", "top"),
  axis.y.position = c("auto", "left", "right"),
  legend.title = "corr",
  legend.position = "auto",
  legend.breaks = NULL,
  legend.labels = NULL,
  coord.fixed = TRUE,
  xlim = NULL,
  ylim = NULL,
  ...
)
{
  axis.x.position <- match.arg(axis.x.position)
  axis.y.position <- match.arg(axis.y.position)
  name <- names(x)
  type <- cor_tbl_type(x)
  show.diag <- cor_tbl_showdiag(x)
  xname <- cor_tbl_xname(x)
  yname <- cor_tbl_yname(x)
  # handle mapping setting
  map_base <- aes_string(x = "x", y = "y", r = "r", fill = "r")
  if("p" %in% name)
    map_base <- modifyList(map_base, aes_string(p = "p"))
  if(all (c("low", "upp") %in% name))
    map_base <- modifyList(map_base, aes_string(low = "low", upp = "upp"))
  if(is.null(mapping)) {
    mapping <- map_base
  } else {
    mapping <- modifyList(map_base, mapping)
  }
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

  p <- ggplot(data = x, mapping = mapping) +
    scale_x_discrete(position = axis.x.position) +
    scale_y_discrete(position = axis.y.position)
  if(type != "full") {
    p <- p + geom_tile(aes_string("x", "y"), data = get_grid_data(x),
                fill = "white", colour = NA, inherit.aes = FALSE)
  }
  # add colour scale
  ## handle colours setting
  if(!is.minimal) {
    if(is.null(fill.colours))
      fill.colours <- .default_colors
    if(fill.bin) {
      p <- p + scale_fill_steps2n(breaks = legend.breaks,
                                  labels = legend.labels,
                                  expand = TRUE,
                                  colours = fill.colours,
                                  limits = c(-1, 1)) +
        guides(fill = guide_colorsteps(even.steps = FALSE,
                                       show.limits = FALSE,
                                       title = legend.title))
    } else {
      p <- p + scale_fill_gradient2n(breaks = legend.breaks,
                                     labels = legend.labels,
                                     expand = TRUE,
                                     colours = fill.colours,
                                     limits = c(-1, 1)) +
        guides(fill = guide_colourbar(title = legend.title,
                                      nbin  = 40))
    }
  }
  # add theme and coord
  if(is.null(xlim)) {
    xlim <- c(0.5, length(xname) + 0.5)
    if(type %in% c("lower", "upper"))
      xlim <- xlim - c(0, 1)
  }
  if(is.null(ylim)) {
    ylim <- c(0.5, length(yname) + 0.5)
    if(type %in% c("lower", "upper"))
      ylim <- ylim - c(0, 1)
  }
  if(coord.fixed) {
    p <- p + coord_fixed(expand = FALSE, xlim = xlim, ylim = ylim)
  } else {
    p <- p + coord_cartesian(expand = FALSE, xlim = xlim, ylim = ylim)
  }
  p <- p + theme_cor2(legend.position = legend.position)
  class(p) <- c("ggcor_", class(p))
  p
}

#' @export
ggcor_.mantel_tbl <- function(x, byrow = TRUE, keep.name = FALSE,
                              legend.title = "Mantel's r", ...) {
  if(keep.name) {
    x <- as_cor_tbl_fct(x, byrow = byrow)
  } else {
    x <- as_cor_tbl(x, byrow = byrow)
  }
  ggcor_(x, legend.title = legend.title, ...)
}

#' @export
ggcor_.rcorr <- function(x, keep.name = FALSE, legend.title = "corr", ...) {
  if(keep.name) {
    x <- as_cor_tbl_fct(x)
  } else {
    x <- as_cor_tbl(x)
  }
  ggcor_(x, legend.title = legend.title, ...)
}

#' @export
ggcor_.corr.test <- function(x, keep.name = FALSE, legend.title = "corr", ...) {
  if(keep.name) {
    x <- as_cor_tbl_fct(x)
  } else {
    x <- as_cor_tbl(x)
  }
  ggcor_(x, legend.title = legend.title, ...)
}
#' @export
ggcor_.default <- function(x, ...) {
  stop(class(x), " hasn't been realized yet.", call. = FALSE)
}

