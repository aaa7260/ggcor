#' @export
add_link <- function(x, ...) {
  UseMethod("add_link")
}

#' @export
add_link.data.frame <- function(x,
                             mapping = NULL,
                             on.left = FALSE,
                             diag.label = FALSE,
                             curvature = NULL,
                             group.label.hspace = NULL,
                             group.label.vspace = 0,
                             extra.params = extra_params(),
                             ...)
{
  link_fun <- function(corr) {
    if(!inherits(corr, "cor_tbl"))
      stop("'corr' need a cor_tbl.", call. = FALSE)
    type <- cor_tbl_type(corr)
    show.diag <- cor_tbl_showdiag(corr)
    n <- length(cor_tbl_xname(corr))
    m <- length(cor_tbl_yname(corr))
    if(type != "full")
      on.left <- FALSE
    data <- handle_link_data(x = x, corr, on.left = on.left, diag.label = diag.label,
                             corrmat.point.hjust = extra.params$link.params$corrmat.point.hjust,
                             corrmat.point.vjust = extra.params$link.params$corrmat.point.vjust,
                             group.point.hjust = extra.params$link.params$group.point.hjust,
                             group.point.vjust = extra.params$link.params$group.point.vjust)
    link.data <- data$link_data
    group.data <- data$group_data
    corrmat.data <- data$corrmat_data
    if(is.null(mapping)) {
      mapping <- aes_string(x = "group_x", y = "group_y", xend = "corr_x", yend = "corr_y")
    } else {
      mapping <- modifyList(aes_string(x = "group_x", y = "group_y", xend = "corr_x", yend = "corr_y"),
                            mapping)
    }

    if(is.null(curvature)) {
      curvature <- switch (type,
                           full = 0,
                           upper = -0.1,
                           lower = 0.1)
    }
    if(is.null(group.label.hspace)) {
      group.label.hspace <- switch (type,
                                    full = if(on.left) - 0.2 else 0.2,
                                    upper = -0.2,
                                    lower = 0.2)
    }
    if(is.null(extra.params$group.label$hjust)) {
      extra.params$group.label$hjust <- switch(
        type,
        full = if(on.left) 1 else 0,
        upper = 1,
        lower = 0)
    }

    gg <- list(
      link.line = geom_curve(mapping, link.data, curvature = curvature,
                             inherit.aes = FALSE, ...))
    if(!is.null(extra.params$group.point))
      gg <- modifyList(
        gg, list(group.point = geom_point(
          aes_(x = ~group_x, y = ~group_y), group.data,
          alpha = extra.params$group.point$alpha,
          colour = extra.params$group.point$colour,
          fill = extra.params$group.point$fill,
          shape = extra.params$group.point$shape,
          size = extra.params$group.point$size,
          stroke = extra.params$group.point$stroke,
          inherit.aes = FALSE)))

    if(!is.null(extra.params$corrmat.point))
      gg <- modifyList(
        gg, list(matcorr.point = geom_point(
          aes_(x = ~corr_x, y = ~corr_y), data = corrmat.data,
          alpha = extra.params$corrmat.point$alpha,
          colour = extra.params$corrmat.point$colour,
          fill = extra.params$corrmat.point$fill,
          shape = extra.params$corrmat.point$shape,
          size = extra.params$corrmat.point$size,
          stroke = extra.params$corrmat.point$stroke,
          inherit.aes = FALSE)))
    if(!is.null(extra.params$group.label))
      gg <- modifyList(
        gg, list(group.label = geom_text(
          aes_(x = ~group_x + group.label.hspace,
               y = ~group_y + group.label.vspace, label = ~group_name), group.data,
          family = extra.params$group.label$family,
          fontface = extra.params$group.label$fontface,
          colour = extra.params$group.label$colour,
          size = extra.params$group.label$size,
          hjust = extra.params$group.label$hjust,
          vjust = extra.params$group.label$vjust,
          angle = extra.params$group.label$angle,
          inherit.aes = FALSE)))
    gg
  }
  class(link_fun) <- c("link_fun", class(link_fun))
  link_fun
}

#' @export
add_link.mantel_tbl <- function(x,
                                mapping = NULL,
                                on.left = FALSE,
                                diag.label = FALSE,
                                r.breaks = c(0.25, 0.5),
                                r.labels = c("<= 0.25", "0.25 - 0.5", "> 0.5"),
                                p.breaks = c(0.001, 0.01, 0.05),
                                p.labels = c("<= 0.001", "0.001 - 0.01", "0.01 - 0.05", "> 0.05"),
                                link.line.colours = NULL,
                                link.line.size = c(0.5, 1.5, 4),
                                legend.title.scale.size = "Mantel's r",
                                legend.title.scale.colour = "p value",
                                legend.drop = FALSE,
                                ...)
{
  mantel <- rename_mantel(x)
  if(is.null(mapping)) {
    mapping <- aes_string(size = "rr", colour = "pp")
  } else {
    mapping <- modifyList(aes_string(size = "rr", colour = "pp"), mapping)
  }
  r.breaks <- unique(sort(c(-1, r.breaks, 1)))
  p.breaks <- unique(sort(c(0, p.breaks, 1)))
  mantel$rr <- cut(mantel$r, breaks = r.breaks, labels = r.labels,
                     include.lowest = FALSE)
  mantel$pp <- cut(mantel$p, breaks = p.breaks, labels = p.labels,
                     include.lowest = FALSE)
  if(is.null(link.line.colours))
    link.line.colours <- link_colour_pal(length(p.breaks) - 1)
  scale <- list(link.line.colour = scale_colour_manual(drop = legend.drop, values = link.line.colours),
                link.line.size   = scale_size_manual(drop = legend.drop, values = link.line.size),
                link.line.guide  = guides(
                  colour = guide_legend(title = legend.title.scale.colour,
                                        override.aes = list(size = 2), order = 2),
                  size = guide_legend(title = legend.title.scale.size,
                                      override.aes = list(colour = "grey35"), order = 1)))
  link <- add_link.data.frame(x = mantel,
                   mapping = mapping,
                   on.left = on.left,
                   diag.label = diag.label,
                   ...)
  list(link, scale)
}


#' @export
add_link.cor_tbl <- function(x,
                             mapping = NULL,
                             on.left = FALSE,
                             diag.label = FALSE,
                             r.breaks = 0,
                             r.labels = c("<= 0", "> 0"),
                             p.breaks = 0.05,
                             p.labels = c("<= 0.05", "> 0.05"),
                             link.line.colours = c("#E31A1C", "#33A02C"),
                             link.line.linetype = c("solid", "dashed"),
                             legend.title.scale.linetype = "p value",
                             legend.title.scale.colour = "r",
                             legend.drop = FALSE,
                             ...)
{
  cor_tbl <- cor_tbl_namebind(x)
  if(is.null(mapping)) {
    mapping <- aes_string(colour = "rr", linetype = "pp")
  } else {
    mapping <- modifyList(aes_string(colour = "rr", linetype = "pp"), mapping)
  }
  r.breaks <- unique(sort(c(-1, r.breaks, 1)))
  p.breaks <- unique(sort(c(0, p.breaks, 1)))
  cor_tbl$rr <- cut(cor_tbl$r, breaks = r.breaks, labels = r.labels,
                  include.lowest = TRUE)
  cor_tbl$pp <- cut(cor_tbl$p, breaks = p.breaks, labels = p.labels,
                  include.lowest = TRUE)
  scale <- list(link.line.colour = scale_colour_manual(drop = legend.drop, values = link.line.colours),
                link.line.linetype   = scale_linetype_manual(drop = legend.drop, values = link.line.linetype),
                link.line.guide  = guides(
                  colour = guide_legend(title = legend.title.scale.colour,
                                        override.aes = list(size = 2), order = 2),
                  linetype = guide_legend(title = legend.title.scale.linetype,
                                      override.aes = list(colour = "grey35"), order = 1)))
  link <- add_link.data.frame(x = cor_tbl,
                              mapping = mapping,
                              on.left = on.left,
                              diag.label = diag.label,
                              ...)
  list(link, scale)
}

#' @export
add_link.default <- function(x, ...) {
  stop(class(x), " hasn't been realized yet.", call. = FALSE)
}

#' @export
ggplot_add.link_fun <- function(object, plot, object_name) {
  data <- plot$data
  new_layer <- object(corr = data)
  plot + new_layer
}

