#' @importFrom ggplot2 aes_ geom_curve geom_text geom_point
#' @importFrom utils modifyList
#' @export
add_link <- function(df,
                     mapping = NULL,
                     spec.key = "spec",
                     env.key = "env",
                     curvature = NULL,
                     spec.label.hspace = NULL,
                     spec.label.vspace = 0,
                     on.left = FALSE,
                     diag.label = FALSE,
                     extra.params = extra_params(),
                     ...)
{
  link_fun <- function(corr) {
    if(!is_cor_tbl(corr)) {
      warning("'corr' need a cor_tbl.", call. = FALSE)
      return(geom_blank())
    }
    type <- get_type(corr)
    show.diag <- get_show_diag(corr)
    n <- length(get_col_name(corr))
    m <- length(get_row_name(corr))
    if(type != "full") on.left <- FALSE
    link.data <- tidy_link_data(df = df,
                                cor_tbl = corr,
                                spec.key = spec.key,
                                env.key = env.key,
                                env.point.hjust = extra.params$link.params$env.point.hjust,
                                env.point.vjust = extra.params$link.params$env.point.vjust,
                                spec.point.hjust = extra.params$link.params$spec.point.hjust,
                                spec.point.vjust = extra.params$link.params$spec.point.vjust,
                                on.left = on.left,
                                diag.label = diag.label)
    spec.point.data <- link.data[!duplicated(link.data[[spec.key]]), , drop = FALSE]
    env.point.data <- link.data[!duplicated(link.data[[env.key]]), , drop = FALSE]
    base.aes <- aes_(x = ~link.x, y = ~link.y, xend = ~link.xend, yend = ~link.yend)
    mapping <- if(!is.null(mapping)) modifyList(base.aes, mapping) else base.aes
    curvature <- curvature %||% switch (type,
                                        full = 0,
                                        upper = -0.1,
                                        lower = 0.1)
    spec.label.hspace <- spec.label.hspace %||% switch (type,
                                                        full = if(on.left) - 0.2 else 0.2,
                                                        upper = -0.2,
                                                        lower = 0.2)
    if(is.null(extra.params$spec.label$hjust)) {
      extra.params$spec.label$hjust <- switch(type,
                                              full = if(on.left) 1 else 0,
                                              upper = 1,
                                              lower = 0)
    }
    min.x <- min(link.data$link.x, na.rm = TRUE)
    max.x <- max(link.data$link.x, na.rm = TRUE)
    xlim <- if(!is.null(extra.params$spec.label)) {
      switch (type,
              full = if(on.left) min.x - 2 else max.x + 2,
              upper = min.x - 2,
              lower = max.x + 2
      )
    } else {
      switch (type,
              full = if(on.left) min.x - 0.2 else max.x + 0.2,
              upper = min.x - 0.2,
              lower = max.x + 0.2
      )
    }

    gg <- list(link.line = geom_curve(mapping, link.data, curvature = curvature,
                                      inherit.aes = FALSE, ...),
               if(!is.null(extra.params$spec.point)) {
                 spec.point <- geom_point(
                   aes_(x = ~link.x, y = ~link.y), spec.point.data,
                   alpha = extra.params$spec.point$alpha,
                   colour = extra.params$spec.point$colour,
                   fill = extra.params$spec.point$fill,
                   shape = extra.params$spec.point$shape,
                   size = extra.params$spec.point$size,
                   stroke = extra.params$spec.point$stroke,
                   inherit.aes = FALSE)
               },
               if(!is.null(extra.params$env.point)) {
                 env.point = geom_point(
                   aes_(x = ~link.xend, y = ~link.yend), env.point.data,
                   alpha = extra.params$env.point$alpha,
                   colour = extra.params$env.point$colour,
                   fill = extra.params$env.point$fill,
                   shape = extra.params$env.point$shape,
                   size = extra.params$env.point$size,
                   stroke = extra.params$env.point$stroke,
                   inherit.aes = FALSE)
               },
               if(!is.null(extra.params$spec.label)) {
                 spec.label.data <- tibble::tibble(
                   x = spec.point.data$link.x + spec.label.hspace,
                   y = spec.point.data$link.y + spec.label.vspace,
                   label = spec.point.data[[spec.key]])
                 spec.label <- geom_text(
                   aes_(x = ~x, y = ~y, label = ~label), spec.label.data,
                   family = extra.params$spec.label$family,
                   fontface = extra.params$spec.label$fontface,
                   colour = extra.params$spec.label$colour,
                   size = extra.params$spec.label$size,
                   hjust = extra.params$spec.label$hjust,
                   vjust = extra.params$spec.label$vjust,
                   angle = extra.params$spec.label$angle,
                   inherit.aes = FALSE)
               },
               expand_axis(x = xlim))
    gg
  }
  class(link_fun) <- c("add_link", class(link_fun))
  link_fun
}



#' @export
ggplot_add.add_link <- function(object, plot, object_name) {
  data <- plot$data
  new_layer <- object(corr = data)
  plot + new_layer
}

