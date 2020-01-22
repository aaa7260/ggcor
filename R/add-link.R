#' Add other association link plot on correlation plot.
#' @description This function can add other associated information link plot more
#'     quickly, and this function can be used to reflect the relationship between
#'     other variables and variables in the correlation coefficient matrix plot.
#' @param df a data frame object.
#' @param mapping NULL (default) or a list of aesthetic mappings to use for plot.
#' @param spec.key string (defaults to "spec"), group variables names in \code{df}.
#' @param env.key string (defaults to "env"),  variables names in \code{df} that
#'     associated with the correlation coefficient matrix.
#' @param curvature a numeric value giving the amount of curvature.
#' @param spec.label.hspace,spec.label.vspace a numeric value giving the amount of
#'     horizontal/vertical space betweed group points and labels.
#' @param on.left add link plot on left or right when the type of correlation plot
#'     is "full".
#' @param diag.label logical (defaults to FALSE) to indicate whether add diag labels.
#' @param extra.params other parameters that control link details can only be set using
#'     the \code{\link[ggcor]{extra_params}} function.
#' @param ... extra params passing to \code{\link[ggplot2]{geom_curve}}.
#' @importFrom ggplot2 aes_ geom_curve geom_text geom_point
#' @importFrom utils modifyList
#' @rdname add_link
#' @examples
#' require(vegan, quietly = TRUE)
#' require(dplyr, quietly = TRUE)
#' require(ggplot2, quietly = TRUE)
#' data("varechem")
#' data("varespec")
#' mantel <- fortify_mantel(varespec, varechem,
#'                            spec.select = list(1:10, 5:14, 7:22, 9:32))
#' quickcor(varechem, type = "upper") +
#'   geom_square() +
#'   add_link(mantel, diag.label = TRUE) +
#'   add_diag_label() + remove_axis("x")
#'
#' mantel01 <- mantel %>%
#'   mutate(r = cut(r, breaks = c(-Inf, 0.25, 0.5, Inf),
#'                  labels = c("<0.25", "0.25-0.5", ">=0.5"),
#'                  right = FALSE),
#'          p.value = cut(p.value, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
#'                        labels = c("<0.001", "0.001-0.01", "0.01-0.05", ">=0.05"),
#'                        right = FALSE))
#' quickcor(varechem, type = "upper") + geom_square() +
#'   add_link(mantel01, mapping = aes(colour = p.value, size = r),
#'            diag.label = TRUE) +
#'   add_diag_label() +
#'   scale_size_manual(values = c(0.5, 1.5, 3)) +
#'   remove_axis("x")
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
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
      return(NULL)
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

    gg <- list(geom_curve(mapping, link.data, curvature = curvature,
                                      inherit.aes = FALSE, ...),
               if(!is.null(extra.params$spec.point)) {
                 geom_point(
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
                 geom_point(
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
                 geom_text(
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

