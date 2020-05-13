#' @importFrom ggplot2 ggplot_add geom_segment
#' @export
ggplot_add.geom_panel_grid <- function(object, plot, object_name) {
  obj <- geom_segment(aes_string(x = "x", y = "y", xend = "xend", yend = "yend"),
                      data = get_grid_data(plot$data, drop = drop),
                      colour = object$colour, size = object$size,
                      inherit.aes = FALSE)
  ggplot_add(object = obj, plot = plot)
}

#' @noRd
get_grid_data <- function(data, drop) {
  if(!is_cor_tbl(data))
    stop("Need a cor_tbl.", call. = FALSE)
  n <- length(get_col_name(data))
  m <- length(get_row_name(data))
  type <- get_type(data)
  show.diag <- get_show_diag(data)

  if(type == "full") {
    xx <- c(0:n + 0.5, rep_len(0.5, m + 1))
    yy <- c(rep_len(0.5, n + 1), 0:m + 0.5)
    xxend <- c(0:n + 0.5, rep_len(n + 0.5, m + 1))
    yyend <- c(rep_len(m + 0.5, n + 1), 0:m + 0.5)
  } else if(type == "upper") {
    if(show.diag) {
      xx <- c(0:n + 0.5, c(n:1 - 0.5, 0.5))
      yy <- c(c(m:1 - 0.5, 0.5), 0:m + 0.5)
      xxend <- c(0:n + 0.5, rep_len(n + 0.5, m + 1))
      yyend <- c(rep_len(m + 0.5, n + 1), 0:m + 0.5)
    } else {
      xx <- c(1:n + 0.5, c(n:2 - 0.5, 1.5))
      yy <- c(c(m:2 - 0.5, 1.5), 1:m + 0.5)
      xxend <- c(1:n + 0.5, rep_len(n + 0.5, m))
      yyend <- c(rep_len(m + 0.5, n), 1:m + 0.5)
    }
  } else {
    if(show.diag) {
      xx <- c(0:n + 0.5, rep_len(0.5, m + 1))
      yy <- c(rep_len(0.5, n + 1), 0:m + 0.5)
      xxend <- c(0:n + 0.5, c(n + 0.5, n:1 + 0.5))
      yyend <- c(c(m + 0.5, m:1 + 0.5), 0:m + 0.5)
    } else {
      xx <- c(1:n - 0.5, rep_len(0.5, m))
      yy <- c(rep_len(0.5, n), 1:m - 0.5)
      xxend <- c(1:n - 0.5, c(n - 0.5, n:2 - 0.5))
      yyend <- c(c(m - 0.5, m:2 - 0.5), 1:m - 0.5)
    }
  }
  new_data_frame(list(x = xx, y = yy, xend = xxend, yend = yyend))
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_diag_label <- function(object, plot, object_name) {
  geom <- match.arg(object$geom, c("text", "label", "image"))
  geom_fun <- switch (object$geom,
                      text = get_function("ggplot2", "geom_text"),
                      label = get_function("ggplot2", "geom_label"),
                      image = get_function("ggimage", "geom_image")
  )

  type <- get_type(plot$data)
  row.names <- get_row_name(plot$data)
  show.diag <- get_show_diag(plot$data)

  n <- length(row.names)
  d <- new_data_frame(list(x = 1:n, y = n:1, label = row.names))
  if(object$geom == "image") {
    if(!"image" %in% names(object$params)) {
      stop("Did you forget to set the 'image' parameter?", call. = FALSE)
    }
    if(!is.null(names(object$params$image))) {
      object$params$image <- object$params$image[row.names]
    }
    mapping <- aes_string(x = "x", y = "y")
  } else {
    mapping <- aes_string(x = "x", y = "y", label = "label")
  }
  oparams <- list(mapping = aes_string(x = "x", y = "y", label = "label"),
                  data = d, inherit.aes = FALSE)
  params <- utils::modifyList(oparams, object$params)
  obj <- do.call(geom_fun, params)
  if(isTRUE(object$remove.axis)) {
    plot <- plot + remove_axis()
  }
  ggplot_add(object = obj, plot = plot)
}

#' @importFrom ggplot2 ggplot_add aes_string expansion
#' @export
ggplot_add.anno_link <- function(object, plot, object_name) {
  if(isTRUE(plot$plot_env$circlar)) {
    stop("`anno_link()` should be used in cartesian coordinates.", call. = FALSE)
  }
  pdata <- plot$data
  type <- get_type(pdata)
  show.diag <- get_show_diag(pdata)
  n <- ncols(pdata)
  m <- nrows(pdata)

  data <- link_tbl(object$data, pdata, object$start.var, object$start.name, object$end.var)
  node.data <- attr(data, "node.pos")
  mapping <- aes_modify(aes_string(x = "x", y = "y", xend = "xend", yend = "yend"),
                        object$mapping)
  params <- modifyList(list(data = data, mapping = mapping, inherit.aes = FALSE),
                       object$params)

  xmin <- min(data$x, na.rm = TRUE)
  xmax <- max(data$x, na.rm = TRUE)
  if(type == "upper") {
    if(isTRUE(show.diag)) {
      xrange <- c(min(-0.5, xmin - 0.2 * n), n + 0.5)
      yrange <- c(0.5, n + 0.5)
    } else {
      xrange <- c(min(-0.5, xmin - 0.2 * n), n - 0.5)
      yrange <- c(-0.5, n - 0.5)
    }

  } else if (type == "lower") {
    if(isTRUE(show.diag)) {
      xrange <- c(0.5, max(xmax + 0.2 * n, n + 1.5))
      yrange <- c(0.5, n + 0.5)
    } else {
      xrange <- c(0.5, max(xmax + 0.2 * n, n + 0.5))
      yrange <- c(0.5, n + 0.5)
    }
  }
  nudge_x <- object$nudge_x
  pos <- object$pos
  if(type == "upper") {
    nudge_x <- - nudge_x
  }
  if(type == "full") {
    if(object$pos == "left")
      nudge_x <- - nudge_x
  }
  hjust <- switch (type,
    lower = 0,
    upper = 1,
    full = if(pos == "right") 0 else 1
  )
  obj <- list(
    do.call(geom_links2, params),
    do.call(geom_text,
            list(mapping = aes_string(x = "x", y = "y", label = "label"),
                 data = node.data, hjust = hjust, size = object$label.size,
                 colour = object$label.colour, family = object$label.family,
                 fontface = object$label.fontface, nudge_x = nudge_x,
                 inherit.aes = FALSE))
    )
  if(type == "full") {
    width <- object$width
    if(is.null(object$expand)) {
      expand <- if(pos == "left") expansion(c(0.5, 0.05)) else expansion(c(0.05, 0.5))
    }
    p <- ggplot() + obj +
      scale_x_continuous(expand = expand) +
      scale_y_continuous(limits = c(0.5, m + 0.5), expand = c(0, 0)) +
      theme_anno2()
    if(isTRUE(plot$plot_env$fixed.xy)) {
      p <- p + coord_fixed()
    }
    plot + anno_row_custom(p, width = width, pos = pos)
  } else {
    plot <- plot + expand_axis(x = xrange, y = yrange)
    ggplot_add(object = obj, plot = plot)
  }
}

#' @importFrom ggplot2 ggplot_add geom_text
#' @importFrom stats order.dendrogram as.dendrogram cutree as.hclust
#' @export
ggplot_add.p_xaxis <- function(object, plot, object_name) {
  if(!isTRUE(plot$plot_env$circular)) {
    stop("Only supports for polar coordinates.", call. = FALSE)
  }
  col.hc <- attr(plot$data, "hclust")$col.hc
  data <- plot$plot_env$polar.args$xaxis_df
  if(isTRUE(object$stretch)) {
    data$x <- seq(1, plot$plot_env$polar.args$xlim[2], length.out = nrow(data))
  }
  bcols <- if(is.list(object$bcols)) object$bcols$col.bcols else object$bcols
  if(!is.null(bcols)) {
    if(!is.null(col.hc)) {
      order <- order.dendrogram(as.dendrogram(col.hc))
      ctree <- cutree(as.hclust(col.hc), length(bcols))[order]
      times <- table(ctree)[unique(ctree)]
      id <- rlang::set_names(rep(seq_along(bcols), times = times), names(ctree))
      object$params$colour <- bcols[id]
    }
  }
  args <- list(mapping = aes_modify(ggplot2::aes_all(names(data)), object$mapping),
               data = data, inherit.aes = FALSE)
  obj <- do.call(geom_text, modifyList(args, object$params))
  ggplot_add(obj, plot)
}

#' @importFrom ggplot2 ggplot_add geom_text
#' @importFrom stats order.dendrogram as.dendrogram cutree as.hclust
#' @export
ggplot_add.p_yaxis <- function(object, plot, object_name) {
  if(!isTRUE(plot$plot_env$circular)) {
    stop("Only supports for polar coordinates.", call. = FALSE)
  }
  row.hc <- attr(plot$data, "hclust")$row.hc
  data <- plot$plot_env$polar.args$yaxis_df
  bcols <- if(is.list(object$bcols)) object$bcols$row.bcols else object$bcols
  if(!is.null(bcols)) {
    if(!is.null(row.hc)) {
      bcols <- rev(bcols)
      order <- order.dendrogram(as.dendrogram(row.hc))
      ctree <- rev(cutree(as.hclust(row.hc), length(bcols))[order])
      times <- table(ctree)[unique(ctree)]
      id <- rlang::set_names(rep(seq_along(bcols), times = times), names(ctree))
      object$params$colour <- bcols[id]
    }
  }
  args <- list(mapping = aes_modify(ggplot2::aes_all(names(data)), object$mapping),
               data = data, inherit.aes = FALSE)
  obj <- do.call(geom_text, modifyList(args, object$params))
  ggplot_add(obj, plot)
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_mark2 <- function(object, plot, object_name) {
  args <- plot$plot_env$polar.args
  if(!isTRUE(plot$plot_env$circular)) {
    warning("`geom_mark2()` only supports for polar coordinates, ",
            "use `geom_mark()` instead.", call. = FALSE)
    obj <- do.call(geom_mark, object)
  } else {
    angle <- args$angle[plot$data$.row.id] + 90 - 180 / diff(args$ylim)
    obj <- do.call(geom_mark, modifyList(object, list(angle = angle)))
  }

  ggplot_add(obj, plot)
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_number2 <- function(object, plot, object_name) {
  args <- plot$plot_env$polar.args
  if(!isTRUE(plot$plot_env$circular)) {
    warning("`geom_number2()` only supports for polar coordinates, ",
            "use `geom_number()` instead.", call. = FALSE)
    obj <- do.call(geom_number, object)
  } else {
    angle <- args$angle[plot$data$.row.id] + 90 - 180 / diff(args$ylim)
    obj <- do.call(geom_number, modifyList(object, list(angle = angle)))
  }

  ggplot_add(obj, plot)
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.anno_row_tree <- function(object, plot, object_name) {
  hc <- attr(plot$data, "hclust")$row.hc
  check_tree_params("row", hc)

  pdata <- plot$data
  n <- length(get_col_name(pdata))
  type <- get_type(pdata)
  circular <- plot$plot_env$circular
  bcols <- object$bcols %||% plot$plot_env$bcols
  bcols <- if(is.list(bcols)) bcols$row.bcols else bcols
  width <- object$width %||% 0.3
  pos <- object$pos

  if(isTRUE(circular)) {
    args <- plot$plot_env$polar.args
    pos <- "left"
    hrange <- c(args$xlim[1], 0.5)
  } else {
    if(is.null(pos)) {
      pos <- switch (type, lower = "left", "right")
    }
    hrange <- c(0.5, width * n + 0.5)
  }

  obj <- build_dendro(hc, circular, bcols, pos, plot$plot_env$fixed.xy, hrange)
  if(isTRUE(circular)) {
    ggplot_add(obj, plot)
  } else {
    .anno_row(plot, obj, pos = pos, width = width)
  }
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.anno_col_tree <- function(object, plot, object_name) {
  hc <- attr(plot$data, "hclust")$col.hc
  check_tree_params("col", hc)

  pdata <- plot$data
  n <- length(get_row_name(pdata))
  type <- get_type(pdata)
  circular <- plot$plot_env$circular
  bcols <- object$bcols %||% plot$plot_env$bcols
  bcols <- if(is.list(bcols)) bcols$col.bcols else bcols
  height <- object$height %||% 0.3
  pos <- object$pos

  if(isTRUE(circular)) {
    args <- plot$plot_env$polar.args
    pos <- "top"
    hrange <- c(0, 0.3 * (args$ylim[2] - n)) + n + 0.5
  } else {
    if(is.null(pos)) {
      pos <- switch (type, lower = "bottom", "top")
    }
    hrange <- if(pos == "bottom") c(- height * n + 0.5, 0.5) else c(0.5, height * n + 0.5) + n
  }

  obj <- build_dendro(hc, circular, bcols, pos, plot$plot_env$fixed.xy, hrange, circular)
  if(isTRUE(circular)) {
    ggplot_add(obj, plot)
  } else {
    .anno_col(plot, obj, pos = pos, height = height)
  }
}

#' @noRd
check_tree_params <- function(index, hc) {
  if(index == "row" && is.null(hc)) {
    stop("Did you forget to cluster rows of the matrix?", call. = FALSE)
  }
  if(index == "col" && is.null(hc)) {
    stop("Did you forget to cluster columns of the matrix?", call. = FALSE)
  }
}

#' @importFrom ggplot2 ggplot_add geom_rect
#' @importFrom stats cutree
#' @export
ggplot_add.anno_hc_rect <- function(object, plot, object_name) {
  pdata <- plot$data
  if(!is_symmet(pdata)) {
    stop("Just supports for symmetric correlation matrix.", call. = FALSE)
  }
  hc <- attr(pdata, "hclust")$row.hc %||% attr(pdata, "hclust")$col.hc
  if(is.null(hc)) {
    return(plot)
  }
  n <- length(get_col_name(pdata))
  k <- object$k
  tree <- cutree(hc, k = k)
  v <- table(tree)[unique(tree[hc$order])]
  cv <- c(0, cumsum(v))
  data <- data.frame(xmin = cv[-(k + 1)] + 0.5, ymin = n - cv[-(k + 1)] + 0.5,
                     xmax = cv[-1] + 0.5, ymax = n - cv[-1] + 0.5)
  mapping <- aes_string(xmin = "xmin", ymin = "ymin", xmax = "xmax", ymax = "ymax")
  obj <- ggplot2::geom_rect(mapping = mapping, data = data, fill = object$fill,
                            colour = object$colour, size = object$size, inherit.aes = FALSE)
  ggplot_add(obj, plot)
}

#' @importFrom ggplot2 ggplot geom_bar scale_x_reverse scale_y_reverse
#' @export
ggplot_add.anno_bar <- function(object, plot, object_name) {
  stopifnot(inherits(plot, "quickcor") && isTRUE(plot$plot_env$circular))
  type <- get_type(plot$data)
  trans <- object$trans
  pos <- object$pos
  if(is.null(pos)) {
    if(!"y" %in% names(object$mapping)) {
      pos <- switch (type, lower = "bottom", "top")
    }
    if(!"x" %in% names(object$mapping)) {
      pos <- switch (type, lower = "left", "right")
    }
  }
  vertical <- pos %in% c("bottom", "top")
  nm <- if(vertical) rlang::as_name(object$mapping$x) else rlang::as_name(object$mapping$y)
  if(isTRUE(object$align)) {
    if(vertical) {
      object$mapping$x <- aes_factor_expr(nm, get_col_name(plot$data))
    } else {
      object$mapping$y <- aes_factor_expr(nm, levels = rev(get_row_name(plot$data)))
    }
  }
  p <- ggplot(object$data, object$mapping) + do.call(geom_bar, object$params)

  if(!plot$coordinates$is_free()) {
    p <- p + coord_fixed()
    if(is.null(trans)) {
      from <- c(0, max(table(object$data[[nm]])))
      to <- if(vertical) {
        c(0, object$height * nrows(plot$data))
      } else {
        c(0, object$width * ncols(plot$data))
      }
      trans <- if(pos %in% c("top", "right")) {
        liner_trans(from, to)
      } else reverse_liner_trans(from, to)
    }
  }

  if(vertical) {
    p <- p + scale_x_discrete(limits = xrange(plot), expand = c(0, 0))
    if(pos == "top") {
      if(!is.null(trans)) {
        p <- p + scale_y_continuous(trans = trans)
      }
    }
    if(pos == "bottom") {
      if(!is.null(trans)) {
        p <- p + scale_y_continuous(trans = trans)
      } else {
        p <- p + scale_y_reverse()
      }
    }
  } else {
    p <- p + scale_y_discrete(limits = yrange(plot), expand = c(0, 0))
    if(pos == "right") {
      if(!is.null(trans)) {
        p <- p + scale_x_continuous(trans = trans)
      }
    }
    if(pos == "left") {
      if(!is.null(trans)) {
        p <- p + scale_x_continuous(trans = trans)
      } else {
        p <- p + scale_x_reverse()
      }
    }
  }

  if(is.character(object$theme)) {
    p <- p + match.fun(object$theme)()
  } else {
    p <- p + object$theme
  }
  remove.axis <- if(object$remove.axis == "auto") {
    if(vertical) "x" else "y"
  } else object$remove.axis
  if(object$remove.axis != "none")
    p <- p + remove_axis(remove.axis)

  if(vertical) {
    .anno_col(plot, p, height = object$height, pos = pos)
  } else {
    .anno_row(plot, p, width = object$width, pos = pos)
  }
}

#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 scale_x_reverse
#' @importFrom ggplot2 scale_y_reverse
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_y_discrete
#' @export
ggplot_add.anno_bar2 <- function(object, plot, object_name) {
  stopifnot(inherits(plot, "quickcor") && isTRUE(plot$plot_env$circular))
  type <- get_type(plot$data)
  data <- object$data
  trans <- object$trans
  pos <- object$pos
  xname <- rlang::as_name(object$mapping$x)
  yname <- rlang::as_name(object$mapping$y)
  if((!is_binary(data[[xname]]) && !is_binary(data[[yname]])) ||
     (is_binary(data[[xname]]) && is_binary(data[[yname]]))) {
    stop("`anno_bar2()` should have one binary and one numeric position vars .",
         call. = FALSE)
  }
  if(is.null(pos)) {
    if(is_binary(data[[xname]])) {
      pos <- switch (type, lower = "bottom", "top")
    } else {
      pos <- switch (type, lower = "left", "right")
    }
  }
  vertical <- pos %in% c("bottom", "top")

  if(isTRUE(object$align)) {
    if(vertical) {
      object$mapping$x <- aes_factor_expr(nm, get_col_name(plot$data))
    } else {
      object$mapping$y <- aes_factor_expr(nm, levels = rev(get_row_name(plot$data)))
    }
  }

  p <- ggplot(object$data, object$mapping) + do.call(geom_col, object$params)

  if(!plot$coordinates$is_free()) {
    p <- p + coord_fixed()
    if(is.null(trans)) {
      all_geq_zero <- if(vertical) all(data[[yname]] >= 0) else all(data[[xname]] >= 0)
      all_leq_zero <- if(vertical) all(data[[yname]] <= 0) else all(data[[xname]] <= 0)
      if(vertical) {
        from <- if(all_geq_zero) {
          c(0, max(data[[yname]], na.rm = TRUE))
        } else if(all_leq_zero) {
          c(min(data[[yname]], na.rm = TRUE), 0)
        } else {
          range(data[[yname]], na.rm = TRUE)
        }
        to <- if(all_geq_zero) {
          c(0, object$height * nrows(plot$data))
        } else if(all_leq_zero) {
          c(- object$height * nrows(plot$data), 0)
        } else {
          c(from[1] / diff(from), from[2] / diff(from)) * object$height * nrows(plot$data)
        }
      } else {
        from <- if(all_geq_zero) {
          c(0, max(data[[xname]], na.rm = TRUE))
        } else if(all_leq_zero) {
          c(min(data[[xname]], na.rm = TRUE), 0)
        } else {
          range(data[[xname]], na.rm = TRUE)
        }
        to <- if(all_geq_zero) {
          c(0, object$width * ncols(plot$data))
        } else if(all_leq_zero) {
          c(- object$width * ncols(plot$data), 0)
        } else {
          c(from[1] / diff(from), from[2] / diff(from)) * object$width * ncols(plot$data)
        }
      }
      trans <- if(pos %in% c("top", "right")) {
        liner_trans(from, to)
      } else reverse_liner_trans(from, to)
    }
  }
  if(vertical) {
    p <- p + scale_x_discrete(limits = xrange(plot), expand = c(0, 0))
    if(pos == "top") {
      if(!is.null(trans)) {
        p <- p + scale_y_continuous(trans = trans)
      }
    }
    if(pos == "bottom") {
      if(!is.null(trans)) {
        p <- p + scale_y_continuous(trans = trans)
      } else {
        p <- p + scale_y_reverse()
      }
    }
  } else {
    p <- p + scale_y_discrete(limits = yrange(plot), expand = c(0, 0))
    if(pos == "right") {
      if(!is.null(trans)) {
        p <- p + scale_x_continuous(trans = trans)
      }
    }
    if(pos == "left") {
      if(!is.null(trans)) {
        p <- p + scale_x_continuous(trans = trans)
      } else {
        p <- p + scale_x_reverse()
      }
    }
  }

  if(is.character(object$theme)) {
    p <- p + match.fun(object$theme)()
  } else {
    p <- p + object$theme
  }
  remove.axis <- if(object$remove.axis == "auto") {
    if(vertical) "x" else "y"
  } else object$remove.axis
  if(object$remove.axis != "none")
    p <- p + remove_axis(remove.axis)

  if(vertical) {
    .anno_col(plot, p, height = object$height, pos = pos)
  } else {
    .anno_row(plot, p, width = object$width, pos = pos)
  }
}

#' @importFrom ggplot2 geom_boxplot
#' @export
ggplot_add.anno_boxplot <- function(object, plot, object_name) {
  stopifnot(inherits(plot, "quickcor") && isTRUE(plot$plot_env$circular))
  type <- get_type(plot$data)
  data <- object$data
  trans <- object$trans
  pos <- object$pos
  xname <- rlang::as_name(object$mapping$x)
  yname <- rlang::as_name(object$mapping$y)
  if((!is_binary(data[[xname]]) && !is_binary(data[[yname]])) ||
     (is_binary(data[[xname]]) && is_binary(data[[yname]]))) {
    stop("`anno_boxplot()` should have one binary and one numeric position vars .",
         call. = FALSE)
  }
  if(is.null(pos)) {
    if(is_binary(data[[xname]])) {
      pos <- switch (type, lower = "bottom", "top")
    } else {
      pos <- switch (type, lower = "left", "right")
    }
  }
  vertical <- pos %in% c("bottom", "top")

  if(isTRUE(object$align)) {
    if(vertical) {
      object$mapping$x <- aes_factor_expr(nm, get_col_name(plot$data))
    } else {
      object$mapping$y <- aes_factor_expr(nm, levels = rev(get_row_name(plot$data)))
    }
  }

  p <- ggplot(object$data, object$mapping) + do.call(geom_boxplot, object$params)

  if(!plot$coordinates$is_free()) {
    p <- p + coord_fixed()
    if(is.null(trans)) {
      if(vertical) {
        from <- range(data[[yname]], na.rm = TRUE)
        to <- c(from[1] / diff(from), from[2] / diff(from)) * object$height * nrows(plot$data)
      } else {
        from <- range(data[[xname]], na.rm = TRUE)
        to <- c(from[1] / diff(from), from[2] / diff(from)) * object$width * ncols(plot$data)
      }
      trans <- if(pos %in% c("top", "right")) {
        liner_trans(from, to)
      } else reverse_liner_trans(from, to)
    }
  }
  if(vertical) {
    p <- p + scale_x_discrete(limits = xrange(plot), expand = c(0, 0))
    if(pos == "top") {
      if(!is.null(trans)) {
        p <- p + scale_y_continuous(trans = trans)
      }
    }
    if(pos == "bottom") {
      if(!is.null(trans)) {
        p <- p + scale_y_continuous(trans = trans)
      } else {
        p <- p + scale_y_reverse()
      }
    }
  } else {
    p <- p + scale_y_discrete(limits = yrange(plot), expand = c(0, 0))
    if(pos == "right") {
      if(!is.null(trans)) {
        p <- p + scale_x_continuous(trans = trans)
      }
    }
    if(pos == "left") {
      if(!is.null(trans)) {
        p <- p + scale_x_continuous(trans = trans)
      } else {
        p <- p + scale_x_reverse()
      }
    }
  }

  if(is.character(object$theme)) {
    p <- p + match.fun(object$theme)()
  } else {
    p <- p + object$theme
  }
  remove.axis <- if(object$remove.axis == "auto") {
    if(vertical) "x" else "y"
  } else object$remove.axis
  if(object$remove.axis != "none")
    p <- p + remove_axis(remove.axis)

  if(vertical) {
    .anno_col(plot, p, height = object$height, pos = pos)
  } else {
    .anno_row(plot, p, width = object$width, pos = pos)
  }
}

#' @importFrom ggplot2 geom_point
#' @export
ggplot_add.anno_point <- function(object, plot, object_name) {
  stopifnot(inherits(plot, "quickcor") && isTRUE(plot$plot_env$circular))
  type <- get_type(plot$data)
  data <- object$data
  pos <- object$pos
  xname <- rlang::as_name(object$mapping$x)
  yname <- rlang::as_name(object$mapping$y)
  if(!is_binary(data[[xname]]) || !is_binary(data[[yname]])) {
    stop("`anno_point()` only support for binary position vars.",
         call. = FALSE)
  }
  if(is.null(pos)) {
    if(all(unique(data[[xname]]) %in% get_col_name(plot$data))) {
      pos <- switch(type, lower = "bottom", "top")
    } else {
      pos <- switch(type, lower = "left", "right")
    }
  }
  vertical <- pos %in% c("bottom", "top")

  if(isTRUE(object$align)) {
    if(vertical) {
      object$mapping$x <- aes_factor_expr(nm, get_col_name(plot$data))
    } else {
      object$mapping$y <- aes_factor_expr(nm, levels = rev(get_row_name(plot$data)))
    }
  }
  p <- ggplot(object$data, object$mapping) + do.call(geom_point, object$params)

  if(!plot$coordinates$is_free()) {
    p <- p + coord_fixed()
  }

  if(vertical) {
    p <- p + scale_x_discrete(limits = xrange(plot), expand = c(0, 0))
  } else {
    p <- p + scale_y_discrete(limits = yrange(plot), expand = c(0, 0))
  }

  if(is.character(object$theme)) {
    p <- p + match.fun(object$theme)()
  } else {
    p <- p + object$theme
  }
  remove.axis <- if(object$remove.axis == "auto") {
    if(vertical) "x" else "y"
  } else object$remove.axis
  if(object$remove.axis != "none")
    p <- p + remove_axis(remove.axis)

  if(vertical) {
    .anno_col(plot, p, height = object$height, pos = pos)
  } else {
    .anno_row(plot, p, width = object$width, pos = pos)
  }
}

#' @noRd
is_binary <- function(x) {
  is.character(x) || is.factor(x)
}

#' @noRd
aes_factor_expr <- function(name, levels) {
  levels <- deparse(substitute(levels))
  str <- paste0("factor(", name, ",", "levels = ",
         levels, ")")
  as.expression(str)
}
