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

#' @importFrom ggplot2 ggplot_add aes_string
#' @export
ggplot_add.anno_link <- function(object, plot, object_name) {
  pdata <- plot$data
  type <- get_type(pdata)
  show.diag <- get_show_diag(pdata)
  n <- length(get_col_name(pdata))
  m <- length(get_row_name(pdata))
  mapping <- aes_modify(aes_string(x = "x", y = "y", xend = "xend", yend = "yend"),
                        object$mapping)

  layout <- if(type != "full") "triangle" else "parallel"
  layout.params <- modifyList(object$layout.params,
                              list(data = object$data, cor_tbl = pdata))
  data <- if(layout == "triangle") {
    do.call(triangle_layout, layout.params)
  } else {
    do.call(parallel_layout, layout.params)
  }
  plot$plot_env$layout_tbl <- data


  params <- modifyList(list(data = data, mapping = mapping), object$params)

  min <- min(data$x, na.rm = TRUE)
  max <- max(data$x, na.rm = TRUE)
  if(type == "upper") {
    if(isTRUE(show.diag)) {
      xrange <- c(min(-0.5, min - 0.2 * n), n + 0.5)
      yrange <- c(0.5, n + 0.5)
    } else {
      xrange <- c(min(-0.5, min - 0.2 * n), n - 0.5)
      yrange <- c(-0.5, n - 0.5)
    }

  } else if (type == "lower") {
    if(isTRUE(show.diag)) {
      xrange <- c(0.5, max(max + 0.2 * n, n + 1.5))
      yrange <- c(0.5, n + 0.5)
    } else {
      xrange <- c(0.5, max(max + 0.2 * n, n + 0.5))
      yrange <- c(0.5, n + 0.5)
    }
  } else {
    xrange <- c(n + 1.5, max + 0.2 * n)
    yrange <- c(0.5, m + 0.5)
  }
  plot <- plot + expand_axis(x = xrange, y = yrange)
  obj <- do.call(geom_links2, params)
  ggplot_add(object = obj, plot = plot)
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.anno_link_label <- function(object, plot, object_name) {
  layout_tbl <- plot$plot_env$layout_tbl
  if(is.null(layout_tbl)) {
    warning("Can only be used after `geom_links2()`.", call. = FALSE)
    return(plot)
  }

  geom <- match.arg(object$geom, c("text", "label", "image"))
  geom_fun <- switch (geom,
                      text = get_function("ggplot2", "geom_text"),
                      label = get_function("ggplot2", "geom_label"),
                      image = get_function("ggimage", "geom_image"))

  type <- get_type(plot$data)
  data <- attr(layout_tbl, "node.pos")

  if(!is.null(object$is.start)) {
    is.start <- NULL
    if(isTRUE(object$is.start)) {
      data <- dplyr::filter(data, is.start)
    } else {
      data <- dplyr::filter(data, !is.start)
    }
  }

  if(type == "upper") {
    data$hjust <- ifelse(data$is.start, 1, 0)
    nudge_x <- ifelse(data$is.start, -object$nudge_x, object$nudge_x)
  } else {
    data$hjust <- ifelse(data$is.start, 0, 1)
    nudge_x <- ifelse(data$is.start, object$nudge_x, -object$nudge_x)
  }

  mapping <- if(geom == "image") {
    if(!"image" %in% names(object$params)) {
      stop("Did you forget to set the 'image' parameter?", call. = FALSE)
    }
    aes_string(x = "x", y = "y")
  } else aes_string(x = "x", y = "y", label = "label", hjust = "hjust")
  mapping <- aes_modify(mapping, object$mapping)

  args <- list(mapping = mapping, data = data, inherit.aes = FALSE,
               nudge_x = nudge_x)
  params <- modifyList(args, object$params)
  obj <- do.call(geom_fun, params)
  ggplot_add(obj, plot)
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
ggplot_add.anno_tree <- function(object, plot, object_name) {
  args <- plot$plot_env$polar.args
  pdata <- plot$data
  bcols <- object$bcols %||% plot$plot_env$bcols
  row.bcols <- if(is.list(bcols)) bcols$row.bcols else bcols
  col.bcols <- if(is.list(bcols)) bcols$col.bcols else bcols
  hc <- attr(pdata, "hclust")
  circular <- plot$plot_env$circular
  index <- object$index
  type <- get_type(pdata)
  n <- length(get_row_name(pdata))
  m <- length(get_col_name(pdata))

  check_tree_params(index, hc)

  if(!isTRUE(circular) && type != "full") {
    stop("Now `anno_tree()` only supports polar coordinates or 'type = full'.", call. = FALSE)
  }

  xlim <- ylim <- NULL
  if(isTRUE(circular)) {
    row.rng <- c(args$xlim[1], 0.5)
    col.rng <- c(n + 0.5, n + 0.5 + 0.3 * (args$ylim[2] - n - 0.5))
  } else {
    min <- min(n, m)
    row.rng <- if(is.null(object$row.height)) {
      c(m + 0.5, 0.3 * min + m + 0.5)
    } else {
      c(m + 0.5, (1 + object$row.height) * m + 0.5)
    }
    col.rng <- if(is.null(object$col.height)) {
      c(n + 0.5, 0.3 * min + n + 0.5)
    } else {
      c(n + 0.5, (1 + object$col.height) * n + 0.5)
    }
    xlim <- c(row.rng[1], row.rng[2] + 0.05 * min)
    ylim <- c(col.rng[1], col.rng[2] + 0.05 * min)
  }

  if(object$index == "all") {
    row.data <- dend_tbl(hc$row.hc, row.bcols, TRUE, row.rng, circular)
    col.data <- dend_tbl(hc$col.hc, col.bcols, FALSE, col.rng, circular)
    mapping <- aes_string(x = "x", y = "y", xend = "xend", yend = "yend")
    rparams <- suppressWarnings(
      list(mapping = mapping, data = row.data,
           colour = object$colour %||% row.data$colour %||% "black",
           size = object$size %||% row.data$size %||% 0.5,
           linetype = object$linetype %||% row.data$linetype %||% "solid",
           inherit.aes = FALSE)
    )
    cparams <- suppressWarnings(
      list(mapping = mapping, data = col.data,
           colour = object$colour %||% col.data$colour %||% "black",
           size = object$size %||% col.data$size %||% 0.5,
           linetype = object$linetype %||% col.data$linetype %||% "solid",
           inherit.aes = FALSE)
    )
    row.tree <- do.call(geom_segment, rparams)
    col.tree <- do.call(geom_segment, cparams)
    plot <- plot + expand_axis(x = xlim, y = ylim)
    ggplot_add(list(row.tree, col.tree), plot)
  } else if(object$index == "row") {
    row.data <- dend_tbl(hc$row.hc, bcols, TRUE, row.rng, circular)
    mapping <- aes_string(x = "x", y = "y", xend = "xend", yend = "yend")
    rparams <- suppressWarnings(
      list(mapping = mapping, data = row.data,
           colour = object$colour %||% row.data$colour %||% "black",
           size = object$size %||% row.data$size %||% 0.5,
           linetype = object$linetype %||% row.data$linetype %||% "solid",
           inherit.aes = FALSE)
    )
    row.tree <- do.call(geom_segment, rparams)
    plot <- plot + expand_axis(x = xlim)
    ggplot_add(row.tree, plot)
  } else {
    col.data <- dend_tbl(hc$col.hc, bcols, FALSE, col.rng, circular)
    mapping <- aes_string(x = "x", y = "y", xend = "xend", yend = "yend")
    cparams <- suppressWarnings(
      list(mapping = mapping, data = col.data,
           colour = object$colour %||% col.data$colour %||% "black",
           size = object$size %||% col.data$size %||% 0.5,
           linetype = object$linetype %||% col.data$linetype %||% "solid",
           inherit.aes = FALSE)
    )
    col.tree <- do.call(geom_segment, cparams)
    plot <- plot + expand_axis(y = ylim)
    ggplot_add(col.tree, plot)
  }
}

#' @noRd
check_tree_params <- function(index, hc) {
  if(index == "all" && any(vapply(hc, is.null, logical(1)))) {
    stop("Did you forget to cluster the matrix?", call. = FALSE)
  }
  if(index == "row" && is.null(hc$row.hc)) {
    stop("Did you forget to cluster rows of the matrix?", call. = FALSE)
  }
  if(index == "col" && is.null(hc$col.hc)) {
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
