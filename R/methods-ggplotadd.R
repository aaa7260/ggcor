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
                      text = rvcheck::get_fun_from_pkg("ggplot2", "geom_text"),
                      label = rvcheck::get_fun_from_pkg("ggplot2", "geom_label"),
                      image = rvcheck::get_fun_from_pkg("ggimage", "geom_image")
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
ggplot_add.geom_links <- function(object, plot, object_name) {
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
ggplot_add.geom_links_label <- function(object, plot, object_name) {
  layout_tbl <- plot$plot_env$layout_tbl
  if(is.null(layout_tbl)) {
    warning("Can only be used after `geom_links2()`.", call. = FALSE)
    return(plot)
  }

  geom <- match.arg(object$geom, c("text", "label", "image"))
  geom_fun <- switch (geom,
                      text = rvcheck::get_fun_from_pkg("ggplot2", "geom_text"),
                      label = rvcheck::get_fun_from_pkg("ggplot2", "geom_label"),
                      image = rvcheck::get_fun_from_pkg("ggimage", "geom_image"))

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
#' @export
ggplot_add.p_xaxis <- function(object, plot, object_name) {
  if(!isTRUE(plot$plot_env$circular)) {
    stop("Only supports for polar coordinates.", call. = FALSE)
  }
  data <- plot$plot_env$polar.args$xaxis_df
  args <- list(mapping = aes_modify(ggplot2::aes_all(names(data)), object$params$mapping),
               data = data, inherit.aes = FALSE)
  obj <- do.call(geom_text, modifyList(args, object$params))
  ggplot_add(obj, plot)
}

#' @importFrom ggplot2 ggplot_add geom_text
#' @export
ggplot_add.p_yaxis <- function(object, plot, object_name) {
  if(!isTRUE(plot$plot_env$circular)) {
    stop("Only supports for polar coordinates.", call. = FALSE)
  }
  data <- plot$plot_env$polar.args$yaxis_df
  args <- list(mapping = aes_modify(ggplot2::aes_all(names(data)), object$params$mapping),
               data = data, inherit.aes = FALSE)
  obj <- do.call(geom_text, modifyList(args, object$params))
  ggplot_add(obj, plot)
}
