#' @importFrom ggplot2 ggplot_add geom_segment
#' @export
ggplot_add.geom_panel_grid <- function(object, plot, object_name) {
  drop <- plot$plot_env$drop
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
  if(type != "full" && !isTRUE(show.diag)) {
    if(isTRUE(drop)) {
      show.diag <- TRUE
      n <- n - 1
      m <- m - 1
    }
  }
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
  geom_fun <- switch (object$geom,
                      text = rvcheck::get_fun_from_pkg("ggplot2", "geom_text"),
                      label = rvcheck::get_fun_from_pkg("ggplot2", "geom_label"),
                      image = rvcheck::get_fun_from_pkg("ggimage", "geom_image")
  )
  
  type <- get_type(plot$data)
  row.names <- get_row_name(plot$data)
  show.diag <- get_show_diag(plot$data)
  
  n <- length(row.names)
  x <- 1:n
  y <- n:1
  if(isTRUE(plot$plot_env$drop)) {
    if(type == "lower") {
      plot <- plot + ggplot2::expand_limits(x = c(0.5, n + 0.5),
                                            y = c(0.5, n + 0.5))
      
    }
    if(type == "upper") {
      if(!isTRUE(show.diag)) {
        plot <- plot + ggplot2::expand_limits(x = c(-0.5, n - 0.5),
                                              y = c(-0.5, n - 0.5))
        x <- x - 1
        y <- y - 1
      }
    }
  }
  d <- new_data_frame(list(x = x, y = y, label = row.names))
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

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_links <- function(object, plot, object_name) {
  layout <- object$layout
  if(layout != "combination") {
    stop("Currently just support for 'combination' layout.", call. = FALSE)
  }
  layout.params <- modifyList(object$layout.params, list(cor_tbl = plot$data))
  data <- do.call(combination_layout, layout.params)
  obj <- do.call(geom_link2, object$params)
  ggplot_add(object = obj, plot = plot)
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_links_label <- function(object, plot, object_name) {
  geom_fun <- switch (object$geom,
                      text = rvcheck::get_fun_from_pkg("ggplot2", "geom_text"),
                      label = rvcheck::get_fun_from_pkg("ggplot2", "geom_label"),
                      image = rvcheck::get_fun_from_pkg("ggimage", "geom_image")
  )
  
  
}