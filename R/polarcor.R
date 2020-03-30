#' Visualize correlation matrix on polar coordinate
#' @description The \code{trans_polar} function transforms the cor_tbl
#' object into polar coordinate format, and \code{polarcor}, \code{geom_arc_colour}
#' and other functions are used for visualization.
#' @param data a data.frame.
#' @param groups character vectors.
#' @param no.axis if TRUE, don't add axis labels for plot.
#' @param ... extra parameters.
#' @rdname polarcor
#' @examples
#' polarcor(mtcars, mapping = aes(fill = r))
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
polarcor <- function(data,
                     no.axis = FALSE,
                     ...) {
  data <- trans_polar(data, ...)
  is.symmet <- attr(data, "is.symmet")
  p <- ggplot2::ggplot(data)
  if(!isTRUE(no.axis)) {
    p <- p + set_arc_axis_x()
    if(!is.symmet) {
      p <- p + set_arc_axis_y()
    }
  }
  p + ggplot2::theme_void()
}

#' @rdname polarcor
#' @export
geom_arc_colour <- function(mapping = NULL,
                            data = NULL,
                            ...)
{
  if(!requireNamespace("ggforce")) {
    stop("'ggforce' package is not installed.", call. = FALSE)
  }
  base.mapping <- ggplot2::aes_string(x0 = 0, y0 = 0, r0 = "r1", r = "r2",
                                      start = "start", end = "end")
  mapping <- aes_modify(base.mapping, mapping)
  ggforce::geom_arc_bar(mapping = mapping, data = data, ...)
}

#' @rdname polarcor
#' @export
set_arc_axis_x <- function(mapping = NULL, data = NULL, groups = NULL, ...) {
  if(!is.null(data)) {
    if(!inherits(data, "polar_tbl")) {
      stop("Need a polar_tbl object.", call. = FALSE)
    }
    data <- attr(data, "axis.x.pos")
    if(!is.null(groups)) {
      data <- dplyr::filter(data, .group %in% groups)
    }
  } else {
    data <- function(data) {
      data <- attr(data, "axis.x.pos")
      if(!is.null(groups)) {
        data <- dplyr::filter(data, .group %in% groups)
      }
      data
    }
  }

  base.mapping <- ggplot2::aes_string(x = "x", y = "y", angle = "angle",
                                      label = "label")
  mapping <- aes_modify(base.mapping, mapping)
  ggplot2::geom_text(mapping = mapping, data = data, ..., hjust = 0,
                     inherit.aes = FALSE)
}

#' @rdname polarcor
#' @export
set_arc_axis_y <- function(mapping = NULL, data = NULL, ...) {
  if(!is.null(data)) {
    if(!inherits(data, "polar_df")) {
      stop("Need a polar_df object.", call. = FALSE)
    }
    data <- attr(data, "axis.y.pos")
  } else {
    data <- function(data) {
      attr(data, "axis.y.pos")
    }
  }

  base.mapping <- ggplot2::aes_string(x = "x", y = "y", label = "label",
                                      angle = "angle")
  mapping <- aes_modify(base.mapping, mapping)
  ggplot2::geom_text(mapping = mapping, data = data, ..., inherit.aes = FALSE)
}

#' Transforms cor_tbl into polar coordinate format
#' @description \code{trans_polar} function transforms the cor_tbl
#' object into polar coordinate format, and this format can draw polar
#' matrix heatmap on cartesian coordinate.
#' @param data a cor_tbl object or others that can be convert to cor_tbl object.
#' @param start.angle,end.angle angle of start and end point (in degree).
#' @param inner.radius,outer.radius,axis.x.radius inner/outer/x axis labels radius.
#' @param split.by.group if TRUE, will split the plot region on groups.
#' @param group.space the space between each group (in degree).
#' @param ... extra parameters.
#' @return a data.frame.
#' @rdname trans_polar
#' @examples
#' trans_polar(mtcars)
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
trans_polar <- function(data,
                        start.angle = NULL,
                        end.angle = NULL,
                        inner.radius = 0.4,
                        outer.radius = 1,
                        axis.x.radius = 1.02,
                        split.by.group = FALSE,
                        group.space = 1, ## in degree
                        ...)
{
  if(!is_cor_tbl(data)) {
    data <- fortify_cor(data, ...)
  }
  is.symmet <- is_symmet(data)
  row.names <- get_row_name(data)
  col.names <- get_col_name(data)
  grouped <- attr(data, "grouped")
  type <- get_type(data)
  n <- length(row.names)
  m <- length(col.names)

  if(!isTRUE(grouped)) {
    if(isTRUE(split.by.group)) {
      warning("The input data should be a grouped cor_tbl.", call. = FALSE)
    }
    split.by.group <- FALSE
  }

  if(is.null(start.angle)) {
    start.angle <- if(is.symmet) 0 else 1 / 12 * pi
  } else {
    start.angle <- degree_radius(start.angle %% 360)
  }
  if(is.null(end.angle)) {
    end.angle <- if(is.symmet) 2 * pi else 23 / 12 * pi
  } else {
    end.angle <- degree_radius(end.angle %% 360)
  }

  if(identical(end.angle, 0)) {
    end.angle <- 2 * pi
  }

  if(start.angle > end.angle) {
    temp <- start.angle
    start.angle <- end.angle
    end.angle <- temp
  }

  mid.angle <- ((start.angle + end.angle) / 2) + pi

  if(inner.radius > outer.radius) {
    temp <- inner.radius
    inner.radius <- outer.radius
    outer.radius <- temp
  }

  r <- seq(inner.radius, outer.radius, length.out = n + 1)

  if(isTRUE(grouped) && isTRUE(split.by.group)) {
    data <- split(data, data$.group)
    name <- names(data)
    group.space <- degree_radius(group.space)
    enclose <- identical(start.angle, end.angle - 2 * pi) ||
      ((end.angle -  start.angle) < group.space)
    l <- length(data)
    cell.angle <- if(enclose) {
      (end.angle - start.angle) / l - group.space
    } else {
      (end.angle - start.angle -  group.space * (l - 1)) / l
    }
    start.angle <- rlang::set_names(start.angle + (1:l - 1) * (cell.angle + group.space), name)
    end.angle <- rlang::set_names(start.angle + cell.angle, name)

    data <- purrr::map_dfr(name, function(.name) {
      t <- seq(start.angle[.name], end.angle[.name], length.out = m + 1)
      pos <- tibble::tibble(start = t[-(m + 1)][data[[.name]]$.col.id],
                            end = t[-1][data[[.name]]$.col.id],
                            r1 = r[-(n + 1)][data[[.name]]$.row.id],
                            r2 = r[-1][data[[.name]]$.row.id])
      dplyr::bind_cols(pos, data)
    })

    axis.x.pos <- purrr::map_dfr(name, function(.name) {
      t <- seq(start.angle[.name], end.angle[.name], length.out = m + 1)
      tibble::tibble(x = axis.x.radius * sin((t[-(m + 1)] + t[-1]) / 2),
                     y = axis.x.radius * cos((t[-(m + 1)] + t[-1]) / 2),
                     label = col.names,
                     angle = ggraph::node_angle(x, y),
                     .group = .name)
    })
  } else {
    t <- seq(start.angle, end.angle, length.out = m + 1)
    pos <- tibble::tibble(start = t[-(m + 1)][data$.col.id],
                          end = t[-1][data$.col.id],
                          r1 = r[-(n + 1)][data$.row.id],
                          r2 = r[-1][data$.row.id])
    data <- dplyr::bind_cols(pos, data)

    axis.x.pos <- tibble::tibble(x = axis.x.radius * sin((t[-(m + 1)] + t[-1]) / 2),
                                 y = axis.x.radius * cos((t[-(m + 1)] + t[-1]) / 2),
                                 label = col.names,
                                 angle = ggraph::node_angle(x, y))
  }

  # pos in cartesian coordinate
  data <- dplyr::mutate(data,
    x = (r1 + r2) * sin((start + end) / 2) / 2,
    y = (r1 + r2) * cos((start + end) / 2) / 2,
    angle = (cell_angle(x, y))
  ) %>% dplyr::arrange(x, y, start, end, r1, r2, angle)
  mid.r <- (r[-(n + 1)] + r[-1]) / 2
  axis.y.pos <- tibble::tibble(x = mid.r * sin(mid.angle),
                               y = mid.r * cos(mid.angle),
                               label = rev(row.names),
                               angle = radius_degree(mid.angle) %% 360)

  structure(.Data = data,
            axis.x.pos = axis.x.pos,
            axis.y.pos = axis.y.pos,
            start.angle = start.angle,
            end.angle = end.angle,
            split.by.group = split.by.group,
            is.symmet = is.symmet,
            class = c("polar_tbl", class(data))
            )
}

#' @noRd
degree_radius <- function(x) {
  x / 180 * pi
}

#' @noRd
radius_degree <- function(x) {
  x * 180 / pi
}

#' @noRd
cell_angle <- function(x, y) {
  angle <- radius_degree(atan2(y, x))
  (angle + 90) %% 360
}
