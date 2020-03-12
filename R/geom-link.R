#' Special layer function for correlation link plot
#' @description A set of custom layer functions that quickly add
#' layers of curves, nodes, and labels.
#' @param mapping aesthetic mappings parameters.
#' @param data NULL or a layout_link_tbl object that create by
#' \code{parallel_layout()} or \code{combination_layout()}.
#' @param  curvature a numeric value giving the amount of curvature.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than
#' combining with them.
#' @param ... extra parameters passing to layer function.
#' @return geom layer.
#' @importFrom ggplot2 aes_string geom_curve geom_point geom_text
#' @importFrom dplyr filter
#' @rdname geom_link
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_link <- function(mapping = NULL,
                      data = NULL,
                      curvature = 0,
                      inherit.aes = getOption("ggcor.link.inherit.aes", TRUE),
                      ...)
{
  mapping <- aes_modify(
    aes_string(x = "x", y = "y", xend = "xend", yend = "yend"), mapping
  )
  geom_curve(mapping = mapping, data = data, curvature = curvature,
             inherit.aes = inherit.aes, ...)
}

#' @rdname geom_link
#' @export
geom_link_point <- function(...)
{
  list(
    geom_start_point(...),
    geom_end_point(...)
  )
}

#' @rdname geom_link
#' @export
geom_link_label <- function(...)
{
  list(
    geom_start_label(...),
    geom_end_label(...)
  )
}

#' @rdname geom_link
#' @export
geom_start_point <- function(mapping = NULL,
                             data = NULL,
                             inherit.aes = getOption("ggcor.link.inherit.aes", TRUE),
                             ...)
{
  if(!is.null(data) && !inherits(data, "layout_link_tbl")) {
    stop("Need a layout_link_tbl.", call. = FALSE)
  }
  data <- if(is.null(data)) {
    get_start_nodes()
  } else {
    get_start_nodes()(data)
  }
  mapping <- aes_modify(
    aes_string(x = "x", y = "y"), mapping
  )
  geom_point(mapping = mapping, data = data, inherit.aes = inherit.aes, ...)
}

#' @rdname geom_link
#' @export
geom_end_point <- function(mapping = NULL,
                           data = NULL,
                           inherit.aes = getOption("ggcor.link.inherit.aes", TRUE),
                           ...)
{
  if(!is.null(data) && !inherits(data, "layout_link_tbl")) {
    stop("Need a layout_link_tbl.", call. = FALSE)
  }
  data <- if(is.null(data)) {
    get_end_nodes()
  } else {
    get_end_nodes()(data)
  }
  mapping <- aes_modify(
    aes_string(x = "xend", y = "yend"), mapping
  )
  geom_point(mapping = mapping, data = data, inherit.aes = inherit.aes, ...)
}

#' @rdname geom_link
#' @export
geom_start_label <- function(mapping = NULL,
                             data = NULL,
                             inherit.aes = getOption("ggcor.link.inherit.aes", TRUE),
                             ...)
{
  if(!is.null(data) && !inherits(data, "layout_link_tbl")) {
    stop("Need a layout_link_tbl.", call. = FALSE)
  }
  data <- if(is.null(data)) {
    get_start_nodes()
  } else {
    get_start_nodes()(data)
  }
  mapping <- aes_modify(
    aes_string(x = "x", y = "y", label = "start.label"), mapping
  )
  geom_text(mapping = mapping, data = data, inherit.aes = inherit.aes, ...)
}

#' @rdname geom_link
#' @export
geom_end_label <- function(mapping = NULL,
                           data = NULL,
                           inherit.aes = getOption("ggcor.link.inherit.aes", TRUE),
                           ...)
{
  if(!is.null(data) && !inherits(data, "layout_link_tbl")) {
    stop("Need a layout_link_tbl.", call. = FALSE)
  }
  data <- if(is.null(data)) {
    get_end_nodes()
  } else {
    get_end_nodes()(data)
  }
  mapping <- aes_modify(
    aes_string(x = "xend", y = "yend", label = "end.label"), mapping
  )
  geom_text(mapping = mapping, data = data, inherit.aes = inherit.aes, ...)
}

#' @rdname geom_link
#' @export
get_start_nodes <- function() {
  function(data) {
    stopifnot(inherits(data, "layout_link_tbl"))
    dplyr::filter(data, .start.filter)
  }
}

#' @rdname geom_link
#' @export
get_end_nodes <- function() {
  function(data) {
    stopifnot(inherits(data, "layout_link_tbl"))
    dplyr::filter(data, .end.filter)
  }
}

#' @importFrom utils modifyList
#' @noRd
aes_modify <- function(aes1, aes2) {
  aes <- modifyList(as.list(aes1), as.list(aes2))
  class(aes) <- "uneval"
  aes
}

