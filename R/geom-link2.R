#' Special layer function for correlation link plot
#' @description A set of custom layer functions that quickly add
#' layers of curves, nodes, and labels.
#' @param mapping aesthetic mappings parameters.
#' @param data NULL or a layout_link_tbl object that create by
#' \code{parallel_layout()} or \code{combination_layout()}.
#' @param  curvature a numeric value giving the amount of curvature.
#' @param ... extra parameters.
#' @return geom layer.
#' @importFrom ggplot2 aes_string geom_curve geom_point geom_text
#' @importFrom dplyr filter
#' @rdname geom-link2
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_link2 <- function(mapping = NULL,
                       data = NULL,
                       curvature = 0,
                       ...)
{
  mapping <- aes_intersect(
    mapping, aes_string(x = "x", y = "y", xend = "xend", yend = "yend")
  )
  geom_curve(mapping = mapping, data = data, curvature = curvature, ...)
}

#' @rdname geom-link2
#' @export
geom_link_point <- function(...)
{
  list(
    geom_start_point(...),
    geom_end_point(...)
  )
}

#' @rdname geom-link2
#' @export
geom_link_label <- function(...)
{
  list(
    geom_start_label(...),
    geom_end_label(...)
  )
}

#' @rdname geom-link2
#' @export
geom_start_point <- function(mapping = NULL,
                             data = NULL,
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
  mapping <- aes_intersect(
    mapping, aes_string(x = "x", y = "y")
  )
  geom_point(mapping = mapping, data = data, ...)
}

#' @rdname geom-link2
#' @export
geom_end_point <- function(mapping = NULL,
                           data = NULL,
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
  mapping <- aes_intersect(
    mapping, aes_string(x = "xend", y = "yend")
  )
  geom_point(mapping = mapping, data = data, ...)
}

#' @rdname geom-link2
#' @export
geom_start_label <- function(mapping = NULL,
                             data = NULL,
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
  mapping <- aes_intersect(
    mapping, aes_string(x = "x", y = "y", label = "start.label")
  )
  geom_text(mapping = mapping, data = data, ...)
}

#' @rdname geom-link2
#' @export
geom_end_label <- function(mapping = NULL,
                           data = NULL,
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
  mapping <- aes_intersect(
    mapping, aes_string(x = "xend", y = "yend", label = "end.label")
  )
  geom_text(mapping = mapping, data = data, ...)
}

#' @rdname geom-link2
#' @export
get_start_nodes <- function() {
  function(data) {
    stopifnot(inherits(data, "layout_link_tbl"))
    dplyr::filter(data, .start.filter)
  }
}

#' @rdname geom-link2
#' @export
get_end_nodes <- function() {
  function(data) {
    stopifnot(inherits(data, "layout_link_tbl"))
    dplyr::filter(data, .end.filter)
  }
}

#' @noRd
aes_intersect <- function (aes1, aes2)
{
  aes <- c(as.list(aes1), aes2[!names(aes2) %in% names(aes1)])
  class(aes) <- "uneval"
  aes
}
