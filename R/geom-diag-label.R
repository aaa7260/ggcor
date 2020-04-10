#' Add diagnoal labels on correlation plot
#' @description \code{geom_diag_label} is mainly used with \code{ggcor} and
#'     \code{quickcor} functions to add diagnoal labels on correct position
#'     base on different type of cor_tbl object.
#' @param geom one of "text", "label" or "image".
#' @param remove.axis if TRUE, will remove the axis.
#' @param ... extra parameters.
#' @importFrom ggplot2 aes_string
#' @rdname geom_diag_label
#' @examples
#' quickcor(mtcars, type = "upper") + geom_colour() + geom_diag_label()
#' quickcor(mtcars, type = "lower") + geom_colour() + geom_diag_label()
#' @seealso \code{\link[ggplot2]{geom_text}}, \code{\link[ggplot2]{geom_label}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_diag_label <- function(..., geom = "text", remove.axis = TRUE)
{
  geom <- match.arg(geom, c("text", "label", "image"))
  params <- list(...)
  structure(.Data = list(params = params, geom = geom, remove.axis = remove.axis),
            class = "geom_diag_label")
}

#' @rdname geom_diag_label
#' @format NULL
#' @usage NULL
#' @export
add_diag_label <- function(...) {
  warning("`add_diag_label()` is deprecated. ",
  "Use `geom_diag_label()` instead.", call. = FALSE)
  geom_diag_label(...)
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
