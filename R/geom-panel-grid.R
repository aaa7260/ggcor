#' Add panel grid line on correlation plot
#' @description \code{geom_grid} is mainly used with \code{ggcor} or \code{quickcor}
#'     function to add a panel grid line on plot region.
#' @param colour,color colour of grid lines.
#' @param size size of grid lines.
#' @importFrom ggplot2 geom_segment aes_string
#' @rdname geom_panel_grid
#' @examples
#' df <- fortify_cor(mtcars)
#' ggcor(df) + geom_panel_grid()
#' require(ggplot2, quietly = TRUE)
#' ggplot(df, aes(x, y)) + geom_panel_grid()
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_panel_grid <- function(colour = "grey50",
                            size = 0.25,
                            color = NULL) {
  if(!is.null(color))
    colour <- color
  structure(.Data = list(colour = colour, size = size),
            class = "geom_panel_grid")
}

#' @rdname geom_panel_grid
#' @format NULL
#' @usage NULL
#' @export
add_grid <- function(...) {
  warning("`add_grid()` is deprecated. ",
          "Use `geom_panel_grid()` instead.", call. = FALSE)
  geom_panel_grid(...)
}

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

