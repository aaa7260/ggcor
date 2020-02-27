#' Add panel grid line on correlation plot
#' @description \code{add_grid} is mainly used with \code{ggcor} function,
#'     and only valid if the input data is cor_tbl in \code{\link[ggplot2]{ggplot}}.
#' @param data NULL (default) or a cor_tbl object.
#' @param colour,color colour of grid lines.
#' @param size size of grid lines.
#' @param ... extra params for \code{\link[ggplot2]{geom_segment}}.
#' @importFrom ggplot2 geom_segment aes_string
#' @rdname geom_grid
#' @examples
#' df <- fortify_cor(mtcars)
#' ggcor(df) + geom_grid()
#' require(ggplot2, quietly = TRUE)
#' ggplot(df, aes(x, y)) + geom_grid()
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_grid <- function(data = NULL, colour = "grey50", size = 0.25, ..., color = NULL) {
  if(!is.null(data)) {
    if(!is_cor_tbl(data)) {
      stop("Need a cor_tbl object.", call. = FALSE)
    }
  }
  if(!is.null(color))
    colour <- color
  geom_segment(aes_string(x = "x", y = "y", xend = "xend", yend = "yend"),
               data = data %||% get_grid_data(), colour = colour, size = size,
               inherit.aes = FALSE, ...)
}

#' @rdname geom_diag_label
#' @export
add_grid <- function(...) {
  warning("`add_grid()` is deprecated. ",
          "Use `geom_grid()` instead.", call. = FALSE)
  geom_grid(...)
}

#' @noRd
get_grid_data <- function() {
  function(data) {
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
}

