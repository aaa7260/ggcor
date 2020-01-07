#' Add dianoal labels on correlation plot
#' @description \code{add_grid} is mainly used with \code{ggcor} and
#'     \code{quickcor} functions.
#' @param drop logical value (default is TRUE). When type of plot is 'upper'
#'     or 'lower' and 'show.diag' is FALSE, whether need to remove the blank label.
#' @param ... extra params for \code{\link[ggplot2]{geom_text}}.
#' @importFrom ggplot2 geom_text aes_string
#' @rdname add_diag_label
#' @examples
#' quickcor(mtcars, type = "upper") + geom_colour() + add_diag_label()
#' quickcor(mtcars, type = "lower") + geom_colour() + add_diag_label()
#' @seealso \code{\link[ggplot2]{geom_text}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
add_diag_label <- function(drop = FALSE, ...)
{
  geom_text(mapping = aes_string("x", "y", label = "label"),
            data = get_diag_label_data(drop = drop), inherit.aes = FALSE, ...)
}

#' @noRd
get_diag_label_data <- function(drop = FALSE) {
  function(data) {
    empty <- new_data_frame(list(x = numeric(0),
                                 y = numeric(0),
                                 label = character(0)))
    if(!is_cor_tbl(data)) {
      warning("Need a cor_tbl.", call. = FALSE)
      return(empty)
    }
    if(!is_symmet(data)) {
      warning("'add_diag_label' just supports for symmetrical correlation matrxi.", call. = FALSE)
      return(empty)
    }
    type <- get_type(data)
    show.diag <- get_show_diag(data)
    row.names <- rev(get_row_name(data))
    n <- length(row.names)
    y <- 1:n
    lab <- row.names
    if(type == "upper") {
      if(show.diag) {
        x <- n - y
      } else {
        x <- n - y + 1
        if(drop) {
          x <- x[2:n]
          y <- y[2:n]
          lab <- lab[2:n]
        }
      }
    } else if(type == "lower") {
      if(show.diag) {
        x <- n - y + 2
      } else {
        x <- n - y + 1
        if(drop) {
          x <- x[1:(n - 1)]
          y <- y[1:(n - 1)]
          lab <- lab[1:(n - 1)]
        }
      }
    } else {
      x <- n - y + 1
    }
    new_data_frame(list(x = x, y = y, label = lab))
  }
}
