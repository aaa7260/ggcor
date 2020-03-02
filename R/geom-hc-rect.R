#' Draw square mark on correlation matrix plot
#'
#' @description Draw the cluster square mark on the correlation matrix plot.
#' @param data a correlation matrix or hc_rect_df object.
#' @param fill NA (default) or the fill colour of square.
#' @param colour,color the colour of square boder.
#' @param size size of square boder line.
#' @importFrom ggplot2 geom_rect geom_blank aes_string
#' @rdname geom_hc_rect
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_hc_rect <- function(data = NULL,
                         fill = NA,
                         colour = "black",
                         size = 2,
                         color = NULL)
{
  if(!is.null(data) && !inherits(data, "hc_rect_df")) {
    stop("Invalid data input.", call. = FALSE)
  }
  if(!is.null(color))
    colour <- color
  geom_rect(mapping = aes_string(xmin = "xmin", ymin = "ymin",
                                 xmax = "xmax", ymax = "ymax"),
            data = if(is.null(data)) get_hc_rect_df() else data,
            colour = colour, size = size, fill = fill, inherit.aes = FALSE)
}

#' @rdname geom_hc_rect
#' @export
get_hc_rect_df <- function() {
  function(data) {
    stopifnot(is_cor_tbl(data))
    attr(data, "hc.rect.df")
  }
}
