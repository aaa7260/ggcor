#' Unit square based on center
#' @description This is the encapsulation of \code{geom_tile()}
#'     function, and some fine-tuning has been done.
#' @param mapping set of aesthetic mappings created by aes() or aes_().
#' @param data the data to be displayed in this layer.
#' @param colour the border colour.
#' @param ... other params pass to \code{\link[ggplot2]{geom_tile}}.
#' @importFrom ggplot2 geom_tile
#' @rdname geom_colour
#' @examples
#' quickcor(mtcars) + geom_colour()
#' @seealso \code{\link[ggplot2]{geom_tile}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_colour <- function(mapping = NULL,
                        data = NULL,
                        colour = "grey60",
                        ...)
{
  geom_tile(mapping = mapping,
            data = data,
            colour = colour,
            ...)
}

#' @rdname geom_colour
#' @export
geom_color <- geom_colour
