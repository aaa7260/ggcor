#' Scale continuous with liner transform
#' @title Scale continuous
#' @param from input range.
#' @param to output range.
#' @param ... extra parameters.
#' @return liner trans.
#' @importFrom ggplot2 scale_x_continuous scale_x_continuous
#' @importFrom scales trans_new
#' @rdname liner_trans
#' @examples \dontrun{
#' library(ggplot2)
#' df <- data.frame(x = sample(letters[1:10], 200, replace = TRUE))
#' p <- ggplot(df, aes(x)) + geom_bar()
#' from <- c(0, max(table(df$x)))
#' p + scale_y_liner(from, c(0, 3)) + coord_fixed()
#' }
scale_x_liner <- function(from, to, ...) {
  scale_x_continuous(..., trans = liner_trans(from, to))
}
scale_y_liner <- function(from, to, ...) {
  scale_y_continuous(..., trans = liner_trans(from, to))
}
scale_x_liner_rev <- function(from, to, ...) {
  scale_x_continuous(..., trans = reverse_liner_trans(from, to))
}
scale_y_liner_rev <- function(from, to, ...) {
  scale_y_continuous(..., trans = reverse_liner_trans(from, to))
}

#' @noRd
liner_trans <- function (from, to)
{
  force(from)
  force(to)
  nm <- paste0("liner-trans:\nfrom ",
               paste0("(", format(from[1]), "->", format(from[2]), ")"),
               " to ",
               paste0("(", format(to[1]), "->", format(to[2]), ")"))
  trans <- function(x) scales::rescale(x, from = from, to = to)
  inv <- function(x) scales::rescale(x, from = to, to = from)
  scales::trans_new(nm,
                    transform = trans,
                    inverse = inv,
                    minor_breaks = scales::regular_minor_breaks())
}

#' @noRd
reverse_liner_trans <- function (from, to)
{
  force(from)
  force(to)
  nm <- paste0("reverse-liner-trans:\nfrom ",
               paste0("(", format(from[1]), "->", format(from[2]), ")"),
               " to ",
               paste0("(", format(to[1]), "->", format(to[2]), ")"))
  trans <- function(x) - scales::rescale(x, from = from, to = to)
  inv <- function(x) - scales::rescale(x, from = to, to = from)
  scales::trans_new(nm,
                    transform = trans,
                    inverse = inv,
                    minor_breaks = scales::regular_minor_breaks(reverse = TRUE))
}
