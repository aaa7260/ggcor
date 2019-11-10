#' @noRd
get_diaglab_data <- function(drop = FALSE) {
  function(data) {
    if(!is_cor_tbl(data)) {
      warning("Need a cor_tbl.", call. = FALSE)
      return(data.frame(x = numeric(0), y = numeric(0), label = character(0)))
    }
    if(!is_symmet(data)) {
      warning("'add_diaglab()' just supports for symmetrical correlation matrxi.", call. = FALSE)
      return(data.frame(x = numeric(0), y = numeric(0), label = character(0)))
    }
    type <- cor_tbl_type(data)
    show.diag <- cor_tbl_showdiag(data)
    yname <- cor_tbl_yname(data)
    n <- length(yname)
    y <- 1:n
    lab <- yname
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
    dd <- data.frame(x = x, y = y, label = lab, stringsAsFactors = FALSE)
    dd
  }
}

#' @export
add_diaglab <- function(drop = FALSE, ...)
{
  geom_text(mapping = aes_string("x", "y", label = "label"),
            data = get_diaglab_data(drop = drop), inherit.aes = FALSE, ...)
}
