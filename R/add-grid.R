#' @importFrom ggplot2 geom_segment aes_string
#' @export
add_grid <- function(colour = "grey50", size = 0.25, ..., color = NULL) {
  if(!is.null(color)) colour <- color
  geom_segment(aes_string(x = "x", y = "y", xend = "xend", yend = "yend"),
               data = get_grid_data(), colour = colour, size = size,
               inherit.aes = FALSE, ...)
}
#' @noRd
get_grid_data <- function() {
  function(data) {
    if(!is_cor_tbl(data))
      stop("Need a cor_tbl.", call. = FALSE)
    n <- length(cor_tbl_xname(data))
    m <- length(cor_tbl_yname(data))
    type <- cor_tbl_type(data)
    show.diag <- cor_tbl_showdiag(data)
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

