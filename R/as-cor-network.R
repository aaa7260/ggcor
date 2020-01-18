#' Tidy co-occurrence network data
#' @description The function calculates correlation coefficient, statistical
#'     significance level and filters according to conditions.
#' @param x any \code{R} object can be converted to cor_tbl.
#' @param simplify logical value (defaults to TRUE) indicating whether to
#'     delete nodes without edge connections.
#' @param r.thres a numeric value.
#' @param r.absolute logical value (defaults to TRUE).
#' @param p.thres a numeric value.
#' @param type a string, "full" (default), "upper" or "lower", display full,
#'     lower triangular or upper triangular matrix.
#' @param ... passing to \code{\link[ggcor]{fortify_cor}}
#' @return a list of nodes and edges.
#' @importFrom dplyr filter
#' @rdname as_cor_network
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
as_cor_network <- function(x, ...) {
  UseMethod("as_cor_network")
}

#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @rdname  as_cor_network
#' @method as_cor_network cor_tbl
#' @export
as_cor_network.cor_tbl <- function(x,
                                   simplify = TRUE,
                                   r.thres = 0.6,
                                   r.absolute = TRUE,
                                   p.thres = 0.05,
                                   ...)
{
  if("p.value" %in% names(x)) {
    edges <- if(r.absolute) {
      with(x, dplyr::filter(x, abs(r) > r.thres, p.value < p.thres))
    } else {
      with(x, dplyr::filter(x, r > r.thres, p.value < p.thres))
    }
  } else {
    edges <- if(r.absolute) {
      with(x, dplyr::filter(x, abs(r) > r.thres))
    } else {
      with(x, dplyr::filter(x, r > r.thres, p.value < p.thres))
    }
  }
  node.name <- if(simplify) {
    unique(c(x$.col.names, x$.row.names))
  } else {
    unique(c(get_col_name(x), get_row_name(x)))
  }
  structure(.Data = list(nodes = tibble::tibble(name = node.name),
                         edges = edges),
            active = "nodes",
            class = "cor_network")
}

#' @rdname  as_cor_network
#' @method as_cor_network default
#' @export
as_cor_network.default <- function(x,
                                   simplify = TRUE,
                                   r.thres = 0.6,
                                   r.absolute = TRUE,
                                   p.thres = 0.05,
                                   type = "upper",
                                   ...)
{
  as_cor_network(fortify_cor(x, type = type, ...), simplify = simplify, r.thres = r.thres,
                 r.absolute = r.absolute, p.thres = p.thres)
}
