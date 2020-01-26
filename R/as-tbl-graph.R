#' Tidy co-occurrence network data
#' @description The function calculates correlation coefficient, statistical
#'     significance level and filters according to conditions.
#' @param x \code{R} object.
#' @param corr correlation matrix.
#' @param p.value significant matrix of correlation.
#' @param row.names,col.names row and column names of correlation matrix.
#' @param rm.dumplicate logical (defaults to TRUE) indicating whether remove duplicate
#'     rows. If TRUE, the correlation between A-B and B-A is retained only A-B.
#' @param simplify logical value (defaults to TRUE) indicating whether to
#'     delete nodes without edge connections.
#' @param r.thres a numeric value.
#' @param r.absolute logical value (defaults to TRUE).
#' @param p.thres a numeric value.
#' @param ... passing to \code{\link[ggcor]{fortify_cor}}
#' @return tbl_graph object.
#' @importFrom dplyr filter %>%
#' @importFrom tibble tibble
#' @importFrom tidygraph tbl_graph
#' @rdname as_tbl_graph
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
as_tbl_graph.cor_tbl <- function(x,
                                 simplify = TRUE,
                                 r.thres = 0.6,
                                 r.absolute = TRUE,
                                 p.thres = 0.05,
                                 ...)
{
  if("p.value" %in% names(x)) {
    edges <- if(r.absolute) {
      dplyr::filter(x, abs(r) > r.thres, p.value < p.thres)
    } else {
      dplyr::filter(x, r > r.thres, p.value < p.thres)
    }
  } else {
    edges <- if(r.absolute) {
      dplyr::filter(x, abs(r) > r.thres)
    } else {
      dplyr::filter(x, r > r.thres, p.value < p.thres)
    }
  }
  node.name <- if(simplify) {
    unique(c(x$.col.names, x$.row.names))
  } else {
    unique(c(get_col_name(x), get_row_name(x)))
  }
  tidygraph::tbl_graph(nodes = tibble::tibble(name = node.name),
                       edges = edges, directed = FALSE)
}

#' @rdname  as_tbl_graph
#' @importFrom tidygraph tbl_graph
#' @export
as_tbl_graph.mantel_tbl <- function(x, ...)
{
  cor_network(as_cor_tbl(x), ...)
}
#' @rdname  as_tbl_graph
#' @importFrom tidygraph tbl_graph
#' @export
as_tbl_graph.rcorr <- function(x, ...)
{
  p.value <- x$P
  diag(p.value) <- 0
  cor_network(x$r, p.value, ...)
}

#' @rdname  as_tbl_graph
#' @importFrom tidygraph tbl_graph
#' @export
as_tbl_graph.corr.test <- function(x, ...)
{
  cor_network(x$r, x$p, ...)
}

#' @rdname  as_tbl_graph
#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.correlation <- function(x, ...)
{
  cor_network(x$r, x$p.value, ...)
}

#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom tidygraph tbl_graph
#' @rdname as_tbl_graph
#' @export
cor_network <- function(corr,
                        p.value = NULL,
                        row.names = NULL,
                        col.names = NULL,
                        rm.dup = TRUE,
                        simplify = TRUE,
                        r.thres = 0.6,
                        r.absolute = TRUE,
                        p.thres = 0.05)
{
  if(!is.matrix(corr))
    corr <- as.matrix(corr)
  if(!is.null(p.value) && !is.matrix(p.value))
    p.value <- as.matrix(p.value)
  .row.names <- row.names %||% rownames(corr) %||% paste0("row", 1:nrow(corr))
  .col.names <- col.names %||% colnames(corr) %||% paste0("col", 1:ncol(corr))
  is.symmet <- length(.row.names) == length(.col.names) && all(.row.names == .col.names)

  edges <- tibble::tibble(.row.names = rep(.row.names, ncol(corr)),
                          .col.names = rep(.col.names, each = nrow(corr)),
                          r = as.vector(corr))
  if(!is.null(p.value))
    edges$p.value <- as.vector(p.value)
  if(is.symmet && rm.dup) {
    edges <- dplyr::filter(edges, lower.tri(corr))
  }
  edges <- if(r.absolute) {
    if(is.null(p.value)) {
      dplyr::filter(edges, abs(r) > r.thres)
    } else {
      dplyr::filter(edges, abs(r) > r.thres, p.value < p.thres)
    }
  } else {
    if(is.null(p.value)) {
      dplyr::filter(edges, r > r.thres)
    } else {
      dplyr::filter(edges, r > r.thres, p.value < p.thres)
    }
  }
  node.name <- if(simplify) {
    unique(c(edges$.col.names, edges$.row.names))
  } else {
    unique(c(.row.names, .col.names))
  }
  tidygraph::tbl_graph(nodes = tibble::tibble(name = node.name),
                       edges = edges, directed = FALSE)
}
