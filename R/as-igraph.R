#' Corece to a igraph object
#' @description Functions to coerce a object to igraph if possible.
#' @param x \code{R} object.
#' @param ... extra params.
#' @return igraph object.
#' @importFrom igraph graph_from_data_frame as.igraph
#' @rdname as_igraph
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
as.igraph.cor_tbl <- function(x, ...)
{
  x <- as_cor_network(x, ...)
  igraph::graph_from_data_frame(x$edges, directed = FALSE, 
                                vertices = x$nodes)
}

#' @rdname  as_igraph
#' @export
as.igraph.mantel_tbl <- function(x, ...)
{
  as.igraph(as_cor_tbl(x), ...)
}

#' @rdname  as_igraph
#' @importFrom tidygraph tbl_graph
#' @export
as.igraph.rcorr <- function(x, ...)
{
  p.value <- x$P
  diag(p.value) <- 0
  cor_network(x$r, p.value, ..., val.type = "igraph")
}

#' @rdname  as_igraph
#' @export
as.igraph.corr.test <- function(x, ...)
{
  cor_network(x$r, x$p, ..., val.type = "igraph")
}

#' @rdname  as_igraph
#' @export
as.igraph.correlation <- function(x, ...)
{
  cor_network(x$r, x$p.value, ..., val.type = "igraph")
}

