#' Corece to a graph_tbl object
#' @description Functions to coerce a object to graph_tbl if possible.
#' @param x \code{R} object.
#' @param ... extra params.
#' @return tbl_graph object.
#' @importFrom tidygraph tbl_graph as_tbl_graph
#' @rdname as_tbl_graph
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
as_tbl_graph.cor_tbl <- function(x, ...)
{
  x <- as_cor_network(x, ...)
  tidygraph::tbl_graph(nodes = x$nodes,
                       edges = x$edges, directed = FALSE)
}


#' @rdname  as_tbl_graph
#' @export
as_tbl_graph.mantel_tbl <- function(x, ...)
{
  as_tbl_graph(as_cor_tbl(x), ...)
}

#' @rdname  as_tbl_graph
#' @export
as_tbl_graph.rcorr <- function(x, ...)
{
  p.value <- x$P
  diag(p.value) <- 0
  cor_network(x$r, p.value, ..., val.type = "tbl_graph")
}

#' @rdname  as_tbl_graph
#' @export
as_tbl_graph.corr.test <- function(x, ...)
{
  cor_network(x$r, x$p, ..., val.type = "tbl_graph")
}

#' @rdname  as_tbl_graph
#' @export
as_tbl_graph.correlate <- function(x, ...)
{
  cor_network(x$r, x$p.value, ..., val.type = "tbl_graph")
}

#' @importFrom tidygraph tbl_graph
#' @rdname  as_tbl_graph
#' @export
as_tbl_graph.cor_network <- function(x, ...)
{
  tidygraph::tbl_graph(nodes = x$nodes, edges = x$edges, directed = FALSE)
}
