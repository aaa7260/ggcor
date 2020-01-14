#' @importFrom igraph as.igraph
#' @export
as.igraph.cor_tbl <- function(x, simplify = TRUE, ...)
{
  name <- if(simplify) {
    unique(c(x$.col.names, x$.row.names))
  } else {
    unique(c(get_col_name(x), get_row_name(x)))
  }
  igraph::graph_from_data_frame(x, directed = FALSE,
                                vertices = new_data_frame(list(name = name)))
}

#' @importFrom igraph as.igraph
#' @export
as.igraph.co_network <- function(x, simplify = TRUE, ...)
{
  name <- if(simplify) {
    unique(c(x$.col.names, x$.row.names))
  } else {
    unique(c(attr(x, ".col.names"), attr(x, ".row.names")))
  }
  igraph::graph_from_data_frame(x, directed = FALSE,
                                vertices = new_data_frame(list(name = name)))
}

#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.cor_tbl <- function(x, simplify = TRUE, ...)
{
  g <- as.igraph(x, simplify = simplify)
  as_tbl_graph(g, ...)
}

#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.co_network <- function(x, simplify = TRUE, ...)
{
  g <- as.igraph(x, simplify = simplify)
  as_tbl_graph(g, ...)
}

#' @export
igraph::as.igraph

#' @export
tidygraph::as_tbl_graph
