#' @importFrom igraph as.igraph
#' @export
as.igraph.cor_network <- function(x, ...)
{
  igraph::graph_from_data_frame(x$edges, directed = FALSE,
                                vertices = x$nodes)
}

#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.cor_network <- function(x, ...)
{
  tidygraph::as_tbl_graph(x, directed = FALSE)
}

#' @importFrom igraph plot.igraph
#' @importFrom graphics plot
#' @method plot cor_network
#' @export
plot.cor_network <- function(x, ...)
{
  plot(as.igraph(x), ...)
}
