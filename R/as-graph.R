#' @importFrom igraph as.igraph
#' @export
as.igraph.cor_network <- function(x, ...)
{
  igraph::graph_from_data_frame(x$edges, directed = FALSE,
                                vertices = x$nodes)
}

#' @importFrom igraph plot.igraph
#' @export
plot.cor_network <- function(x, ...)
{
  x <- as.igraph(x)
  plot.igraph(x, ...)
}
