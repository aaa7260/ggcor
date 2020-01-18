#' @importFrom igraph as.igraph
#' @export
as.igraph.cor_network <- function(x, ...)
{
  igraph::graph_from_data_frame(x$edges, directed = FALSE,
                                vertices = x$nodes)
}

#' @importFrom igraph plot.igraph
#' @importFrom graphics plot
#' @method plot cor_network
#' @export
plot.cor_network <- function(x, ...)
{
  plot(as.igraph(x), ...)
}
