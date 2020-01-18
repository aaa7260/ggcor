#' Network statistics function
#' @description All network statistical functions are the encapsulation
#'     of corresponding functions in the igraph package. These function
#'     names start with "cn_" in ggcor, and the rest are corresponding
#'     function names in igraph.
#' @param cn cor_network object.
#' @param ... extra params.
#' @rdname network_stats
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @rdname network_stats
#' @export
cn_degree <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::degree(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_degree_distribution <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::degree_distribution(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_diameter <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::diameter(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_distance_table <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::distance_table(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_mean_distance <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::mean_distance(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_distances <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::distances(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_shortest_paths <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::shortest_paths(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_all_shortest_paths <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::all_shortest_paths(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_estimate_betweenness <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::estimate_betweenness(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_betweenness <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::betweenness(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_edge_betweenness <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::edge_betweenness(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_edge_density <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::edge_density(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_edge_connectivity <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::edge_connectivity(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_ego_size <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::ego_size(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_ego <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::ego(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_make_ego_graph <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::make_ego_graph(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_eccentricity <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::eccentricity(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_ecount <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::ecount(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_ecount <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::ecount(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_eigen_centrality <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::eigen_centrality(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_embed_adjacency_matrix <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::embed_adjacency_matrix(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_embed_laplacian_matrix <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::embed_laplacian_matrix(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_closeness <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::closeness(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_estimate_closeness <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::estimate_closeness(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_transitivity <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::transitivity(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_centr_betw <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::centr_betw(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_centr_betw_tmax <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::centr_betw_tmax(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_centr_clo <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::centr_clo(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_centr_clo_tmax <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::centr_clo_tmax(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_centr_degree <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::centr_degree(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_centr_degree_tmax <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::centr_degree_tmax(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_centr_eigen <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::centr_eigen(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_centr_eigen_tmax <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::centr_eigen_tmax(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_component_distribution <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::component_distribution(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_components <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::components(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_cluster_edge_betweenness <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::cluster_edge_betweenness(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_cluster_fast_greedy <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::cluster_fast_greedy(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_cluster_infomap <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::cluster_infomap(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_cluster_label_prop <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::cluster_label_prop(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_cluster_leading_eigen <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::cluster_leading_eigen(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_cluster_louvain <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::cluster_louvain(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_cluster_optimal <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::cluster_optimal(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_cluster_spinglass <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::cluster_spinglass(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_cluster_walktrap <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::cluster_walktrap(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_vertex_connectivity <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::vertex_connectivity(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_cohesion <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::cohesion(as.igraph(cn), ...)
}

#' @rdname network_stats
#' @export
cn_coreness <- function(cn, ...)
{
  stopifnot(is_cor_network(cn))
  igraph::coreness(as.igraph(cn), ...)
}

