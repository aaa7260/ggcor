#' Tidy co-occurrence network data
#' @description The function calculates correlation coefficient, statistical
#'     significance level and filters according to conditions.
#' @param x a cor_network object.
#' @param corr correlation matrix.
#' @param p.value significant matrix of correlation.
#' @param row.names,col.names row and column names of correlation matrix.
#' @param rm.dup logical (defaults to TRUE) indicating whether remove duplicate
#'     rows. If TRUE, the correlation between A-B and B-A is retained only A-B.
#' @param simplify logical value (defaults to TRUE) indicating whether to
#'     delete nodes without edge connections.
#' @param r.thres a numeric value.
#' @param r.absolute logical value (defaults to TRUE).
#' @param p.thres a numeric value.
#' @param val.type return value type:
#'   \itemize{
#'       \item \code{graph_tbl}: return graph_tbl object
#'       \item \code{igraph}: return igraph object
#'       \item \code{list}: return a list of nodes and edges
#'    }
#' @param ... extra params for printing.
#' @return tbl_graph object.
#' @importFrom dplyr filter %>%
#' @importFrom tibble tibble
#' @importFrom tidygraph tbl_graph
#' @importFrom igraph graph_from_data_frame
#' @rdname cor-network
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
cor_network <- function(corr,
                        p.value = NULL,
                        row.names = NULL,
                        col.names = NULL,
                        rm.dup = TRUE,
                        simplify = TRUE,
                        r.thres = 0.6,
                        r.absolute = TRUE,
                        p.thres = 0.05,
                        val.type = "graph_tbl")
{
  val.type <- match.arg(val.type, c("graph_tbl", "igraph", "list"))
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
  edges <- if(is.finite(r.thres)) {
    if(r.absolute) {
      if(is.null(p.value) || !is.finite(p.thres)) {
        dplyr::filter(edges, abs(r) > r.thres)
      } else {
        dplyr::filter(edges, abs(r) > r.thres, p.value < p.thres)
      }
    } else {
      if(is.null(p.value) || !is.finite(p.thres)) {
        dplyr::filter(edges, r > r.thres)
      } else {
        dplyr::filter(edges, r > r.thres, p.value < p.thres)
      }
    }
  } else {
    if(is.null(p.value) || !is.finite(p.thres)) {
      edges
    } else {
      dplyr::filter(edges, p.value < p.thres)
    }
  }
  nodes <- if(simplify) {
    tibble::tibble(name = unique(c(edges$.col.names, edges$.row.names)))
  } else {
    tibble::tibble(name = unique(c(.row.names, .col.names)))
  }

  switch (val.type,
          graph_tbl = tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = FALSE),
          igraph    = igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes),
          list      = structure(.Data = list(nodes = nodes, edges  = edges), class = "cor_network")
  )
}

#' @rdname cor-network
#' @export
print.cor_network <- function(x, ...)
{
  cat("A cor_network object:", "\n")
  cat("Nodes table: ")
  print(x$nodes, ...)
  cat("Edges table: ")
  print(x$edges, ...)
}
