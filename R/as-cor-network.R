#' Tidy co-occurrence network data
#' @description The function calculates correlation coefficient, statistical
#'     significance level and filters according to conditions.
#' @param x \code{R} object.
#' @param active "nodes" (defaults) or "edges", indicating whether to
#'     handle edges or nodes.
#' @param corr correlation matrix.
#' @param p.value significant matrix of correlation.
#' @param simplify logical value (defaults to TRUE) indicating whether to
#'     delete nodes without edge connections.
#' @param r.thres a numeric value.
#' @param r.absolute logical value (defaults to TRUE).
#' @param p.thres a numeric value.
#' @param ... passing to \code{\link[ggcor]{fortify_cor}}
#' @return a list of nodes and edges.
#' @importFrom dplyr filter
#' @rdname as_cor_network
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
as_cor_network <- function(x, ...) {
  UseMethod("as_cor_network")
}

#' @importFrom dplyr filter %>%
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
#' @method as_cor_network mantel_tbl
#' @export
as_cor_network.mantel_tbl <- function(x, ...)
{
  cor_network(as_cor_tbl(x), ...)
}
#' @rdname  as_cor_network
#' @method as_cor_network rcorr
#' @export
as_cor_network.rcorr <- function(x, ...)
{
  p.value <- x$P
  diag(p.value) <- 0
  cor_network(x$r, p.value, ...)
}

#' @rdname  as_cor_network
#' @method as_cor_network corr.test
#' @export
as_cor_network.corr.test <- function(x, ...)
{
  cor_network(x$r, x$p, ...)
}

#' @rdname  as_cor_network
#' @method as_cor_network correlation
#' @export
as_cor_network.correlation <- function(x, ...)
{
  cor_network(x$r, x$p.value, ...)
}

#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @rdname as_cor_network
#' @export
cor_network <- function(corr,
                        p.value = NULL,
                        simplify = TRUE,
                        r.thres = 0.6,
                        r.absolute = TRUE,
                        p.thres = 0.05)
{
  if(!is.matrix(corr))
    corr <- as.matrix(corr)
  if(!is.null(p.value) && !is.matrix(p.value))
    p.value <- as.matrix(p.value)
  .row.names <- rownames(corr) %||% paste0("row", 1:nrow(corr))
  .col.names <- colnames(corr) %||% paste0("col", 1:ncol(corr))
  is.symmet <- length(.row.names) == length(.col.names) && all(.row.names == .col.names)

  edges <- tibble::tibble(.row.names = rep(.row.names, ncol(corr)),
                          .col.names = rep(.col.names, each = nrow(corr)),
                          r = as.vector(corr))
  if(!is.null(p.value))
    edges$p.value <- as.vector(p.value)
  if(is.symmet) {
    edges <- subset(edges, upper.tri(corr))
  }
  edges <- if(r.absolute) {
    if(is.null(p.value)) {
      with(edges, dplyr::filter(edges, abs(r) > r.thres))
    } else {
      with(edges, dplyr::filter(edges, abs(r) > r.thres, p.value < p.thres))
    }
  } else {
    if(is.null(p.value)) {
      with(edges, dplyr::filter(edges, r > r.thres))
    } else {
      with(edges, dplyr::filter(edges, r > r.thres, p.value < p.thres))
    }
  }
  node.name <- if(simplify) {
    unique(c(edges$.col.names, edges$.row.names))
  } else {
    unique(c(.row.names, .col.names))
  }
  structure(.Data = list(nodes = tibble::tibble(name = node.name),
                         edges = edges),
            active = "nodes",
            class = "cor_network")
}

#' @rdname as_cor_network
#' @export
is_cor_network <- function(x)
{
  inherits(x, "cor_network")
}

#' @rdname as_cor_network
#' @export
cn_active <- function(x, active = "nodes")
{
  stopifnot(is_cor_network(x))
  attr(x, "active") <- active
  x
}
