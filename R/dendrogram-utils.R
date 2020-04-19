### copy from ComplexHeatmap packages
### see https://github.com/jokergoo/ComplexHeatmap/blob/master/R/grid.dendrogram.R for details.
#' @noRd
dend_tbl <- function(dend) {
  if(is.null(attr(dend, "x"))) {
    dend <- adjust_dend(dend)
  }
  env <- as.environment(list(x = NULL, y = NULL, xend = NULL, yend = NULL,
                             col = NULL, lty = NULL, lwd = NULL))
  generate_children_dendrogram_segments <- function(dend, env = NULL) {
    if(is.leaf(dend)) {
      return(NULL)
    }
    height <- attr(dend, "height")
    nc <- length(dend)
    xl <- vapply(seq_len(nc), function(i) attr(dend[[i]], "x"), numeric(1))
    yl <- vapply(seq_len(nc), function(i) attr(dend[[i]], "height"), numeric(1))
    max_x <- max(xl)
    min_x <- min(xl)
    mid_x <- (max_x + min_x) * 0.5
    # graphic parameters for current branch
    edge_gp_list <- lapply(seq_len(nc), function(i) as.list(attr(dend[[i]], "edgePar")))
    for(i in c(setdiff(seq_len(nc), c(1, nc)), c(1, nc))) {
      for(gp_name in c("col", "lwd", "lty")) {
        # gp for two segments
        if(is.null(edge_gp_list[[i]][[gp_name]])) {
          gpa <- rep(default_gpar(gp_name), 2)
        } else {
          gpa <- rep(edge_gp_list[[i]][[gp_name]], 2)
        }
        env[[gp_name]] <- c(env[[gp_name]], gpa)
      }
      env$x <- c(env$x, xl[i], xl[i])
      env$xend <- c(env$xend, xl[i], mid_x)
      env$y <- c(env$y, yl[i], height)
      env$yend <- c(env$yend, height, height)
      generate_children_dendrogram_segments(dend[[i]], env)
    }
  }
  generate_children_dendrogram_segments(dend, env)
  new_data_frame(as.list(env)) %>%
    rename(colour = col, size = lwd, linetype = lty)
}

#' @noRd
subset_dendrogram <- function(x, ind) {
  if(is.null(ind)) x else x[[ind]]
}

#' @noRd
default_gpar <- function(name) {
  switch (name,
          col = "black",
          lwd = 0.5,
          lty = "solid"
  )
}

#' @noRd
adjust_dend <- function(dend) {
  n <- nobs(dend)
  leaves_pos <- 1:n
  dend_order <- order.dendrogram(dend)
  od2index <- NULL
  od2index[dend_order] <- 1:n
  env <- as.environment(list(dend = dend))
  adj_dend <- function(ind = NULL) {
    d <- subset_dendrogram(env$dend, ind)
    n_node <- length(d)
    if(is.leaf(d)) {
      i <- od2index[ d[][[1]] ]
      x <- leaves_pos[i]
    } else {
      nc <- length(d)
      for(i in seq_len(nc)) {
        adj_dend(c(ind, i))
      }
      d <- subset_dendrogram(env$dend, ind)

      xl <- vapply(1:nc, function(i) attr(d[[i]], "x"), numeric(1))
      x <- (max(xl) + min(xl))*0.5
    }
    if(is.null(ind)) {
      attr(env$dend, "x") <- x
    } else {
      attr(env$dend[[ind]], "x") <- x
    }
    x
  }
  adj_dend()
  dend <- env$dend
  return(dend)
}
