### modified from ComplexHeatmap packages
### see https://github.com/jokergoo/ComplexHeatmap/blob/master/R/grid.dendrogram.R for details.
#' @importFrom stats as.dendrogram as.hclust is.leaf nobs order.dendrogram
#' @noRd
dend_tbl <- function(dend, bcols, horiz, height.range, circular) {
  if(!inherits(dend, "dendrogram")) {
    dend <- stats::as.dendrogram(dend)
  }
  if(is.null(attr(dend, "x"))) {
    dend <- adjust_dend(dend)
  }
  if(!is.null(bcols)) {
    branches_color <- get_function("dendextend", "branches_color")
    bcols <- unique(bcols)
    dend <- branches_color(dend, k = length(bcols), col = bcols)
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
  col <- lwd <- lty <- NULL
  data <- new_data_frame(as.list(env)) %>%
    dplyr::rename(colour = col, size = lwd, linetype = lty)
  adjust_dend_tbl(data, horiz, height.range, circular)
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

#' @noRd
adjust_dend_tbl <- function(dend_tbl, horiz, height.range, circular) {
  aes <- setdiff(names(dend_tbl), c("x", "y", "xend", "yend"))
  maxheight <- max(dend_tbl$y, dend_tbl$yend, na.rm = TRUE)
  dend_tbl$y <- scales::rescale(dend_tbl$y, height.range, c(0, maxheight))
  dend_tbl$yend <- scales::rescale(dend_tbl$yend, height.range, c(0, maxheight))
  if(isTRUE(horiz)) {
    dend_tbl <- with(dend_tbl, {
      max.x <- max(x, xend, na.rm = TRUE)
      tempx <- max.x - x + 1
      tempxend <- max.x - xend + 1
      pos <- tibble::tibble(x = y, y = tempx, xend = yend, yend = tempxend)
      if(length(aes) > 0) dplyr::bind_cols(pos, dend_tbl[aes]) else pos
    })
    if(isTRUE(circular)) {
      dend_tbl <- with(dend_tbl, {
        min.x <- min(x, xend, na.rm = TRUE)
        pos <- tibble::tibble(x = min.x - x + 0.5, y = y, xend = min.x - xend + 0.5, yend = yend)
        if(length(aes) > 0) dplyr::bind_cols(pos, dend_tbl[aes]) else pos
      })
    } else {
      if(isTRUE(circular)) {
        max.x <- max(dend_tbl$x, dend_tbl$xend, na.rm = TRUE)
        dend_tbl$x <- max.x - dend_tbl$x + 1
        dend_tbl$xend <- max.x - dend_tbl$xend + 1
      }
    }
  }
  dend_tbl
}
