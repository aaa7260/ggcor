#' Reorder cor_tbl
#' @description Reorder cor_tbl object by name, hclut or or other method.
#' @param x a cor_tbl object
#' @param row.order,col.order order of the correlation matrix, and it can be
#' NULL (default), numeric vector, character vector, hclust or dendrogram
#' object.
#' @param x numeric vector, character vector, hclust object or dendrogram object.
#' @param ... parameters.
#' @return a modified cor_tbl object.
#' @rdname reorder_cor_tbl
#' @examples 
#' fortify_cor(mtcars) %>% 
#'   reorder_cor_tbl(sample(1:11)) %>% 
#'   quickcor() + geom_square()
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
reorder_cor_tbl <- function(x, 
                            row.order = NULL, 
                            col.order = if(is_symmet(x)) row.order else NULL)
{
  if((is.null(row.order) && is.null(col.order)) || (nrow(x) == 0)) {
    return(x)
  }
  
  is.symmet <- is_symmet(x)
  if(is.symmet) {
    if(!identical(row.order, col.order)) {
      warning("Different row/column order for symmetric matrix.", call. = FALSE)
    }
    row.order <- row.order %||% col.order
    col.order <- row.order
  }
  
  if(!is.null(row.order)) {
    row.order <- get_order(row.order)
    row.names <- if(is.character(row.order)) row.order else get_row_name(x)[row.order]
    if(length(row.order) != length(row.names)) {
      stop("Length of 'row.order' must be same as rows.")
    }
    
    attr(x, ".row.names") <- row.names
    x$.row.id <- as.integer(factor(x$.row.names, levels = rev(row.names)))
  }
  if(!is.null(col.order)) {
    col.order <- get_order(col.order)
    col.names <- if(is.character(col.order)) col.order else get_col_name(x)[col.order]
    if(length(col.order) != length(col.names)) {
      stop("Length of 'col.order' must be same as rows.")
    }
    attr(x, ".col.names") <- col.names
    x$.col.id <- as.integer(factor(x$.col.names, levels = col.names))
  }
  x
}

#' @rdname reorder_cor_tbl
#' @export
get_order <- function(x, ...) {
  UseMethod("get_order")
}

#' @rdname reorder_cor_tbl
#' @export
#' @method get_order numeric
get_order.numeric <- function(x, ...) {
  x
}

#' @rdname reorder_cor_tbl
#' @export
#' @method get_order character
get_order.character <- function(x, ...) {
  x
}

#' @rdname reorder_cor_tbl
#' @export
#' @method get_order hclust
get_order.hclust <- function(x, ...) {
  x$order
}

#' @rdname reorder_cor_tbl
#' @export
#' @method get_order dendrogram
get_order.dendrogram <- function(x, ...) {
  get_order(stats::as.hclust(x))
}
