#' Convert a object to matrix
#' @description Functions to convert cor_tbl object to a list of matrix.
#' @param x any \code{R} object.
#' @param index character vector indicating which columns will be convert. If "all",
#' all columns will be convert.
#' @param missing If NULL (default), the missing value will be filled with NAs.
#' @param ... extra params.
#' @return a list of matrix.
#' @importFrom purrr walk walk2
#' @importFrom rlang set_names
#' @importFrom dplyr filter
#' @rdname as_matrix
#' @examples
#' cor(mtcars) %>% as_cor_tbl() %>% as_matrix()
#' cor(mtcars) %>% as_cor_tbl() %>% as_matrix("r")
#' fortify_cor(iris[-5],group = iris$Species, cor.test = TRUE) %>%
#'   as_matrix()
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
as_matrix <- function(x, ...) {
  UseMethod("as_matrix")
}

#' @rdname  as_matrix
#' @export
#' @method as_matrix cor_tbl
as_matrix.cor_tbl <- function(x,
                              index = "all",
                              missing = NULL,
                              ...) {
  if(length(index) == 1 && index == "all") {
    index <- setdiff(names(x), c(".row.names", ".col.names", ".row.id", ".col.id",
                                 ".group"))
  }
  row.name <- get_row_name(x)
  col.name <- get_col_name(x)
  if(is.null(missing)) {
    mat <- matrix(nrow = length(row.name), ncol = length(col.name),
                  dimnames = list(row.name, col.name))
  } else {
    mat <- matrix(missing, nrow = length(row.name), ncol = length(col.name),
                  dimnames = list(row.name, col.name))
  }

  mat <- rlang::set_names(rep_len(list(mat), length(index)), index)
  if(isTRUE(attr(x, "grouped"))) {
    group <- unique(x$.group)
    mat <- rlang::set_names(rep_len(list(mat), length(group)), group)
    purrr::walk(group, function(.grp) {
      purrr::walk(index, function(.index){
        purrr::walk2(x$.row.id, x$.col.id, function(.row, .col) {
          temp <- dplyr::filter(x, .row.id == .row, .col.id == .col, .group == .grp)
          mat[[.grp]][[.index]][length(row.name) - .row + 1, .col] <<- temp[[.index]]
        })
      })
    })
  } else {
    purrr::walk(index, function(.index){
      purrr::walk2(x$.row.id, x$.col.id, function(.row, .col) {
        temp <- dplyr::filter(x, .row.id == .row, .col.id == .col)
        mat[[.index]][length(row.name) - .row + 1, .col] <<- temp[[.index]]
      })
    })
  }
  return(mat)
}

#' @rdname  as_matrix
#' @export
#' @method as_matrix mantel_tbl
as_matrix.mantel_tbl <- function(x, ...) {
  as_matrix(as_cor_tbl(x), ...)
}

#' @rdname  as_matrix
#' @export
#' @method as_matrix pro_tbl
as_matrix.pro_tbl <- function(x, ...) {
  as_matrix(as_cor_tbl(x), ...)
}

