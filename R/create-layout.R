#' Transform data base on different layout
#' @description These layout functions are not layout in the network diagram,
#' it just converts the original data into a form that makes it easy to draw
#' a curve graph.
#' @param data a data frame.
#' @param start.var,end.var character to specify which variable is the starting
#' points and which is the ending points. if the variable is not character, it
#' is forced to be converted.
#' @param stretch logical, indicating whether the heights/width of start points and
#' end points are consistent.
#' @param type the type (""upper" or "lower") of the correlation matrix plot.
#' @param show.diag a logical value indicating whether keep the diagonal.
#' @param row.names,col.names row/column names of correlation matrix.
#' @param cor_tbl a col_tbl object.
#' @return a data frame.
#' @importFrom rlang enquo eval_tidy set_names quo_is_null
#' @importFrom dplyr filter
#' @rdname create_layout
#' @examples
#' cor_tbl(cor(mtcars)) %>%
#'   parallel_layout()
#' \dontrun{
#' data("varespec", package = "vegan")
#' data("varechem", package = "vegan")
#' mantel_test(varespec, varechem) %>%
#'   combination_layout(type = "upper", col.names = colnames(varechem),
#'                      show.diag = FALSE)
#' }
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
parallel_layout <- function(data,
                            type = NULL,
                            show.diag = NULL,
                            row.names = NULL,
                            col.names = NULL,
                            start.var = NULL,
                            end.var = NULL,
                            stretch = FALSE,
                            cor_tbl)
{
  if(!is.data.frame(data))
    data <- as.data.frame(data)
  no.cor.tbl <- missing(cor_tbl)
  if(!no.cor.tbl) {
    if(!is_cor_tbl(cor_tbl))
      stop("Need a cor_tbl.", call. = FALSE)
  }
  row.names <- if(no.cor.tbl) rev(row.names) else rev(get_row_name(cor_tbl))
  col.names <- if(no.cor.tbl) col.names else get_col_name(cor_tbl)
  type <- if(no.cor.tbl) type else get_type(cor_tbl)
  show.diag <- if(no.cor.tbl) show.diag else get_show_diag(cor_tbl)
  start <- if(quo_is_null(enquo(start.var))) {
    data[[1]]
  } else {
    eval_tidy(enquo(start.var), data)
  }
  end <- if(quo_is_null(enquo(end.var))) {
    data[[2]]
  } else {
    eval_tidy(enquo(end.var), data)
  }
  if(!is.character(start))
    start <- as.character(start)
  if(!is.character(end))
    end <- as.character(end)
  spec.name <- unique(start[!is.na(start)])
  n <- length(row.names)
  m <- length(spec.name)
  x1 <- max(length(col.names), n) * 0.45 + length(col.names)
  x2 <- length(col.names) + 1

  if(m == 1) stretch <- FALSE
  start.pos <- if(m < n && !stretch) {
    set_names(seq(1, n, length.out = m + 2)[-c(1, m + 2)], spec.name)
  } else {
    set_names(seq(1, n, length.out = m), spec.name)
  }

  end.pos <- set_names(seq(1, n, length.out = n), row.names)

  edge.pos <- tibble(x = x1, y = start.pos[start], xend = x2, yend = end.pos[end])
  node.pos <- tibble(x = rep(c(x1, x2), c(m, n)),
                     y = c(start.pos[spec.name], end.pos[row.names]),
                     label = c(spec.name, row.names),
                     is.start = rep(c(TRUE, FALSE), c(m, n)))

  structure(.Data = dplyr::bind_cols(edge.pos, data), node.pos = node.pos,
            class = c("parallel_layout_tbl", "layout_tbl", class(edge.pos)))
}

#' @rdname create_layout
#' @export
triangle_layout <- function(data,
                            type = NULL,
                            show.diag = NULL,
                            row.names = NULL,
                            col.names = NULL,
                            start.var = NULL,
                            end.var = NULL,
                            cor_tbl)
{
  no.cor.tbl <- missing(cor_tbl)
  if(!no.cor.tbl) {
    if(!is_cor_tbl(cor_tbl) || !is_symmet(cor_tbl))
      stop("Need a symmetric cor_tbl.", call. = FALSE)
  }
  row.names <- if(no.cor.tbl) {
    rev(row.names) %||% col.names
  } else {
    rev(get_row_name(cor_tbl))
  }
  type <- if(no.cor.tbl) type else get_type(cor_tbl)
  show.diag <- if(no.cor.tbl) show.diag else get_show_diag(cor_tbl)
  start <- if(quo_is_null(enquo(start.var))) {
    data[[1]]
  } else {
    eval_tidy(enquo(start.var), data)
  }
  end <- if(quo_is_null(enquo(end.var))) {
    data[[2]]
  } else {
    eval_tidy(enquo(end.var), data)
  }
  if(!is.character(start))
    start <- as.character(start)
  if(!is.character(end))
    end <- as.character(end)

  spec.name <- unique(start[!is.na(start)])
  n <- length(row.names)
  m <- length(spec.name)
  ## get position of spec point
  if(type == "full") {
    stop("The 'type' of cor_tbl is not supported.", call. = FALSE)
  }

  if(type == "upper") {
    if(m == 1) {
      x <- 0.5 + 0.18 * n
      y <- 0.5 + 0.3 * n
    } else if(m == 2) {
      x <- c(0.5 - 0.02 * n, 0.5 + 0.2 * n)
      y <- c(0.5 + 0.46 * n, 0.5 + 0.2 * n)
    } else {
      y <- seq(0.5 + n * (1 - 0.3), 0.5 + n * 0.1, length.out = m)
      x <- seq(0.5 - 0.25 * n, 0.5 + 0.3 * n, length.out = m)
    }
  } else if(type == "lower") {
    if(m == 1) {
      x <- 0.5 + 0.82 * n
      y <- 0.5 + 0.7 * n
    } else if(m == 2) {
      x <- c(0.5 + 0.8 * n, 0.5 + 1.02 * n)
      y <- c(0.5 + 0.8 * n, 0.5 + 0.54 * n)
    } else {
      y <- seq(0.5 + n * (1 - 0.1), 0.5 + n * 0.3, length.out = m)
      x <- seq(0.5 + 0.75 * n, 0.5 + 1.3 * n, length.out = m)
    }
  }
  x <- set_names(x, spec.name)
  y <- set_names(y, spec.name)

  ## get position of env point
  xend <- n:1
  yend <- 1:n
  if(type == "upper") {
    if(show.diag) {
      xend <- xend - 1
    }
  } else {
    if(show.diag) {
      xend <- xend + 1
    }
  }
  xend <- set_names(xend, row.names)
  yend <- set_names(yend, row.names)

  ## bind postion end data
  edge.pos <- tibble::tibble(x = x[start], y = y[start],
                             xend = xend[end], yend = yend[end])
  node.pos <- tibble::tibble(x = c(x[spec.name], xend[row.names]),
                             y = c(y[spec.name], yend[row.names]),
                             label = c(spec.name, row.names),
                             is.start = rep(c(TRUE, FALSE), c(m, n)))
  structure(.Data = dplyr::bind_cols(edge.pos, data), node.pos = node.pos, type = type,
            class = c("combination_layout_tbl", "layout_tbl", class(edge.pos)))
}

#' @rdname create_layout
#' @format NULL
#' @usage NULL
#' @export
add_link <- function(...) {
  warning("`add_link()` is deprecated. ",
          "Please see `?combination_layout` for more detail.", call. = FALSE)
  list()
}
