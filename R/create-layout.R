#' Transform data base on different layout
#' @description These layout functions are not layout in the network diagram,
#' it just converts the original data into a form that makes it easy to draw
#' a curve graph.
#' @param data a data frame.
#' @param start.var,end.var character to specify which variable is the starting
#' points and which is the ending points. if the variable is not character, it
#' is forced to be converted.
#' @param horiz a logical value. If FALSE, the parallel graph are drawn vertically.
#' If TRUE, the parallel graph are drawn horizontally.
#' @param stretch logical, indicating whether the heights/width of start points and
#' end points are consistent.
#' @param sort.start,sort.end charater vector, the nodes will be sorted by this parameter.
#' @param start.x,start.y,end.x,end.y numeric to specify the x (horiz = TRUE) or y
#' (horiz = FALSE) coordinates.
#' @param type the type (""upper" or "lower") of the correlation matrix plot.
#' @param show.diag a logical value indicating whether keep the diagonal.
#' @param row.names,col.names row/column names of correlation matrix.
#' @param cor_tbl a col_tbl object.
#' @return a data frame.
#' @importFrom rlang enquo eval_tidy set_names quo_is_null
#' @importFrom dplyr filter
#' @rdname create-layout
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
                            start.var = NULL,
                            end.var = NULL,
                            horiz = FALSE,
                            stretch = TRUE,
                            sort.start = NULL,
                            sort.end = NULL,
                            start.x = NULL,
                            start.y = NULL,
                            end.x = NULL,
                            end.y = NULL)
{
  if(!is.data.frame(data))
    data <- as.data.frame(data)
  start <- if(rlang::quo_is_null(enquo(start.var))) {
    data[[1]]
  } else {
    rlang::eval_tidy(rlang::enquo(start.var), data)
  }
  end <- if(rlang::quo_is_null(enquo(end.var))) {
    data[[2]]
  } else {
    rlang::eval_tidy(rlang::enquo(end.var), data)
  }
  if(!is.character(start))
    start <- as.character(start)
  if(!is.character(end))
    end <- as.character(end)

  start.unique <- unique(start[!is.na(start)])
  end.unique <- unique(end[!is.na(end)])
  start.len <- length(start.unique)
  end.len <- length(end.unique)
  n <- max(start.len, end.len)
  if(!is.null(sort.start) && length(sort.start) != length(start.unique)) {
    stop("Length of 'sort.start' and unique elements of 'start' don't match.",
          call. = FALSE)
  }
  if(!is.null(sort.end) && length(sort.end) != length(end.unique)) {
    stop("Length of 'sort.end' and unique elements of 'end' don't match.",
          call. = FALSE)
  }
  start.pos <- if(is.null(sort.start)) {
    if(start.len < n && !stretch) {
      rlang::set_names(
        seq(n, 1, length.out = start.len + 2)[-c(1, start.len + 2)], start.unique)
    } else {
      rlang::set_names(seq(n, 1, length.out = start.len), start.unique)
    }
  } else {
    if(start.len < n && !stretch) {
      rlang::set_names(
        seq(n, 1, length.out = start.len + 2)[-c(1, start.len + 2)], sort.start)
    } else {
      rlang::set_names(seq(n, 1, length.out = start.len), sort.start)
    }
  }

  end.pos <- if(is.null(sort.end)) {
    if(end.len < n && !stretch) {
      rlang::set_names(
        seq(n, 1, length.out = end.len + 2)[-c(1, end.len + 2)], end.unique)
    } else {
      rlang::set_names(seq(n, 1, length.out = end.len), end.unique)
    }
  } else {
    if(end.len < n && !stretch) {
      rlang::set_names(
        seq(n, 1, length.out = end.len + 2)[-c(1, end.len + 2)], sort.end)
    } else {
      rlang::set_names(seq(n, 1, length.out = end.len), sort.end)
    }
  }

  start.width <- if(start.len < n && !stretch) {
    n / (start.len + 2)
  } else n / start.len

  end.width <- if(end.len < n && !stretch) {
    n / (end.len + 2)
  } else n / end.len

  pos <- if(isTRUE(horiz)) {
    tibble::tibble(x = start.pos[start], y = start.y %||% 0, xend = end.pos[end],
                   yend = end.y %||% 1, start.label = start, end.label = end,
                   start.width = start.width, end.width = end.width,
                   .start.filter = !duplicated(start) & !is.na(start),
                   .end.filter = !duplicated(end) & !is.na(end))
  } else {
    tibble::tibble(x = start.x %||% 0, y = start.pos[start], xend = end.x %||% 1,
                   yend = end.pos[end], start.label = start, end.label = end,
                   start.width = start.width, end.width = end.width,
                   .start.filter = !duplicated(start) & !is.na(start),
                   .end.filter = !duplicated(end) & !is.na(end))
  }
  structure(.Data = dplyr::bind_cols(pos, data), horiz = horiz,
            class = c("layout_link_tbl", class(pos)))
}

#' @rdname create-layout
#' @export
combination_layout <- function(data,
                               type = NULL,
                               show.diag = NULL,
                               row.names = NULL,
                               col.names = NULL,
                               start.var = NULL,
                               end.var = NULL,
                               cor_tbl)
{
  non.cor.tbl <- missing(cor_tbl)
  if(!non.cor.tbl) {
    if(!is_cor_tbl(cor_tbl) || !is_symmet(cor_tbl))
      stop("Need a symmetric cor_tbl.", call. = FALSE)
  }
  row.names <- if(non.cor.tbl) {
    rev(row.names) %||% col.names
  } else {
    rev(get_row_name(cor_tbl))
  }
  type <- if(non.cor.tbl) type else get_type(cor_tbl)
  show.diag <- if(non.cor.tbl) show.diag else get_show_diag(cor_tbl)
  start <- if(rlang::quo_is_null(enquo(start.var))) {
    data[[1]]
  } else {
    rlang::eval_tidy(rlang::enquo(start.var), data)
  }
  end <- if(rlang::quo_is_null(enquo(end.var))) {
    data[[2]]
  } else {
    rlang::eval_tidy(rlang::enquo(end.var), data)
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
  x <- rlang::set_names(x, spec.name)
  y <- rlang::set_names(y, spec.name)

  ## get position of env point
  xend <- n:1
  yend <- 1:n
  if(type == "upper") {
    if(show.diag) {
      xend <- xend - 2
    } else {
      xend <- xend - 1
    }
  } else {
    if(show.diag) {
      xend <- xend + 2
    } else {
      xend <- xend + 1
    }
  }
  xend <- rlang::set_names(xend, row.names)
  yend <- rlang::set_names(yend, row.names)

  ## bind postion end data
  pos <- tibble::tibble(x = x[start], y = y[start],
                        xend = xend[end], yend = yend[end],
                        start.label = start, end.label = end,
                        .start.filter = !duplicated(start) & !is.na(start),
                        .end.filter = !duplicated(end) & !is.na(end))
  structure(.Data = dplyr::bind_cols(pos, data),
            class = c("layout_link_tbl", class(pos)))
}
