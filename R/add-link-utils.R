#' @noRd
#' @importFrom dplyr %>% left_join
#' @importFrom stats setNames
tidy_link_data <- function(df,
                             cor_tbl,
                             spec.key = "spec",
                             env.key = "env",
                             env.point.hjust = NULL,
                             env.point.vjust = NULL,
                             spec.point.hjust = NULL,
                             spec.point.vjust = NULL,
                             on.left = FALSE,
                             diag.label = FALSE)

{
  if(!is_cor_tbl(cor_tbl))
    stop("Need a cor_tbl.", call. = FALSE)
  xname <- cor_tbl_xname(cor_tbl)
  yname <- cor_tbl_yname(cor_tbl)
  type <- cor_tbl_type(cor_tbl)
  show.diag <- cor_tbl_showdiag(cor_tbl)
  spec.name <- unique(df[[spec.key]])
  env_data <- link_env_data(yname = yname,
                            n.row = length(yname),
                            n.col = length(xname),
                            type = type,
                            show.diag = show.diag,
                            hjust = env.point.hjust,
                            vjust = env.point.vjust,
                            on.left = on.left,
                            diag.label = diag.label)
  group_data <- link_spec_data(spec.name = spec.name,
                               n.row = length(yname),
                               n.col = length(xname),
                               type = type,
                               hjust = spec.point.hjust,
                               vjust = spec.point.vjust,
                               on.left = on.left)
  link_data <- df %>%
    dplyr::left_join(env_data, by = setNames("env.key", env.key)) %>%
    dplyr::left_join(group_data, by = setNames("spec.key", spec.key))
  structure(.Data = link_data, class = c("link_tbl", class(link_data)))
}

#' @noRd
link_spec_data <- function(spec.name,
                           n.row,
                           n.col,
                           type,
                           hjust = NULL,
                           vjust = NULL,
                           on.left = FALSE)
{
  len <- length(spec.name)
  if(!is.null(hjust) && length(hjust) != len)
    hjust <- rep_len(hjust, len)
  if(!is.null(vjust) && length(hjust) != len)
    vjust <- rep_len(vjust, len)
  if(type == "full") {
    y <- seq(0.5, n.row + 0.5, length.out = len + 2)[2:(len + 1)]
    if(on.left) {
      x <- rep_len(0.5 - 0.40 * n.col, len)
    } else {
      x <- rep_len(n.col + 0.5 + 0.40 * n.col, len)
    }
  } else if(type == "upper") {
    if(len == 1) {
      x <- 0.5 + 0.18 * n.col
      y <- 0.5 + 0.3 * n.row
    } else if(len == 2) {
      x <- c(0.5 - 0.02 * n.col, 0.5 + 0.2 * n.col)
      y <- c(0.5 + 0.46 * n.row, 0.5 + 0.2 * n.row)
    } else {
      y0 <- 0.5 + n.row * (1 - 0.3)
      y1 <- 0.5 + n.row * 0.1
      x0 <- 0.5 - 0.25 * n.col
      x1 <- 0.5 + 0.3 * n.col
      y <- seq(y0, y1, length.out = len)
      x <- seq(x0, x1, length.out = len)
    }
  } else {
    if(len == 1) {
      x <- 0.5 + 0.82 * n.col
      y <- 0.5 + 0.7 * n.row
    } else if(len == 2) {
      x <- c(0.5 + 0.8 * n.col, 0.5 + 1.02 * n.col)
      y <- c(0.5 + 0.8 * n.row, 0.5 + 0.54 * n.row)
    } else {
      y0 <- 0.5 + n.row * (1 - 0.1)
      y1 <- 0.5 + n.row * 0.3
      x0 <- 0.5 + 0.75 * n.col
      x1 <- 0.5 + 1.3 * n.col
      y <- seq(y0, y1, length.out = len)
      x <- seq(x0, x1, length.out = len)
    }
  }
  if(!is.null(hjust))
    x <- x + hjust
  if(!is.null(vjust))
    y <- y + vjust
  tibble::tibble(x = x,
                 y = y,
                 spec.key = spec.name)
}

#' @noRd
link_env_data <- function(yname,
                          n.row,
                          n.col,
                          type = "upper",
                          show.diag = FALSE,
                          hjust = NULL,
                          vjust = NULL,
                          on.left = FALSE,
                          diag.label = FALSE)
{
  if(!is.null(hjust) && length(hjust) != n.row)
    hjust <- rep_len(hjust, n.row)
  if(!is.null(vjust) && length(hjust) != n.row)
    vjust <- rep_len(vjust, n.row)
  x <- n.col:1
  y <- 1:n.row
  if(type == "full") {
    if(on.left) {
      x <- rep_len(0, n.row)
    } else {
      x <- rep_len(n.col + 1, n.row)
    }
  } else {
    if(type == "upper") {
      if(show.diag) {
        if(diag.label) {
          x <- x - 2
        } else {
          x <- x - 1
        }
      } else {
        if(diag.label) {
          x <- x - 1
        }
      }
    } else {
      if(show.diag) {
        if(diag.label) {
          x <- x + 2
        } else {
          x <- x + 1
        }
      } else {
        if(diag.label) {
          x <- x + 1
        }
      }
    }
  }
  if(!is.null(hjust))
    x <- x + hjust
  if(!is.null(vjust))
    y <- y + vjust
  tibble::tibble(xend = x,
                 yend = y,
                 env.key = yname)
}

#' @noRd
link_colour_pal <- function(n)
{
  stopifnot(n <= 6)
  colors <- c("#D95F02", "#1B9E77", "#7570B3",
              "#E7298A", "#A6761D", "#CCCCCC")
  if(n == 1)
    return(colors[1])
  col <- c(colors[1:(n - 1)], colors[6])
  col

}
