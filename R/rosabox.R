#' rosabox
#'
#' Produces a boxplot and histogram of the df using `ggplot2`
#'
#' @param df a data frame
#' @param var a column vector from the data frame (numerical)
#' @param ... passes extra arguments to the function
#'
#' @importFrom ggplot2 ggplot geom_boxplot geom_histogram layer_data aes
#' @importFrom dplyr %>% mutate if_else
#'
#' @return a boxplot and histogram, plus a list of data from the `geom_boxplot` function output
#' @export
#'
#' @examples
#' \dontrun{rosabox(ddt, LENGTH)}
rosabox <- function(df, var, ...){

  pbase <- ggplot(df, aes(x = .data[[var]]))

  pb <- pbase +
    geom_boxplot()

  print(pb)

  out <-  layer_data(pb)
  outliers <- out$outliers

  df_new <- df %>%
    mutate(inout = if_else(is.element(.data[[var]], outliers[[1]]), "out", "in"))

  ph <- pbase +
    geom_histogram(data = df_new, aes(fill = inout))

  print(ph)

  list(out = out, df_new = df_new)
}
