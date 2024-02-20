#' rosabox
#'
#' Produces a boxplot and histogram of the df using `ggplot2`
#'
#' @param df a data frame
#' @param x a column vector from the data frame (numerical)
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
rosabox <- function(df, x, ...){

  .data <- NULL
  inout <- NULL


  pbase <- ggplot(df, aes(x = .data[[x]]))

  pb <- pbase +
    geom_boxplot()

  print(pb)

  out <-  layer_data(pb)
  outliers <- out$outliers
  iqrint <- c(out$xlower, out$xupper)

  df_new <- df %>%
    mutate(inout = if_else(is.element(.data[[x]], outliers[[1]]), "out", "in"))

  df_new <- df_new %>%
    mutate(iqr1 = ifelse(.data[[x]] >= out$xlower & .data[[x]] <= out$xupper, "iqr", "no"))

  df_new$inout[df_new$inout == "in" & df_new$iqr1 == "iqr"] <- "iqr"

  ph <- pbase +
    geom_histogram(data = df_new, aes(fill = inout))

  print(ph)

  list(out = out, outliers = outliers, df_new = df_new, iqr = iqrint)
}


