#' Calculate descriptive statistics of each metabolite.
#'
#' @param data The lipidomics dataset.
#'
#' @return A data.frame.
#'
descriptive_stats <- function(data) {
  data |>
    dplyr::group_by(metabolite) |>
    dplyr::summarise(dplyr::across(
      value,
      list(
        mean = mean,
        sd = sd,
        median = median,
        iqr = IQR
      )
    )) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        ~ round(.x, digits = 1)
      )
    )
}


#' Plot for basic distribution of metabolite data.
#'
#' @param data The lipidomics dataset.
#'
#' @return A ggplot2 graph.
#'
plot_distributions <- function(data) {
    data |>
        ggplot2::ggplot(ggplot2::aes(x = value)) +
        ggplot2::geom_histogram() +
        ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}


#' Convert a columnøs character values to snakecase format.
#'
#' @param data The lipidomics dataset.
#' @param columns The column you want to convert into the snakecase format.
#' @return A data frame.
#'
column_values_to_snake_case <- function(data, columns) {
    data |>
        dplyr::mutate(dplyr::across({{ columns }}, snakecase::to_snake_case))
}
