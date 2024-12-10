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
        sd = sd
      )
    )) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        ~ round(.x, digits = 1)
      )
    )
}
