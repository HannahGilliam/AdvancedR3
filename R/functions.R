#' Title Calculate descriptive statistics of each metabolite
#'
#' @param data The lipidomics dataset.
#'
#' @return A data.frame/tibble.

descriptive_stats <- function(data) {
  data %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(dplyr::across(value, list(
      mean = mean,
      sd = sd,
      iqr = IQR,
      median = median
    ))) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(.x, digits = 1)))
}

#' Title Plot distributions
#'
#' @param data Lipidomics data
#'
#' @return plots the distribution of values one histogram per metabolite

plot_distributions <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}
