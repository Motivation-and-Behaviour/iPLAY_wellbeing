#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @author Taren Sanders
#' @param df_clean
get_quantiles <- function(df_clean) {
  df_clean_t0 <- df_clean %>%
    dplyr::filter(time_point == "Baseline")

  quantiles <- quantile(df_clean_t0$cwb_who_total_f,
    probs = c(0.1, 0.2, 0.50, 0.80)
  )

  return(quantiles)
}