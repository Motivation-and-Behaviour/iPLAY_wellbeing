#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_raw
#' @return
#' @author Taren Sanders
#' @export
clean_data <- function(df_raw) {
  df_clean <- df_raw %>%
    dplyr::mutate(
      time_point = factor(
        time_point,
        levels = c(0, 1, 2),
        labels = c("Baseline", "12m", "24m")
      )
    ) %>%
    # Fix missing data
    group_by(time_point) %>%
    mutate(
      cdem_age = if_else(is.na(cdem_age), median(cdem_age), cdem_age),
      cdem_atsi = forcats:::fct_explicit_na(cdem_atsi, "Don't know")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(cid_id) %>%
    dplyr::mutate(
      cwb_who_total_f = dplyr::first(cwb_who_total),
      across(c(
        cdem_age, cdem_atsi, cdem_atsi, cdem_bornaus, cdem_lang, cdem_sex,
        cdem_wealth
      ), dplyr::first)
      # Covariates should be set to baseline
    ) %>%
    dplyr::ungroup() %>%
    # Exclude those without any outcome data
    tidyr::drop_na(cwb_who_total)

  return(df_clean)
}