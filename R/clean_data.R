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
    dplyr::group_by(cid_id) %>%
    dplyr::mutate(cwb_who_total_f = dplyr::first(cwb_who_total)) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na()

  # TODO: Fix the missing data

  return(df_clean)
}