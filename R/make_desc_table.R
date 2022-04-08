#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_clean
#' @return
#' @author Taren Sanders
#' @export
make_desc_table <- function(df_clean) {
  df_clean <-
    df_clean %>%
    dplyr::filter(time_point == 0) %>%
    dplyr::select(-cwb_who_total_f, -cid_id, -sid_id, -time_point, -sid_status)

  gtsummary::tbl_summary(df_clean,
    type = list(cdem_age ~ "continuous"),
    statistic = list(cdem_age ~ "{mean} ({sd})")
  ) %>%
    modify_caption("Baseline characteristics of participants")
}