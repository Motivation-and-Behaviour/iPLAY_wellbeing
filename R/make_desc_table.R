#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_clean
#' @return
#' @author Taren Sanders
#' @export
make_desc_table <- function(df_clean, quantiles) {
  df_clean_table <-
    df_clean %>%
    dplyr::filter(time_point == "Baseline") %>%
    dplyr::select(
      -cwb_who_total_f, -cid_id, -sid_id, -time_point, -sid_status
    ) %>%
    dplyr::mutate(
      wellbeing_group = dplyr::case_when(
        cwb_who_total <= quantiles[1] ~ "Very Low",
        cwb_who_total <= quantiles[2] ~ "Low",
        cwb_who_total <= quantiles[4] ~ "Average",
        TRUE ~ "High"
      ),
      wellbeing_group = factor(
        wellbeing_group,
        c("Very Low", "Low", "Average", "High")
      )
    )


  gtsummary::tbl_summary(df_clean_table,
    by = wellbeing_group,
    type = list(cdem_age ~ "continuous"),
    statistic = list(cdem_age ~ "{mean} ({sd})")
  ) %>%
    gtsummary::modify_caption("Baseline characteristics of participants") %>%
    gtsummary::bold_labels() %>%
    gtsummary::add_overall() %>%
    gtsummary::modify_spanning_header(
      stat_1:stat_4 ~ "**Baseline Wellbeing**"
    ) %>%
    gtsummary::modify_footnote(
      stat_1 ~ "&le;10th Percentile",
      stat_2 ~ "&gt;10th - &le;20th Percentile",
      stat_3 ~ "&gt;20th - &le;80th Percentile",
      stat_4 ~ "&gt;80th Percentile"
    ) %>%
    gtsummary::modify_header(all_stat_cols(FALSE) ~ "**{level}**<br> N = {n}")
}