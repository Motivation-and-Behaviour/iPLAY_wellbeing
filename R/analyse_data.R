#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_clean
#' @return
#' @author Taren Sanders
#' @export
analyse_data <- function(df_clean) {
  model_unadj <- lmerTest::lmer(cwb_who_total ~
  intervention * time_point * cwb_who_total_f +
    (1 | cid_id) + (1 | sid_id), data = df_clean)

  model_adj <- lmerTest::lmer(cwb_who_total ~
  intervention * time_point * cwb_who_total_f +
    cdem_age + cdem_atsi + cdem_books + cdem_lang +
    cdem_sex + cdem_wealth + sdem_icsea + sdem_remote +
    (1 | cid_id) + (1 | sid_id), data = df_clean)

  return(list(
    "unadjusted" = model_unadj,
    "adjusted" = model_adj
  ))
}