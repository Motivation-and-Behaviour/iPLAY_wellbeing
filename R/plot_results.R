#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param analysis
#' @return
#' @author Taren Sanders
#' @export
plot_results <- function(analysis, df_clean) {
  df_clean_t0 <- df_clean %>%
    dplyr::filter(time_point == 0)

  low_wb <- mean(df_clean_t0$cwb_who_total_f) -
    (2 * sd(df_clean_t0$cwb_who_total_f))

  mean_wb <- mean(df_clean_t0$cwb_who_total_f)

  pred_df <- data.frame(
    intervention = rep(c(0, 1), each = 300),
    cwb_who_total_f = rep(c(low_wb, mean_wb), each = 150),
    wellbeing_lab = factor(rep(c("Low", "Average"), each = 150), levels = c(c("Low", "Average"))),
    time_point = rep(c(0, 1, 2), length = 300)
  )

  pred_df$cwb_pred <- predict(analysis$unadjusted, pred_df, re.form = NA)

  ggplot(
    pred_df,
    aes(x = time_point, y = cwb_pred, col = factor(intervention))
  ) +
    geom_line() +
    facet_grid(~wellbeing_lab)
}