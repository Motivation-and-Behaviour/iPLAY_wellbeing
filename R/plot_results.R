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
    (1 * sd(df_clean_t0$cwb_who_total_f))

  mean_wb <- mean(df_clean_t0$cwb_who_total_f)

  high_wb <- mean(df_clean_t0$cwb_who_total_f) +
    (1 * sd(df_clean_t0$cwb_who_total_f))

  quantiles <- quantile(df_clean_t0$cwb_who_total_f,
    probs = c(0.10, 0.2, 0.50, 0.80)
  )
  very_low_wb <- quantiles[1]
  low_wb <- quantiles[2]
  mean_wb <- quantiles[3]
  high_wb <- quantiles[4]


  pr <- ggpredict(analysis$unadjusted,
    terms = c(
      "time_point[0,1,2]",
      "intervention [0,1]",
      glue::glue("cwb_who_total_f[{very_low_wb},{low_wb},{mean_wb},{high_wb}]")
    )
  )

  wb_categories <- c(
    `32` = "Very Low (10th Percentile)",
    `36` = "Low (20th Percentile)",
    `41` = "Average (50th Percentile)",
    `45` = "High (80th Percentile)"
  )

  ggplot(pr, aes(x, predicted, colour = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
      alpha = 0.3, linetype = 0
    ) +
    facet_wrap(~facet, ncol = 4, labeller = as_labeller(wb_categories)) +
    scale_x_continuous(
      name = "Time Point",
      breaks = c(0, 1, 2),
      labels = c("Baseline", "12M", "24M")
    ) +
    scale_fill_manual(
      name = "",
      labels = c("Control", "Intervention"),
      values = c(tidyMB::iplay_pal()[[1]], tidyMB::iplay_pal()[[2]])
    ) +
    scale_colour_manual(
      name = "",
      labels = c("Control", "Intervention"),
      values = c(tidyMB::iplay_pal()[[1]], tidyMB::iplay_pal()[[2]])
    ) +
    labs(
      y = "Predicted Student Wellbeing",
      title = "Effect of the iPLAY Program on Student Wellbeing",
      subtitle = "Moderation by Initial Wellbeing"
    ) +
    tidyMB::theme_mb() +
    theme(
      legend.position = "bottom",
      text = element_text(family = "sans")
    )
}