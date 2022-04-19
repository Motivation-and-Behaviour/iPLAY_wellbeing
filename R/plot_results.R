#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param analysis
#' @return
#' @author Taren Sanders
#' @export
plot_results <- function(analysis, df_clean, quantiles) {
  pr <- ggpredict(analysis$adjusted,
    terms = c(
      "time_point",
      "intervention [0,1]",
      "cwb_who_total_f[32,35,41,45]"
    )
  )

  wb_categories <- c(
    `32` = "Very Low\n(10th Percentile)",
    `35` = "Low\n(20th Percentile)",
    `41` = "Average\n(50th Percentile)",
    `45` = "High\n(80th Percentile)"
  ) # TODO: Can this be un-hardcoded?

  ggplot(pr, aes(x, predicted, colour = group, group = group)) +
    geom_line() +
    facet_wrap(~facet, ncol = 4, labeller = as_labeller(wb_categories)) +
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
      x = "Time Point",
      title = "Effect of the iPLAY Program on Student Wellbeing",
      subtitle = "Moderation by Initial Wellbeing"
    ) +
    tidyMB::theme_mb() +
    theme(
      legend.position = "bottom",
      text = element_text(family = "sans")
    )
}