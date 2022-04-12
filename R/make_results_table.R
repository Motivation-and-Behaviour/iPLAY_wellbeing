#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param analysis
make_results_table <- function(analysis) {
  unadj_table <- gtsummary::tbl_regression(analysis$unadjusted,
    estimate_fun = purrr::partial(gtsummary::style_number, digits = 2),
    pvalue_fun = purrr::partial(gtsummary::style_pvalue, digits = 3),
    label = list(
      intervention ~ "Intervention",
      time_point ~ "Time Point",
      cwb_who_total_f ~ "Baseline Wellbeing"
    )
  ) %>%
    gtsummary::bold_p() %>%
    gtsummary::modify_header(label = "") %>%
    gtsummary::modify_caption(
      "**Effect of the *iPLAY* Intervention on Student Wellbeing**"
    )

  adj_table <-
    gtsummary::tbl_regression(analysis$adjusted,
      estimate_fun = purrr::partial(gtsummary::style_number, digits = 2),
      pvalue_fun = purrr::partial(gtsummary::style_pvalue, digits = 3),
      label = list(
        intervention ~ "Intervention",
        time_point ~ "Time Point",
        cwb_who_total_f ~ "Baseline Wellbeing"
      )
    ) %>%
    gtsummary::bold_p() %>%
    gtsummary::modify_header(label = "") %>%
    gtsummary::modify_caption(
      "**Effect of the *iPLAY* Intervention on Student Wellbeing**"
    )

  return(list("unadjusted" = unadj_table, "adjusted" = adj_table))
}