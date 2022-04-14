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
        cwb_who_total_f ~ "Baseline Wellbeing",
        cdem_age ~ "Child Age",
        cdem_atsi ~ "Child Indigenous Status",
        cdem_books ~ "Number of Books in Home",
        cdem_lang ~ "Language Spoken at Home",
        cdem_sex ~ "Child Sex",
        cdem_wealth ~ "Child's Perceived Wealth",
        sdem_icsea ~ "School ICSEA",
        sdem_remote ~ "School Remoteness"
      )
    ) %>%
    gtsummary::bold_p() %>%
    gtsummary::modify_header(label = "") %>%
    gtsummary::modify_caption(
      "**Effect of the *iPLAY* Intervention on Student Wellbeing**"
    ) %>%
    gtsummary::bold_labels()

  strat_table <-
    gtsummary::tbl_regression(analysis$strat,
      estimate_fun = purrr::partial(gtsummary::style_number, digits = 2),
      pvalue_fun = purrr::partial(gtsummary::style_pvalue, digits = 3),
      label = list(
        intervention ~ "Intervention",
        time_point ~ "Time Point",
        cdem_age ~ "Child Age",
        cdem_atsi ~ "Child Indigenous Status",
        cdem_books ~ "Number of Books in Home",
        cdem_lang ~ "Language Spoken at Home",
        cdem_sex ~ "Child Sex",
        cdem_wealth ~ "Child's Perceived Wealth",
        sdem_icsea ~ "School ICSEA",
        sdem_remote ~ "School Remoteness"
      )
    ) %>%
    gtsummary::bold_p() %>%
    gtsummary::modify_header(label = "") %>%
    gtsummary::modify_caption(
      "**Effect of the *iPLAY* Intervention on Student Wellbeing**"
    ) %>%
    gtsummary::bold_labels()

  return(list(
    "unadjusted" = unadj_table,
    "adjusted" = adj_table,
    "strat" = strat_table
  ))
}