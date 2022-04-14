## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  # Load data
  tar_target(df_raw, get_data()),

  # Clean and tidyset
  tar_target(df_clean, clean_data(df_raw)),
  tar_target(quantiles, get_quantiles(df_clean)),

  # Create some descriptive tables and plots?
  tar_target(desc_table, make_desc_table(df_clean, quantiles)),

  # Analyse data
  tar_target(analysis, analyse_data(df_clean)),

  # Describe the results
  tar_target(results_table, make_results_table(analysis)),
  tar_target(plots, plot_results(analysis, df_clean, quantiles)),

  # Generate the report
  tar_render(report, "docs/index.Rmd")
)