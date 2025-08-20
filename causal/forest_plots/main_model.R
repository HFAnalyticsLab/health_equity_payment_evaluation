## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: main_model.R

# Corresponding author:Sarah Opie-Martin (sarah.opie-martin@health.org.uk)

# Description:
# Create forest plots for main model (figure 3) and missing data analysis

# Notes:
## ========================================================================== ##


source("~/causal/preamble.R")

### read in the effect estimates from the matching_and_estimation.R
main_model <- readRDS("main_models_noPMS.rds")

options(scipen = 10)

fp_df <- main_model %>%
  rename(
    mean = estimate,
    lower = conf.low,
    upper = conf.high,
    study = out
  ) %>%
  mutate(
    study = case_when(
      study == "q18_12pct_POST_INT" ~ "Overall how would you describe your experience of contacting your GP practice?",
      study == "q28_12pct_POST_INT" ~ "Overall how would you describe your experience of your GP practice?",
      study == "q90_12pct_POST_INT" ~ "Thinking about your last appointment, were your needs were met?",
      study == "qof_percent_POST_INT" ~ "Percentage of total QOF points achieved",
      study == "total_gp_extg_fte_weighted_POST_INT" ~ "FTE GPs excluding training grade/ 1000 weighted patients",
      study == "total_nurses_fte_weighted_POST_INT" ~ "FTE Nurses/ 1000 weighted patients",
      study == "total_admin_fte_weighted_POST_INT" ~ "FTE admin/ 1000 weighted patients"
    )
  ) %>%
  mutate(across(
    c(mean, lower, upper),
    ~ case_when(
      grepl("Overall", study) | study == "Percentage of total QOF points achieved" ~ . * 100,
      TRUE ~ .
    )
  )) %>%
  mutate(
    estimate = paste0(round(mean, 2), " (", round(lower, 2), ", ", round(upper, 2), ")"),
    p_value = round(p.value, 2),
  )



tiff("forest_plot.tiff", units = "in", width = 10, height = 5, res = 300)


fp_df %>%
  forestplot(labeltext = c(study, estimate, p_value), clip = c(-6, 6)) %>%
  fp_set_style(box = "#189EDC", line = "#11609C") %>%
  fp_add_header(study = "Outcome measure", estimate = "Effect estimate (95% CI)", p_value = "p value") %>%
  fp_set_zebra_style("#EFEFEF") %>%
  fp_set_style(txt_gp = fpTxtGp(label = gpar(cex = 0.8), ticks = gpar(cex = 0.7)))


dev.off()

all_main_models_missing <- readRDS("all_models_missing.rds")

fp_df <- all_main_models_missing %>%
  rename(
    mean = estimate,
    lower = conf.low,
    upper = conf.high,
    study = out
  ) %>%
  mutate(
    study = case_when(
      study == "q18_12pct_POST_INT" ~ "Overall how would you describe your experience of contacting your GP practice?",
      study == "q28_12pct_POST_INT" ~ "Overall how would you describe your experience of your GP practice?",
      study == "q90_12pct_POST_INT" ~ "Thinking about your last appointment, were your needs were met?",
      study == "qof_percent_POST_INT" ~ "Percentage of total QOF points achieved",
      study == "total_gp_extg_fte_weighted_POST_INT" ~ "FTE GPs excluding training grade/ 1000 weighted patients"
    ),
    estimate = paste0(round(mean, 3), " (", round(lower, 3), ", ", round(upper, 3), ")"),
    p_value = round(p.value, 5)
  )

fp_df %>%
  forestplot(labeltext = c(study, estimate, p_value), clip = c(-0.2, 0.05)) %>%
  fp_set_style(box = "#189EDC", line = "#11609C") %>%
  fp_add_header(study = "Outcome measure", estimate = "Effect estimate (95% CI)", p_value = "p value") %>%
  fp_set_zebra_style("#EFEFEF") %>%
  fp_set_style(txt_gp = fpTxtGp(label = gpar(cex = 0.7), ticks = gpar(cex = 0.7)))
