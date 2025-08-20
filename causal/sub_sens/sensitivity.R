## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: sensitivity.R

# Corresponding author:Sarah Opie-Martin (sarah.opie-martin@health.org.uk)

# Description:
# Perform specified sensitivity analyses

# Dependencies: preamble.R
#

# Inputs:
# matched data object
# outcomes files
# outputs from clean_data.R

# Outputs:
# Model results for all specified sensitivity analyses

# Notes:
## ========================================================================== ##


source("~/casual/preamble.R")

### read in matcheding object saved in matching_and_estimation.R
m.out0 <- readRDS("matching_agreed_noPMS.rds")


md <- match_data(m.out0)

outcomes <- c("q18_12pct_POST_INT", "q28_12pct_POST_INT", "q90_12pct_POST_INT", "qof_percent_POST_INT", "total_gp_extg_fte_weighted_POST_INT", "total_nurses_fte_weighted_POST_INT", "total_admin_fte_weighted_POST_INT")
preintervention_outcomes <- c("q18_12pct", "q28_12pct", "q90_12pct", "qof_percent", "total_gp_extg_fte_weighted", "total_nurses_fte_weighted", "total_admin_fte_weighted")

md <- md %>%
  mutate(
    treated = case_when(
      treated == TRUE ~ 1,
      treated == FALSE ~ 0
    ),
    treat_level = case_when(
      Amount_21_22 < 3 ~ 2,
      Amount_21_22 > 2 ~ 1,
      is.na(Amount_21_22) ~ 0
    ),
    qof_percent = qof_percent / 100,
    qof_percent_POST_INT = qof_percent_POST_INT / 100,
    treated = as.factor(treated),
    treat_level = as.factor(treat_level)
  )

## columns to select to that all resulting model outputs can be rbound
columns_to_select <- c("term", "contrast", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high", "out", "form")

#### controlling no variables ----------


no_covariates_model <- function(outcome, preintervention, binary) {
  print(outcome)

  comparison <- "~ treated"
  variable_att <- "treated"

  formula <- as.formula(paste(outcome, comparison))

  if (preintervention %in% c("q28_12pct", "q18_12pct", "q90_12pct", "qof_percent")) {
    fit <- glm(formula, data = md, family = quasibinomial())
  } else if (preintervention %in% c("total_gp_extg_fte_weighted", "total_nurses_fte_weighted", "total_admin_fte_weighted")) {
    fit <- glm(formula, data = md, family = "Gamma")
  }

  ATT <- avg_comparisons(fit,
    variables = variable_att,
    vcov = ~subclass,
    newdata = subset(treated == 1)
  )


  ATT_tidy <- as.data.frame(ATT) %>%
    mutate(
      out = outcome,
      form = Reduce(paste, deparse(formula))
    ) %>%
    dplyr::select(all_of(columns_to_select)) %>%
    mutate(model = "No covariates")
}

no_covariates_results <- mapply(no_covariates_model, outcome = outcomes, preintervention = preintervention_outcomes, SIMPLIFY = FALSE)

all_models_no_covariates <- do.call(rbind, no_covariates_results)



#### controlling pre intervention variables ----------


preintervention_only_model <- function(outcome, preintervention, binary) {
  print(outcome)

  comparison <- "~ treated * ("
  variable_att <- "treated"

  formula <- as.formula(paste(outcome, paste(comparison, paste(preintervention, collapse = " + "), ")")))

  if (preintervention %in% c("q28_12pct", "q18_12pct", "q90_12pct", "qof_percent")) {
    fit <- glm(formula, data = md, family = quasibinomial())
  } else if (preintervention %in% c("total_gp_extg_fte_weighted", "total_nurses_fte_weighted", "total_admin_fte_weighted")) {
    fit <- glm(formula, data = md, family = "Gamma")
  }

  ATT <- avg_comparisons(fit,
    variables = variable_att,
    vcov = ~subclass,
    newdata = subset(treated == 1)
  )


  ATT_tidy <- as.data.frame(ATT) %>%
    mutate(
      out = outcome,
      form = Reduce(paste, deparse(formula))
    ) %>%
    dplyr::select(all_of(columns_to_select)) %>%
    mutate(model = "Preintervention only covariates")
}

preintervention_only_results <- mapply(preintervention_only_model, outcome = outcomes, preintervention = preintervention_outcomes, SIMPLIFY = FALSE)

all_models_preintervention_only <- do.call(rbind, preintervention_only_results)



#### linear model -------


linear_model <- function(outcome, preintervention, binary) {
  print(outcome)

  comparison <- "~ treated * ("
  variable_att <- "treated"

  unbalanced_variables <- c("PERC_MALE", "chi")
  final_covariates <- unique(c(preintervention, unbalanced_variables))
  formula <- as.formula(paste(outcome, paste(comparison, paste(final_covariates, collapse = " + "), ")")))

  fit <- lm(formula, data = md)

  ATT <- avg_comparisons(fit,
    variables = variable_att,
    vcov = ~subclass,
    newdata = subset(treated == 1)
  )


  ATT_tidy <- as.data.frame(ATT) %>%
    mutate(
      out = outcome,
      form = Reduce(paste, deparse(formula))
    ) %>%
    dplyr::select(all_of(columns_to_select)) %>%
    mutate(model = "Linear models")
}

linear_results <- mapply(linear_model, outcome = outcomes, preintervention = preintervention_outcomes, SIMPLIFY = FALSE)

all_models_linear <- do.call(rbind, linear_results)

#### highest gpps scores 2024 -----

outcomes_2024 <- readRDS("cleaned_data/outcomes_2024.rds")

md_gpps <- md %>%
  left_join(dplyr::select(outcomes_2024, PRACTICE_CODE, gpcontactoverall_1_pct_POST_INT, lastgpapptneeds_1_pct_POST_INT, overallexp_1_pct_POST_INT), by = "PRACTICE_CODE")

outcomes_topgpps <- c("gpcontactoverall_1_pct_POST_INT", "overallexp_1_pct_POST_INT", "lastgpapptneeds_1_pct_POST_INT")
preintervention_outcomes_topgpps <- c("q18_12pct", "q28_12pct", "q90_12pct")

gpps_top_score_model <- function(outcome, preintervention, binary) {
  print(outcome)

  comparison <- "~ treated * ("
  variable_att <- "treated"

  unbalanced_variables <- c("PERC_MALE", "chi")
  final_covariates <- unique(c(preintervention, unbalanced_variables))
  formula <- as.formula(paste(outcome, paste(comparison, paste(final_covariates, collapse = " + "), ")")))

  fit <- glm(formula, data = md_gpps, family = quasibinomial())

  ATT <- avg_comparisons(fit,
    variables = variable_att,
    vcov = ~subclass,
    newdata = subset(treated == 1)
  )


  ATT_tidy <- as.data.frame(ATT) %>%
    mutate(
      out = outcome,
      form = Reduce(paste, deparse(formula))
    ) %>%
    dplyr::select(all_of(columns_to_select)) %>%
    mutate(model = "GPPS very good percent")
}

gpps_top_score_results <- mapply(gpps_top_score_model, outcome = outcomes_topgpps, preintervention = preintervention_outcomes_topgpps, SIMPLIFY = FALSE)

all_models_gpps_top_models <- do.call(rbind, gpps_top_score_results)

#### dose response models -----------


dose_reponse_model <- function(outcome, preintervention) {
  print(outcome)

  comparison <- "~ treat_level * ("
  variable_att <- "treat_level"

  unbalanced_variables <- c("PERC_MALE", "chi")
  final_covariates <- unique(c(preintervention, unbalanced_variables))
  formula <- as.formula(paste(outcome, paste(comparison, paste(final_covariates, collapse = " + "), ")")))

  if (preintervention %in% c("q28_12pct", "q18_12pct", "q90_12pct", "qof_percent")) {
    fit <- glm(formula, data = md, family = quasibinomial())
  } else if (preintervention %in% c("total_gp_extg_fte_weighted", "total_nurses_fte_weighted", "total_admin_fte_weighted")) {
    fit <- glm(formula, data = md, family = "Gamma")
  }


  ATT <- avg_comparisons(fit, variables = variable_att, vcov = ~subclass, newdata = subset(md, treated == 1))


  dep_form <- deparse(formula)

  # if(preintervention != "q90_12pct"){
  #   dep_form <- paste(dep_form[[1]], dep_form[[2]])
  # }


  ATT_tidy <- as.data.frame(ATT) %>%
    mutate(
      out = outcome,
      form = dep_form
    ) %>%
    dplyr::select(all_of(columns_to_select)) %>%
    mutate(model = "Dose response model")
}

dose_response_results <- mapply(dose_reponse_model, outcome = outcomes, preintervention = preintervention_outcomes, SIMPLIFY = FALSE)

all_models_dose_response <- do.call(rbind, dose_response_results)


#### GPPS 2023 results
outcomes_2023 <- readRDS("cleaned_data/outcomes_2023.rds")

md_gpps_2023 <- md %>%
  # swap the 2024 gpps scores for the 2023 ones
  dplyr::select(-c("q18_12pct_POST_INT", "q28_12pct_POST_INT", "q90_12pct_POST_INT")) %>%
  left_join(dplyr::select(outcomes_2023, PRACTICE_CODE, q18_12pct_POST_INT, q28_12pct_POST_INT, q90_12pct_POST_INT), by = "PRACTICE_CODE")

outcomes_gpps_2023 <- c("q18_12pct_POST_INT", "q28_12pct_POST_INT", "q90_12pct_POST_INT")
preintervention_outcomes_gpps_2023 <- c("q18_12pct", "q28_12pct", "q90_12pct")

gpps_2023_model <- function(outcome, preintervention, binary) {
  print(outcome)

  comparison <- "~ treated * ("
  variable_att <- "treated"

  unbalanced_variables <- c("PERC_MALE", "chi")
  final_covariates <- unique(c(preintervention, unbalanced_variables))
  formula <- as.formula(paste(outcome, paste(comparison, paste(final_covariates, collapse = " + "), ")")))

  fit <- glm(formula, data = md_gpps_2023, family = quasibinomial())

  ATT <- avg_comparisons(fit,
    variables = variable_att,
    vcov = ~subclass,
    newdata = subset(treated == 1)
  )


  ATT_tidy <- as.data.frame(ATT) %>%
    mutate(
      out = outcome,
      form = Reduce(paste, deparse(formula))
    ) %>%
    dplyr::select(all_of(columns_to_select)) %>%
    mutate(model = "GPPS 2023")
}

gpps_2023_results <- mapply(gpps_2023_model, outcome = outcomes_gpps_2023, preintervention = preintervention_outcomes_gpps_2023, SIMPLIFY = FALSE)

all_models_gpps_2023 <- do.call(rbind, gpps_2023_results)

### missing data analyses -----------

### read in data from cleaned+data.R
baseline <- readRDS("cleaned_data/baseline_data.rds")
## payments
payments <- readRDS("cleaned_data/practice_payments_cleaned.rds")
## outcome
outcomes_2024 <- readRDS("cleaned_data/outcomes_2024.rds")

# remove nursing and admin from the matching panel
static_matching_panel <- c(
  "IMD_quintile",
  "NUMBER_OF_PATIENTS",
  "chi",
  "PERC_65",
  "PERC_MALE",
  "POP_WHITE",
  "POP_ENG2",
  "POP_RURAL",
  "gp_partner_1",
  "contract_type",
  "AST",
  "CKD",
  "COPD",
  "CHD",
  "DM",
  "HYP",
  "q28_12pct", "q18_12pct", "q90_12pct", "qof_percent", "total_gp_extg_fte_weighted"
)

outcomes <- c("q18_12pct_POST_INT", "q28_12pct_POST_INT", "q90_12pct_POST_INT", "qof_percent_POST_INT", "total_gp_extg_fte_weighted_POST_INT")
preintervention_outcomes <- c("q18_12pct", "q28_12pct", "q90_12pct", "qof_percent", "total_gp_extg_fte_weighted")

baseline <- baseline %>%
  filter((LLR == "LLR" & treated == TRUE) | (LLR == "Not LLR" & treated == FALSE)) %>%
  filter(contract_type != "PMS") %>%
  mutate(
    IMD_quintile = as.factor(clean_quintile)
  ) %>%
  drop_na(all_of(static_matching_panel)) %>%
  left_join(payments, by = c("PRACTICE_CODE" = "Practice.Code")) %>%
  left_join(outcomes_2024, by = "PRACTICE_CODE")




formula <- as.formula(paste("treated ~ ", paste(static_matching_panel, collapse = " + ")))

set.seed(1234)

m.out0 <- matchit(
  formula,
  data = baseline,
  method = "genetic",
  distance = "glm",
  pop.size = 1000,
  replace = FALSE
)

saveRDS(m.out0, "matching_missingno_PMS.rds")

m.out0 <- readRDS("matching_missingno_PMS.rds")

branding_colours <- c("#EB003B", "#189EDC")

new_names <- c(
  IMD_quintile = "IMD quintile",
  NUMBER_OF_PATIENTS = "List size",
  chi = "Weighted patient factor",
  PERC_65 = "Percentage of patients >65 years old",
  PERC_MALE = "Percentage of male patients",
  POP_WHITE = "Percentage of population is white",
  POP_ENG2 = "Percentage of population with English as a second language",
  POP_RURAL = "Percentage of population living in rural areas",
  gp_partner_1 = "Practice partner count",
  contract_type = "Practice contract type",
  AST = "Asthma prevalence",
  CKD = "Chronic kidney disease prevalence",
  COPD = "Chronic obstructive pulmonary disease prevalence",
  CHD = "Coronary heart disease prevalence",
  DM = "Diabetes mellitus prevalence",
  HYP = "Hypertension prevalence",
  q28_12pct = "Percent good overall experience of practice",
  q18_12pct = "Percent good experience of contact",
  q90_12pct = "Percent 'yes' needs met at appointment",
  qof_percent = "Percent overall QOF achievement",
  total_gp_extg_fte_weighted = "FTE GPs (excluding training grade) / 1000 weighted patients",
  total_nurses_fte_weighted = "FTE Nurses / 1000 weighted patients",
  total_admin_fte_weighted = "FTE admin / 1000 weighted patients"
)

love.plot(formula,
  data = baseline,
  weights = m.out0,
  binary = "std",
  addl = c("total_nurses_fte_weighted", "total_admin_fte_weighted"),
  thresholds = c(m = .1, ks = .05, v = 2),
  colors = branding_colours,
  var.names = new_names,
  line = T
)

bal.tab(
  formula,
  data = baseline,
  weights = m.out0
)

md <- match_data(m.out0)

md <- md %>%
  # left_join(payments, by = c("PRACTICE_CODE" = "Practice.Code")) %>%
  # left_join(outcomes_2024, by = "PRACTICE_CODE") %>%
  mutate(
    treated = case_when(
      treated == TRUE ~ 1,
      treated == FALSE ~ 0
    ),
    treat_level = case_when(
      Amount_21_22 < 3 ~ 2,
      Amount_21_22 > 2 ~ 1,
      is.na(Amount_21_22) ~ 0
    ),
    qof_percent = qof_percent / 100,
    qof_percent_POST_INT = qof_percent_POST_INT / 100,
    treated = as.factor(treated),
    treat_level = as.factor(treat_level)
  )

outcomes <- c("q18_12pct_POST_INT", "q28_12pct_POST_INT", "q90_12pct_POST_INT", "qof_percent_POST_INT", "total_gp_extg_fte_weighted_POST_INT")
preintervention_outcomes <- c("q18_12pct", "q28_12pct", "q90_12pct", "qof_percent", "total_gp_extg_fte_weighted")


missing_data_model <- function(outcome, preintervention, binary) {
  print(outcome)

  comparison <- "~ treated * ("
  variable_att <- "treated"

  unbalanced_variables <- c("chi", "q18_12pct", "total_gp_extg_fte_weighted", "PERC_65")
  final_covariates <- unique(c(preintervention, unbalanced_variables))
  formula <- as.formula(paste(outcome, paste(comparison, paste(final_covariates, collapse = " + "), ")")))


  if (preintervention %in% c("q28_12pct", "q18_12pct", "q90_12pct", "qof_percent")) {
    fit <- glm(formula, data = md, family = quasibinomial())
  } else if (preintervention %in% c("total_gp_extg_fte_weighted")) {
    fit <- glm(formula, data = md, family = "Gamma")
  }


  ATT <- avg_comparisons(fit,
    variables = variable_att,
    vcov = ~subclass,
    newdata = subset(treated == 1)
  )


  ATT_tidy <- as.data.frame(ATT) %>%
    mutate(
      out = outcome,
      form = Reduce(paste, deparse(formula))
    ) %>%
    dplyr::select(all_of(columns_to_select)) %>%
    mutate(model = "Missing data analysis")
}

missing_data_results <- mapply(missing_data_model, outcome = outcomes, preintervention = preintervention_outcomes, SIMPLIFY = FALSE)

all_models_missing <- do.call(rbind, missing_data_results)

saveRDS(all_models_missing, "all_models_missing.rds")

### put models together in one dataframe

get_objects <- ls()

models <- str_subset(get_objects, "all_models")
model_list <- mget(models)

sensitivity_analyses <- do.call(rbind, model_list)

saveRDS(sensitivity_analyses, "all_sensitivity_analysis_models.rds")

all_sensitivity <- readRDS("all_sensitivity_analysis_models.rds")

write.csv(all_sensitivity, "all_sensitivity.csv")
