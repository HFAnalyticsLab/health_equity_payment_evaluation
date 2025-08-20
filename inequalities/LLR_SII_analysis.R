## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: LLR_sii_analysis.R

# Corresponding author: Emma Whitfield (emma.whitfield@health.org.uk)

# Description:
# Calculate point estimate and confidence interval of slope index for relevant outcomes in LLR only

# Dependencies:
#

# Inputs:
# cleaned public data

# Outputs:
# time series of SI estimates

# Notes:
## ========================================================================== ##

source("~/iaheq1/inequalities/preamble_ineq.R")

# 1. Useful functions ----

FindXValues <- function(xvals, no_quantiles) {
  # from PHEindicatormethods package
  df <- data.frame(
    prop = numeric(no_quantiles), cumprop = numeric(no_quantiles),
    output = numeric(no_quantiles)
  )
  df$prop <- xvals / sum(xvals, na.rm = TRUE)
  df$cumprop <- cumsum(df$prop)
  df$output <- ifelse(is.na(lag(df$cumprop, 1)), df$prop / 2,
    df$prop / 2 + lag(df$cumprop, 1)
  )
  FindXValues <- df$output
}

# function to prevent NANs from division by 0
myeps <- function(x) {
  if (x == 1) {
    cat("----- mean of 1 - myeps used -----\n")
    return(x - .Machine$double.eps)
  } else {
    return(x)
  }
}

sii <- function(data, quantile, population, value, value_type = 0) {
  # simplified version of PHEindicatormethods phe_sii function
  # check for missing data
  if (missing(data) | missing(quantile) | missing(population)) {
    stop("function phe_sii requires the arguments: data, quantile, population")
  }
  if (missing(value)) {
    stop("function phe_sii requires value field")
  }
  if (!(value_type %in% c(0, 1, 2))) {
    stop("value_type should be 0, 1 or 2")
  }

  quantile <- enquo(quantile)
  population <- enquo(population)
  if (!missing(value)) {
    value <- enquo(value)
  }

  if (!((class(pull(data, {{ population }})) %in% c("numeric", "integer")) & (class(pull(data, {{ value }})) %in% c("numeric", "integer")))) {
    stop("some input fields in data.frame are non-numeric")
  }

  negative_pops <- data %>%
    filter({{ population }} <= 0 | is.na({{ population }}))
  if (nrow(negative_pops) > 0) {
    stop("some groups have a zero, negative or missing population")
  }

  if (value_type == 2) {
    if (rlang::quo_text(value) %in% names(data)) {
      invalid_prop <- data %>%
        filter({{ value }} < 0 | {{ value }} > 1)
      if (nrow(invalid_prop) > 0) {
        stop("value proportions are not all between 0 and 1")
      }
    }
  }

  grouping_variables <- group_vars(data)
  data <- data %>%
    ungroup() %>%
    mutate(across(where(is.factor), as.character)) %>%
    group_by(!!!syms(c(grouping_variables)))
  quantile_list <- unique(select(ungroup(data), {{ quantile }}))
  no_quantiles <- nrow(quantile_list)
  if (no_quantiles < 5 | no_quantiles > 100) {
    stop("Number of quantiles must be between 5 and 100")
  } else if (no_quantiles > 10) {
    warning("WARNING: Small values can make SII unstable when using a large number of quantiles")
  }

  valid_areas <- data %>%
    filter({{ population }} > 0) %>%
    summarise(n = length(unique({{ quantile }}))) %>%
    filter(n == no_quantiles)
  valid_deciles <- valid_areas %>%
    merge(quantile_list, all.x = TRUE, all.y = TRUE)
  if (nrow(valid_deciles) != nrow(data)) {
    warning("WARNING: some records have been removed due to incomplete or invalid data")
  }

  pops_prep <- valid_deciles %>%
    left_join(data, by = c(grouping_variables, rlang::quo_text(quantile))) %>%
    group_by(!!!syms(c(grouping_variables, rlang::quo_text(quantile)))) %>%
    arrange(!!!syms(c(grouping_variables, rlang::quo_text(quantile))))

  if (rlang::quo_text(value) %in% names(pops_prep)) {
    pops_prep <- pops_prep %>%
      mutate(value = {{ value }})
  }

  pops_prep <- pops_prep %>%
    rowwise() %>%
    mutate(value = ifelse(value_type == 1, 
                          log(.data$value),
                          felse(value_type == 2, 
                                log(.data$value / (1 - .data$value)), 
                                .data$value)
    )) %>%
    ungroup()

  pops_prep_ab <- pops_prep %>%
    group_by(!!!syms(grouping_variables)) %>%
    mutate(
      a_vals = {{ population }} / sum({{ population }}),
      b_vals = FindXValues({{ population }}, no_quantiles)
    )

  pops_prep_ab <- pops_prep_ab %>%
    group_by(!!!syms(c(grouping_variables, rlang::quo_text(quantile)))) %>%
    mutate(
      sqrt_a = sqrt(.data$a_vals),
      b_sqrt_a = .data$b_vals * .data$sqrt_a,
      yvals = .data$sqrt_a * .data$value
    )

  if (length(grouping_variables) == 0) {
    popsSII_model <- pops_prep_ab %>%
      tidyr::nest(data = everything())
  } else {
    popsSII_model <- pops_prep_ab %>%
      group_by(!!!syms(grouping_variables)) %>%
      tidyr::nest()
  }

  popsSII_model <- popsSII_model %>%
    mutate(model = purrr::map(data, function(df) stats::lm(yvals ~ sqrt_a + b_sqrt_a - 1, data = df))) %>%
    mutate(model = purrr::map(.data$model, broom::tidy)) %>%
    tidyr::unnest("model") %>%
    select(!c("std.error", "statistic", "p.value")) %>%
    tidyr::pivot_wider(names_from = "term", values_from = "estimate")

  popsSII_model <- popsSII_model %>%
    mutate(
      sii = .data$b_sqrt_a,
      intercept = .data$sqrt_a
    ) %>%
    select(all_of(grouping_variables), "sii", "intercept")
  if (value_type == 1) {
    popsSII_model <- popsSII_model %>%
      mutate(
        xequals1 = .data$intercept + .data$sii,
        antilogintercept = exp(.data$intercept),
        antilogxequals1 = exp(.data$xequals1),
        sii = (.data$antilogxequals1 - .data$antilogintercept)
      )
  } else if (value_type == 2) {
    popsSII_model <- popsSII_model %>%
      mutate(
        xequals1 = .data$intercept + .data$sii,
        antilogintercept = exp(.data$intercept) / (1 + exp(.data$intercept)),
        antilogxequals1 = exp(.data$xequals1) / (1 + exp(.data$xequals1)),
        sii = (.data$antilogxequals1 - .data$antilogintercept)
      )
  }

  popsSII_model <- popsSII_model %>%
    mutate(
      indicator_type = if_else(value_type == 0, 
                               "normal",
                               if_else(value_type == 1, 
                                       "rate", 
                                       "proportion")
      ),
      transform = if_else(value_type == 1, 
                          "log",
                          if_else(value_type == 2, 
                                  "logit", 
                                  "none")
      )
    )

  return(popsSII_model)
}

# define wrapper to facilitate use of lapply in combo with strata
myboot <- function(x, f, R1, strata1) {
  boot(x, f, R = R1, strata = x %>% pull({{ strata1 }}))
}

plot_outcomes <- function(df, time_var1, outcome_var1, quantile_var1, transform = F, transform_f = function(x) {x}) {
  if (quantile_var1 == "amount") {
    no_quantiles <- 8
  } else {
    no_quantiles <- 5
  }

  p <- df %>%
    group_by(.data[[time_var1]], .data[[quantile_var1]]) %>%
    summarise(
      mean.out = mean(.data[[outcome_var1]]),
      sd.out = sd(.data[[outcome_var1]]),
      n.pracs = n_distinct(PRACTICE_CODE)
    ) %>%
    mutate(
      low.var = mean.out - sd.out,
      up.var = mean.out + sd.out,
      ridits = FindXValues(n.pracs, no_quantiles)
    )

  title <- glue("{outcome_var1} - untransformed ({quantile_var1})")

  if (transform) {
    if (outcome_var1 %in% c("q18_12pct", "q28_12pct", "q90_12pct", "qof_percent")) {
      # check for 1s to prevent dividing by zero
      p <- p %>%
        rowwise() %>%
        mutate(
          mean.out = myeps(mean.out),
          low.var = myeps(low.var),
          up.var = myeps(up.var)
        ) %>%
        ungroup()
    }

    p <- p %>% 
      mutate(
        mean.out = transform_f(mean.out),
        low.var = transform_f(low.var),
        up.var = transform_f(up.var)
      )
    title <- glue("{outcome_var1} - transformed ({quantile_var1})")
  }

  if (quantile_var1 == "amount") {
    min_quant <- 1
    max_quant <- 8
  } else {
    min_quant <- 1
    max_quant <- 5
  }

  p %>%
    ungroup() %>%
    ggplot(aes(ridits, mean.out)) +
    facet_wrap(.data[[time_var1]] ~ .) +
    geom_point() +
    geom_errorbar(aes(ymin = low.var, ymax = up.var)) +
    geom_line(data = . %>% filter(.data[[quantile_var1]] %in% c(min_quant, max_quant)), linetype = "dashed", colour = "red") +
    theme_bw() +
    labs(
      title = title,
      subtitle = "Spread is +- 1 SD"
    )
}

# 2. Apply to data ----
set.seed(177)

td <- today() # define for consistent filenaming when running over several days

clean_df <- readRDS("cleaned_data/complete_cleaned.rds")

LLR_payments <- readRDS("cleaned_data/practice_payments_cleaned.rds")
payment_key <- readRDS("cleaned_data/payment_key.rds")

# set up sinkfile for output
sink(glue("{sinkfile}/LLR_only/LLR_bootstrapping_sii_{td}.txt"))
outcomes <- c("total_gp_extg_fte_weighted", "total_nurses_fte_weighted", "total_admin_fte_weighted", "q18_12pct", "q28_12pct", "q90_12pct", "qof_percent")

# and restrict to LLR only
LLR_df <- clean_df %>%
  filter(icb_code == "QK1")

# add LLR payment levels from first round of payments
LLR_df <- LLR_df %>%
  left_join(LLR_payments %>% select(Practice.Code, Amount_21_22), by = join_by(PRACTICE_CODE == Practice.Code)) %>%
  rename(amount = Amount_21_22) %>%
  select(PRACTICE_CODE, map_date, survey_year, qof_year, IMD_score, amount, q18_12pct, q28_12pct, q90_12pct, qof_percent, total_gp_extg_fte_weighted, total_nurses_fte_weighted, total_admin_fte_weighted)

# calculate LLR quintiles
LLR_fixed_quintiles <- LLR_df %>%
  filter(map_date == ymd("2021-06-01")) %>%
  mutate(LLR_quintile = 6 - ntile(IMD_score, 5)) %>%
  dplyr::select(PRACTICE_CODE, LLR_quintile)

LLR_df <- LLR_df %>%
  left_join(LLR_fixed_quintiles, by = join_by("PRACTICE_CODE"), relationship = "many-to-one")

# rescale qof_percent to 0 to 1
LLR_df <- LLR_df %>%
  mutate(qof_percent = qof_percent / 100)

cat(glue('Running SII analysis for the following outcomes: {glue_collapse(outcomes, sep = ", ")}\n'))
cat("\n")

for (outcome in outcomes) {
  cat("*****************************************\n")
  cat(glue("{str_to_upper(outcome)}\n"))

  # determine time variable to be used
  if (outcome %in% c("total_gp_extg_fte_weighted", "total_nurses_fte_weighted", "total_admin_fte_weighted")) {
    time_var <- "map_date"
  } else if (outcome %in% c("q18_12pct", "q28_12pct", "q90_12pct")) {
    time_var <- "survey_year"
  } else if (outcome == "qof_percent") {
    time_var <- "qof_year"
  }

  # determine transformation to be applied
  if (outcome %in% c("total_gp_extg_fte_weighted", "total_nurses_fte_weighted", "total_admin_fte_weighted")) {
    val_type <- 1
    transform_f <- function(x) log(x)
  } else if (outcome %in% c("q18_12pct", "q28_12pct", "q90_12pct", "qof_percent")) {
    val_type <- 2
    transform_f <- function(x) log(x / (1 - x))
  } else {
    val_type <- 0
    transform_f <- function(x) x
  }

  # Start with LLR quintiles
  cat("\n")
  cat(glue("LLR quintiles\n"))
  cat("\n")

  sub_df <- LLR_df %>%
    filter(
      !is.na(LLR_quintile),
      !is.na(.data[[outcome]])
    ) %>%
    select(PRACTICE_CODE, {{ time_var }}, LLR_quintile, {{ outcome }}) %>%
    distinct()

  # check for small data (< 5 practice in quintile at any time point)
  small_df <- sub_df %>%
    group_by({{ time_var }}, LLR_quintile) %>%
    filter(n_distinct(PRACTICE_CODE) < 5) %>%
    ungroup()

  if (nrow(small_df)) {
    cat(glue("At least one quantile during the time series has fewer than 5 practices - analysis will not be carried out"))
    break
  }

  # define boot_wrapper function for specific outcome to aid boot.ci function
  boot_wrapper_quintile <- function(data, indices) {
    # wrapper function for bootstrapping
    selected <- data[indices, ] %>%
      group_by(LLR_quintile) %>%
      summarise(
        mean.out = mean(.data[[outcome]]),
        n.pracs = n()
      ) %>%
      ungroup()

    if (outcome %in% c("q18_12pct", "q28_12pct", "q90_12pct", "qof_percent")) {
      selected <- selected %>%
        rowwise() %>%
        mutate(mean.out = case_when(
          mean.out == 1 ~ myeps(mean.out),
          T ~ mean.out
        ))
    }

    return(sii(selected, quantile = LLR_quintile, population = n.pracs, value = mean.out, value_type = val_type)$sii)
  }

  res_quintile <- tibble()
  # plots for checking
  cat("Plotting untransformed outcomes\n")
  p <- plot_outcomes(sub_df, time_var, outcome, "LLR_quintile")
  ggsave(glue("{sinkfile}/LLR_only/untransformed_{outcome}_LLR_quintile_{td}.png"), p, width = 7, height = 7)

  if (val_type > 0) {
    cat("Plotting transformed outcomes\n")
    q <- plot_outcomes(sub_df, time_var, outcome, "LLR_quintile", transform = T, transform_f)
    ggsave(glue("{sinkfile}/LLR_only/transformed_{outcome}_LLR_quintile_{td}.png"), q, width = 7, height = 7)
  }

  sp_LLR <- split(sub_df, sub_df[time_var])
  cat("Running bootstrapping\n")
  system.time(bootstrap_llr <- lapply(sp_LLR, function(x) myboot(x, boot_wrapper_quintile, 1000, "LLR_quintile")))

  # save bootstrap output
  saveRDS(bootstrap_llr, glue("{resfile}/bootstraps/LLR_only/LLR_quintile_{outcome}_{td}.rds"))
  cat("Bootstrapping complete - outputs saved\n")
  cat("Processing results\n")

  # process results
  for (j in 1:length(bootstrap_llr)) {
    current_time <- names(bootstrap_llr)[j]
    t0 <- bootstrap_llr[[j]]$t0

    if (is.na(t0)) {
      lci <- NA
      uci <- NA
    } else {
      lims <- boot.ci(bootstrap_llr[[j]], type = "bca")$bca
      lci <- lims[4]
      uci <- lims[5]
    }

    res_quintile <- res_quintile %>%
      rbind(tibble(time_var = current_time, t0 = t0, lci = lci, uci = uci))
  }

  # save processed results
  saveRDS(res_quintile, glue("{resfile}/processed_results/LLR_only/LLR_quintile_{outcome}_{td}.rds"))

  # Now repeat for HEP amounts
  cat("\n")
  cat(glue("HEP amount\n"))
  cat("\n")

  sub_df <- LLR_df %>%
    filter(
      !is.na(amount),
      !is.na(.data[[outcome]])
    ) %>%
    select(PRACTICE_CODE, {{ time_var }}, amount, {{ outcome }}) %>%
    distinct()

  # check for small data (< 5 practice in quintile at any time point)
  small_df <- sub_df %>%
    group_by({{ time_var }}, amount) %>%
    filter(n_distinct(PRACTICE_CODE) < 5) %>%
    ungroup()

  if (nrow(small_df)) {
    cat(glue("At least one quantile during the time series has fewer than 5 practices - analysis will not be carried out"))
    break
  }

  # define boot_wrapper function for specific outcome to aid boot.ci function
  boot_wrapper_amount <- function(data, indices) {
    # wrapper function for bootstrapping
    selected <- data[indices, ] %>%
      group_by(amount) %>%
      summarise(
        mean.out = mean(.data[[outcome]]),
        n.pracs = n()
      ) %>%
      ungroup()

    if (outcome %in% c("q18_12pct", "q28_12pct", "q90_12pct", "qof_percent")) {
      selected <- selected %>%
        rowwise() %>%
        mutate(mean.out = case_when(
          mean.out == 1 ~ myeps(mean.out),
          T ~ mean.out
        ))
    }

    return(sii(selected, quantile = amount, population = n.pracs, value = mean.out, value_type = val_type)$sii)
  }

  res_amount <- tibble()

  # plots for checking
  cat("Plotting untransformed outcomes\n")
  p <- plot_outcomes(sub_df, time_var, outcome, "amount")
  ggsave(glue("{sinkfile}/LLR_only/untransformed_{outcome}_amount_{td}.png"), p, width = 7, height = 7)

  if (val_type > 0) {
    cat("Plotting transformed outcomes\n")
    q <- plot_outcomes(sub_df, time_var, outcome, "amount", transform = T, transform_f)
    ggsave(glue("{sinkfile}/LLR_only/transformed_{outcome}_amount_{td}.png"), q, width = 7, height = 7)
  }

  sp_LLR <- split(sub_df, sub_df[time_var])
  cat("Running bootstrapping\n")
  system.time(bootstrap_amount <- lapply(sp_LLR, function(x) myboot(x, boot_wrapper_amount, 1000, "amount")))

  # save bootstrap output
  saveRDS(bootstrap_amount, glue("{resfile}/bootstraps/LLR_only/amount_{outcome}_{td}.rds"))

  cat("Bootstrapping complete - outputs saved\n")
  cat("Processing results\n")

  # process results
  for (j in 1:length(bootstrap_amount)) {
    current_time <- names(bootstrap_amount)[j]
    t0 <- bootstrap_amount[[j]]$t0

    if (is.na(t0)) {
      lci <- NA
      uci <- NA
    } else {
      lims <- boot.ci(bootstrap_amount[[j]], type = "bca")$bca
      lci <- lims[4]
      uci <- lims[5]
    }

    res_amount <- res_amount %>%
      rbind(tibble(time_var = current_time, t0 = t0, lci = lci, uci = uci))
  }

  # save processed results
  saveRDS(res_amount, glue("{resfile}/processed_results/LLR_only/amount_{outcome}_{td}.rds"))
  cat(str_glue("Finished {outcome}"))
  cat("\n")
}
sink()
