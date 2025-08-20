## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: sii_analysis.R

# Corresponding author: Emma Whitfield (emma.whitfield@health.org.uk)

# Description:
# Calculate point estimate and confidence interval of slope index for relevant outcomes in LLR, comparable ICSs, and at a national level

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
  # removed code relating to confidence intervals and removed transform argument as always wants
  # equivalent of transform = T

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
  # shouldn't hit this error as have checked for bad ICSs before running function
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
                          ifelse(value_type == 2, 
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

plot_outcomes <- function(df, time_var1, outcome_var1, subtitle, transform = F, transform_f = function(x) {x}) {
  # plot to check linearity across quantiles
  # N.B. plot does not show CIs - errorbars for proportions may cross 1

  p <- df %>%
    group_by(.data[[time_var1]], clean_quintile) %>%
    summarise(
      mean.out = mean(.data[[outcome_var1]]),
      sd.out = sd(.data[[outcome_var1]]),
      n.pracs = n_distinct(PRACTICE_CODE)
    ) %>%
    mutate(
      low.var = mean.out - sd.out,
      up.var = mean.out + sd.out,
      ridits = FindXValues(n.pracs, 5)
    )

  title <- glue("{outcome_var1} - untransformed")

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

    p <- p %>% mutate(
      mean.out = transform_f(mean.out),
      low.var = transform_f(low.var),
      up.var = transform_f(up.var)
    )
    title <- glue("{outcome_var1} - transformed")
  }

  p %>%
    ungroup() %>%
    ggplot(aes(ridits, mean.out)) +
    facet_wrap(.data[[time_var1]] ~ .) +
    geom_point() +
    geom_errorbar(aes(ymin = low.var, ymax = up.var)) +
    geom_line(data = . %>% filter(clean_quintile %in% c(1, 5)), linetype = "dashed", colour = "red") +
    theme_bw() +
    labs(
      title = title,
      subtitle = subtitle
    )
}

# 2. Apply to data ----
set.seed(177)

td <- today() # define for consistent filenaming when running over several days

clean_df <- readRDS("cleaned_data/complete_cleaned.rds")

# rescale qof_percent to 0 to 1
clean_df <- clean_df %>%
  mutate(qof_percent = qof_percent / 100)

# set up sinkfile for output
sink(glue("{sinkfile}/{td}/bootstrapping_sii.txt"))

outcomes <- c("total_gp_extg_fte_weighted", "total_nurses_fte_weighted", "total_admin_fte_weighted", "q18_12pct", "q28_12pct", "q90_12pct", "qof_percent")
ics <- c("QK1", "QJ2", "QOQ", "QT1", "QHM", "QWO", "QF7", "QE1", "QWU", "QJM", "QNC")

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

  # remove any rows with missing data
  # and as have 1 row per practice per month restrict to distinct rows for given outcome/time_var
  sub_df <- clean_df %>%
    filter(
      !is.na(clean_quintile),
      !is.na(.data[[outcome]])
    ) %>%
    select(PRACTICE_CODE, icb_code, {{ time_var }}, clean_quintile, {{ outcome }}) %>%
    distinct()

  # check for small ICS (< 5 practice in quintile at any time point)
  small_ics <- sub_df %>%
    group_by({{ time_var }}, icb_code, clean_quintile) %>%
    filter(n_distinct(PRACTICE_CODE) < 5) %>%
    ungroup() %>%
    distinct(icb_code) %>%
    pull()

  small_ics <- intersect(small_ics, ics)
  if (length(small_ics)) {
    cat(glue('The following ICSs contain insufficient practices in at least one quantile during the time series and will be excluded from analysis of this outcome: {glue_collapse(small_ics, sep = ", ")}'))
    remaining_ics <- setdiff(ics, small_ics)
  } else {
    remaining_ics <- ics
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

  # define boot_wrapper function for specific outcome to aid boot.ci function
  # in line with boot documentation first two arguments must be data and indices
  boot_wrapper <- function(data, indices) {
    # wrapper function for bootstrapping
    selected <- data[indices, ] %>%
      group_by(clean_quintile) %>%
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

    n_quintiles <- selected %>%
      select(clean_quintile) %>%
      n_distinct()

    if (n_quintiles != 5) {
      warning("Incorrect number of quintiles - SII not calculated")
    } else {
      return(sii(selected, quantile = clean_quintile, population = n.pracs, value = mean.out, value_type = val_type)$sii)
    }
  }

  res <- tibble()

  ## ICS ----
  # loop through ICSs
  for (i in remaining_ics) {
    cat("\n")
    cat(glue("{i}"))
    cat("\n")

    ics_df <- sub_df %>%
      filter(icb_code == i) %>%
      select(-icb_code)

    # plots for checking
    cat("Plotting untransformed outcomes\n")
    p <- plot_outcomes(ics_df, time_var, outcome, i)
    ggsave(glue("{sinkfile}/{td}/untransformed_{outcome}_{i}_{td}.png"), p, width = 7, height = 7)

    if (val_type > 0) {
      cat("Plotting transformed outcomes\n")
      q <- plot_outcomes(ics_df, time_var, outcome, i, transform = T, transform_f)
      ggsave(glue("{sinkfile}/{td}/transformed_{outcome}_{i}_{td}.png"), q, width = 7, height = 7)
    }

    sp_ics <- split(ics_df, ics_df[time_var])
    cat("Running bootstrapping\n")
    system.time(bootstrap_ics <- lapply(sp_ics, function(x) myboot(x, boot_wrapper, 1000, "clean_quintile")))

    # save bootstrap output
    saveRDS(bootstrap_ics, glue("{resfile}/bootstraps/{i}_{outcome}_{td}.rds"))
    cat("Bootstrapping complete - outputs saved\n")
    cat("Processing results\n")

    # process results
    for (j in 1:length(bootstrap_ics)) {
      current_time <- names(bootstrap_ics)[j]
      t0 <- bootstrap_ics[[j]]$t0

      if (is.na(t0)) {
        lci <- NA
        uci <- NA
      } else {
        lims <- boot.ci(bootstrap_ics[[j]], type = "bca")$bca
        lci <- lims[4]
        uci <- lims[5]
      }

      res <- res %>%
        rbind(tibble(time_var = current_time, t0 = t0, lci = lci, uci = uci, loc = i))
    }
  }

  ## National ----
  # Now repeat at national level
  cat("\n")
  cat(glue("National\n"))
  cat("\n")
  nat_df <- sub_df %>%
    select(-icb_code)

  # plots for checking
  cat("Plotting untransformed outcomes\n")
  p <- plot_outcomes(nat_df, time_var, outcome, "National")
  ggsave(glue("{sinkfile}/{td}/untransformed_{outcome}_national_{td}.png"), p, width = 7, height = 7)

  if (val_type > 0) {
    cat("Plotting transformed outcomes\n")
    q <- plot_outcomes(nat_df, time_var, outcome, "National", transform = T, transform_f)
    ggsave(glue("{sinkfile}/{td}/transformed_{outcome}_national_{td}.png"), q, width = 7, height = 7)
  }

  sp_nat <- split(nat_df, nat_df[time_var])
  cat("Running bootstrapping\n")
  system.time(bootstrap_nat <- lapply(sp_nat, function(x) myboot(x, boot_wrapper, 1000, "clean_quintile")))

  # save bootstrap output
  saveRDS(bootstrap_nat, glue("{resfile}/bootstraps/national_{outcome}_bootstrap_{td}.rds"))

  cat("Bootstrapping complete - outputs saved\n")
  cat("Processing results\n")

  # process results
  for (j in 1:length(bootstrap_nat)) {
    time_var <- names(bootstrap_nat)[j]
    t0 <- bootstrap_nat[[j]]$t0

    if (is.na(t0)) {
      lci <- NA
      uci <- NA
    } else {
      lims <- boot.ci(bootstrap_nat[[j]], type = "bca")$bca
      lci <- lims[4]
      uci <- lims[5]
    }

    res <- res %>%
      rbind(tibble(time_var = time_var, t0 = t0, lci = lci, uci = uci, loc = "National"))
  }

  # save processed results
  saveRDS(res, glue("{resfile}/processed_results/{outcome}_{td}.rds"))

  cat(glue("Finished {outcome}"))
  cat("\n")
}
sink()
