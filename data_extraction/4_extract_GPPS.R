## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: 4_extract_GPPS.R

# Corresponding author: Emma Whitfield (emma.whitfield@health.org.uk)

# Description:
# Extracts GPPS data

# Dependencies:
# 0_prereqs.R

# Inputs:
# public data on GPPS outcomes

# Outputs:
# Dataset of GPPS outcomes

# Notes:
## ========================================================================== ##

library(tidyverse)
library(rvest)
library(xml2)
library(janitor)
library(glue)

source("0_prereqs.R")

cols_to_include <- c(
  "practice_code", "distributed", "received", "resprate", "q18_12pct",
  "q28_12pct", "q90_12pct", "gpcontactoverall_1_pct", "lastgpapptneeds_1_pct", "overallexp_1_pct"
)

# Check that these vars are comparable YoY
vars_allyrs <- tibble()
for (y in 2018:2024) {
  variables <- read_csv(glue("{data_folder}/GPPS_{y}_List_of_reporting_variables_(csv)_PUBLIC.csv")) %>%
    mutate(Variable = str_to_lower(Variable)) %>%
    filter(Variable %in% cols_to_include)

  vars_allyrs <- vars_allyrs %>%
    rbind(variables %>%
      select(Variable, Description) %>%
      mutate(year = y))
}

# Iterate through GP-level patient survey results and select relevant questions
final_df <- tibble()
for (y in 2018:2023) {
  df <- read_csv(glue("{data_folder}/GPPS_{y}_Practice_data_(weighted)_(csv)_PUBLIC.csv")) %>%
    clean_names() %>%
    select(any_of(cols_to_include))

  # if any columns are missing add and fill with na
  for (c in cols_to_include[!(cols_to_include %in% names(df))]) {
    df[[c]] <- NA
  }

  final_df <- final_df %>%
    rbind(df %>%
      mutate(year = y))
}

# add 2024 data manually as uses different variable names
df <- read_csv(glue("{data_folder}/GPPS_2024_Practice_data_(weighted)_(csv)_PUBLIC.csv"))

df <- df %>%
  clean_names() %>%
  rename("practice_code" = ad_practicecode) %>%
  select(any_of(c(cols_to_include, "gpcontactoverall_1_pct", "gpcontactoverall_2_pct", "lastgpapptneeds_1_pct", "lastgpapptneeds_2_pct", "overallexp_1_pct", "overallexp_2_pct"))) %>%
  mutate(
    q18_12pct = gpcontactoverall_1_pct + gpcontactoverall_2_pct,
    q28_12pct = overallexp_1_pct + overallexp_2_pct,
    q90_12pct = lastgpapptneeds_1_pct + lastgpapptneeds_2_pct
  ) %>%
  select(-c(gpcontactoverall_2_pct, lastgpapptneeds_2_pct, overallexp_2_pct))

# if any columns are missing add and fill with na
for (c in cols_to_include[!(cols_to_include %in% names(df))]) {
  df[[c]] <- NA
}

df <- df %>%
  mutate(year = 2024)

final_df <- rbind(final_df, df)

# save
write_csv(final_df, glue("{results_folder}/GPPS_responses_{analysis_dt}.csv"))
