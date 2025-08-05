## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: 6_extract_payments_data

# Corresponding author: Emma Whitfield (emma.whitfield@health.org.uk)

# Description:
# Extracts data on primary care funding

# Dependencies:
# 0_prereqs.R

# Inputs:
# public data on primary care funding

# Outputs:
# Dataset of primary care funding information

# Notes:
## ========================================================================== ##

library(tidyverse)
library(rvest) # to pull monthly GP number urls
library(glue)

source("0_prereqs.R")

# 1. Extract annual payments data ----

cols_to_include <- c("prac_code", "contract_type", "avg_registered", "avg_weighted", "avg_pay_registered", "avg_pay_weighted", "covid_immun_pay", "covid_support_pay", "covid_long_pay")
cols_to_rename <- c(
  "prac_code" = "practice_code",
  "avg_registered" = "average_number_of_registered_patients",
  "avg_registered" = "number_of_registered_patients_last_known_figure",
  "avg_weighted" = "average_number_of_weighted_patients",
  "avg_weighted" = "number_of_weighted_patients_last_known_figure",
  "avg_pay_registered" = "average_payments_per_registered_patient",
  "avg_pay_weighted" = "average_payments_per_weighted_patient",
  "covid_immun_pay" = "covid_immunisation",
  "covid_support_pay" = "covid_support_and_expansion",
  "covid_long_pay" = "long_covid"
)

payments_df <- tibble(prac_code = c(), contract_type = c(), avg_registered = c(), avg_weighted = c(), avg_pay_registered = c(), avg_pay_weighted = c(), covid_immun_pay = c(), covid_support_pay = c(), covid_long_pay = c())
for (y in (18:22)) {
  URL <- glue("https://digital.nhs.uk/data-and-information/publications/statistical/nhs-payments-to-general-practice/england-20{y}-{y+1}")
  pg <- read_html(URL)
  links <- html_attr(html_nodes(pg, "a"), "href")

  link <- links[str_detect(links, "-csv")]

  if (length(link) > 1) {
    link <- link[str_detect(link, "prac")]
  }
  data <- read_csv(link)

  # in some years variable names have trailing numbers - need removing and making consistent
  names(data) <- gsub("[0-9]$", "", names(data))
  data <- data %>%
    janitor::clean_names()


  data <- data %>%
    rename(any_of(cols_to_rename)) %>%
    select(any_of(cols_to_include)) %>%
    mutate(pay_year = 2000 + y)

  if (y == 20) {
    payments_df <- payments_df %>%
      mutate(
        covid_immun_pay = NA,
        covid_support_pay = NA,
        covid_long_pay = NA
      )

    data <- data %>%
      mutate(covid_long_pay = NA)
  }
  payments_df <- payments_df %>%
    rbind(data)
}

payments_df <- payments_df %>%
  mutate(
    avg_pay_registered = as.numeric(avg_pay_registered),
    avg_pay_weighted = as.numeric(avg_pay_weighted)
  )

# save and upload
write_csv(payments_df, glue("{results_folder}/GP_payments_{analysis_dt}.csv"))
