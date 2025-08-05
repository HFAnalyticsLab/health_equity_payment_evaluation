## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: 7_combine_all.R

# Corresponding author: Emma Whitfield (emma.whitfield@health.org.uk)

# Description:
# Links all datasets produced in previous scripts

# Dependencies:
# 0_prereqs.R

# Inputs:
# outputs of scripts 2 - 6

# Outputs:
# Combined dataset of practice variables

# Notes:
## ========================================================================== ##

library(tidyverse)
library(glue)

source("0_prereqs.R")

# read in all dataframes
GP_dem_monthly <- read_csv(glue("{results_folder}/GP_demographics_monthly_{analysis_dt}.csv"))
pop_dem_quarterly <- read_csv(glue("{results_folder}/population_demographics_quarterly_{analysis_dt}.csv"))
GP_workforce_monthly <- read_csv(glue("{results_folder}/GP_workforce_{analysis_dt}.csv"))
QOF_yearly <- read_csv(glue("{results_folder}/QOF_summary_{analysis_dt}.csv"))
GPPS_responses <- read_csv(glue("{results_folder}/GPPS_responses_{analysis_dt}.csv"))
payments <- read_csv(glue("{results_folder}/GP_payments_{analysis_dt}.csv"))

# create master file by joining all datasets

# 1. Demograhics ----
# make sure to include row for every possible month between open date and close date
GP_valid_dates <- GP_dem_monthly %>%
  select(PRACTICE_CODE, OPEN_DT, CLOSE_DT) %>%
  distinct() %>%
  full_join(tibble(map_date = seq(ymd("2018-01-01"), ymd("2024-11-01"), "months")),
    by = join_by(OPEN_DT <= map_date)
  ) %>%
  filter(is.na(CLOSE_DT) | CLOSE_DT >= map_date)

combined_demographics <- GP_valid_dates %>%
  left_join(GP_dem_monthly %>% select(PRACTICE_CODE, NUMBER_OF_PATIENTS, PERC_MALE, PERC_65, quarter, CLEAN_DATE),
    by = join_by(PRACTICE_CODE, map_date == CLEAN_DATE)
  ) %>%
  left_join(pop_dem_quarterly,
    by = join_by(PRACTICE_CODE, quarter)
  )

rm(GP_valid_dates, GP_dem_monthly, pop_dem_quarterly)

# 2. Workforce ----
# Need to roll initial quarterly data over to get monthly from start to finish
GP_workforce_monthly <- GP_workforce_monthly %>%
  mutate(
    map_date = date + days(1),
    map_quarter = glue("{(2 + month(map_date)) %/% 3}Q{str_remove(year(map_date), '20')}")
  )

GP_workforce_quarterly <- GP_workforce_monthly %>%
  filter(map_date < ymd("2021-07-01"))

GP_workforce_monthly <- GP_workforce_monthly %>%
  filter(map_date >= ymd("2021-07-01"))

dates_to_rep <- tibble(map_date = seq(ymd("2018-01-01"), ymd("2021-06-01"), "months")) %>%
  mutate(map_quarter = glue("{(2 + month(map_date)) %/% 3}Q{str_remove(year(map_date), '20')}"))

GP_workforce_quarterly <- GP_workforce_quarterly %>%
  select(-c(map_date)) %>%
  left_join(dates_to_rep, by = join_by(map_quarter), relationship = "many-to-many")

GP_workforce_monthly <- GP_workforce_monthly %>%
  rbind(GP_workforce_quarterly)

# add workforce to demographics
dem_work <- combined_demographics %>%
  mutate(map_quarter = glue("{(2 + month(map_date)) %/% 3}Q{str_remove(year(map_date), '20')}")) %>%
  left_join(GP_workforce_monthly,
    by = join_by(PRACTICE_CODE == prac_code, map_date)
  )

rm(dates_to_rep, GP_workforce_monthly, GP_workforce_quarterly, combined_workforce)

# 3. GPPS ----
dem_work <- dem_work %>%
  mutate(survey_year = case_when(
    month(map_date) <= 3 ~ year(map_date),
    T ~ year(map_date) + 1
  ))

# now combine with other data
dem_work_survey <- dem_work %>%
  left_join(GPPS_responses,
    by = join_by(PRACTICE_CODE == practice_code, survey_year == year)
  )

rm(GPPS_responses)

# 4. QOF ----
dem_work_survey <- dem_work_survey %>%
  mutate(qof_year = case_when(
    month(map_date) > 3 ~ year(map_date),
    T ~ year(map_date) - 1
  ))

dem_work_survey_qof <- dem_work_survey %>%
  left_join(QOF_yearly,
    by = join_by(PRACTICE_CODE, qof_year == year)
  )

rm(QOF_yearly)

# 5. GP payments ----
dem_work_survey_qof <- dem_work_survey_qof %>%
  mutate(pay_year = case_when(
    month(map_date) <= 3 ~ year(map_date) - 1,
    T ~ year(map_date)
  ))

final_combined <- dem_work_survey_qof %>%
  left_join(payments, by = join_by(PRACTICE_CODE == prac_code, pay_year))

# Tidy and save ----
# icb_map taken from https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/november-2024#resources 'Mapping' file
icb_map <- read_csv(glue("{data_folder}/gp_icb_map_nov24.csv"))
final_combined <- final_combined %>%
  left_join(icb_map %>% select(PRACTICE_CODE, ICB_CODE, SUB_ICB_LOCATION_CODE) %>% rename(ICB24 = ICB_CODE, SUBICB24 = SUB_ICB_LOCATION_CODE),
    by = join_by(PRACTICE_CODE)
  )

final_combined <- final_combined %>%
  group_by(PRACTICE_CODE) %>%
  arrange(desc(map_date)) %>%
  mutate(
    icb_code = case_when(
      !is.na(ICB24) ~ ICB24,
      T ~ icb_code
    ),
    sub_icb_code = case_when(
      !is.na(SUBICB24) ~ SUBICB24,
      T ~ sub_icb_code
    )
  ) %>%
  fill(icb_code, sub_icb_code, .direction = "downup") %>%
  ungroup() %>%
  select(-c(ICB24, SUBICB24))

# check practices missing ICBs
missing_icbs <- final_combined %>% filter(is.na(icb_code))

# how many did not close before July 2022 or have > 500 patients
missing_icbs %>%
  filter(
    is.na(CLOSE_DT) | (CLOSE_DT > ymd("2022-07-01")),
    NUMBER_OF_PATIENTS > 500
  )
# no practices - so these will all be cleaned out later

# check practices with multiple ICBs
multi_ICBs <- final_combined %>%
  group_by(PRACTICE_CODE) %>%
  mutate(n.icbs = sum(n_distinct(icb_code, na.rm = T))) %>%
  filter(n.icbs > 1)
# no practices

# remove extraneous variables
final_combined <- final_combined %>%
  select(-c(quarter))

write_csv(final_combined, glue("{results_folder}/final_public_combined_{analysis_dt}.csv"))
