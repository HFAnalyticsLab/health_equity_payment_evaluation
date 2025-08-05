## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: 1_extract_IMD.R

# Corresponding author: Emma Whitfield (emma.whitfield@health.org.uk)

# Description:
# Produces IMD scores for 2021 LSOAs

# Dependencies:
# 0_prereqs.R

# Inputs:
# public data on IMD scores and LSOAs

# Outputs:
# LSOA IMD scores based on 2021 LSOAs

# Notes:
## ========================================================================== ##

library(tidyverse)
library(rvest)
library(xml2)
library(glue)

source("0_prereqs.R")

# 1. Pull + prep IMD data ----
# https://www.ons.gov.uk/peoplepopulationandcommunity/personalandhouseholdfinances/incomeandwealth/datasets/mappingincomedeprivationatalocalauthoritylevel
imd <- read_csv(glue("{data_folder}/ons_lsoa_imd_19.csv"))
imd <- imd %>%
  rename(
    LSOA_CODE = `LSOA code (2011)`,
    IMD_score = `Overall Index of Multiple Deprivation (IMD) Score`
  ) %>%
  select(LSOA_CODE, IMD_score)

# reweight IMD for 2021 LSOA codes
# mapping from https://geoportal.statistics.gov.uk/datasets/ons::lsoa-2011-to-lsoa-2021-to-local-authority-district-2022-best-fit-lookup-for-ew-v2/about
# There are four designated categories to describe the changes, and these are as follows:
# U - No Change from 2011 to 2021. This means that direct comparisons can be made between these 2011 and 2021 LSOA.
# S - Split. This means that the 2011 LSOA has been split into two or more 2021 LSOA.
#     There will be one record for each of the 2021 LSOA that the 2011 LSOA has been split into.
#     This means direct comparisons can be made between estimates for the single 2011 LSOA and the estimates from the aggregated 2021 LSOA.
# M - Merged. 2011 LSOA have been merged with another one or more 2011 LSOA to form a single 2021 LSOA.
#     This means direct comparisons can be made between the aggregated 2011 LSOAs’ estimates and the single 2021 LSOA’s estimates.
# X - The relationship between 2011 and 2021 LSOA is irregular and fragmented.
#     This has occurred where 2011 LSOA have been redesigned because of local authority district boundary changes, or to improve their social homogeneity.
#     These can’t be easily mapped to equivalent 2021 LSOA like the regular splits (S) and merges (M), and therefore like for like comparisons of estimates for 2011 LSOA and 2021 LSOA are not possible.

# Import the CSV
mapping <- read_csv(glue("{data_folder}/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Exact_Fit_Lookup_for_EW_(V3).csv"))

# only need to calculate a weight if the LSOA CHNGID M or X
weighted_mapping <- mapping %>%
  group_by(LSOA21CD) %>%
  mutate(WEIGHT = 1 / n()) %>%
  ungroup()

# join back to IMD and reweight
weighted_imd <- weighted_mapping %>%
  left_join(
    imd %>%
      select(LSOA_CODE, IMD_score),
    by = c("LSOA11CD" = "LSOA_CODE")
  ) %>%
  mutate(weighted_imd = IMD_score * WEIGHT) %>%
  group_by(LSOA21CD) %>%
  summarise(IMD_score = sum(weighted_imd)) %>%
  select(LSOA21CD, IMD_score) %>%
  rename(LSOA_code = LSOA21CD)

# save
write_csv(weighted_imd, glue("{results_folder}/weighted_imd_{analysis_dt}.csv"))
