## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: 2_extract_practice_characteristics.R

# Corresponding author: Emma Whitfield (emma.whitfield@health.org.uk)

# Description:
# Extracts and calculates practice characteristics

# Dependencies:
# 0_prereqs.R, 1_extract_IMD.R

# Inputs:
# public data on practice and area characteristics
# LSOA IMD scores based on 2021 LSOAs

# Outputs:
# Dataset of practice characteristics including
# Practice open date, close date
# List size
# Rurality - determined from 2011 rural urban classification on LSOAs
# Registered patient demographics - % male, % age 65+
# Area demographics - %male, %age >65, %white, %english as second language
# Practice IMD score

# Notes:
## ========================================================================== ##

library(tidyverse)
library(rvest) # to pull monthly GP number urls
library(xml2)
library(glue)

source("0_prereqs.R")

# 1. Useful functions ----
pull_data <- function(link, csv_name = "", sheet = 6) {
  if (substr(link, nchar(link) - 3, nchar(link)) == ".zip") {
    temp <- tempfile()
    download.file(link, temp)
    file <- read.csv(unz(temp, csv_name))
    unlink(temp)
  } else if (substr(link, nchar(link) - 3, nchar(link)) == ".csv") {
    file <- read_csv(link)
  } else if (substr(link, nchar(link) - 4, nchar(link)) == ".xlsx") {
    file <- openxlsx::read.xlsx(link, sheet = sheet)
  }
  return(file)
}

# 2. Practice demographics ----
# Get basic practice details - https://digital.nhs.uk/services/organisation-data-service/export-data-files/csv-downloads/gp-and-gp-practice-related-data
# don't think there are any practices missing data in epraccur so epracarc not needed
# maximum close date in epracarc is 31/3/2013 so no relevant practices
epraccur <- read_csv(glue("{data_folder}/epraccur.csv"))

epraccur <- epraccur %>%
  select(-c(`...19`, `...20`, `...21`, `...23`, PRESCRIBING_SETTING)) %>%
  rename(PRESCRIBING_SETTING = `...26`)

epraccur <- epraccur %>%
  filter(str_like(PRESCRIBING_SETTING, "4%")) %>% # select GP practices only
  select(PRACTICE_CODE, OPEN_DT, CLOSE_DT) %>%
  mutate(
    OPEN_DT = lubridate::ymd(OPEN_DT),
    CLOSE_DT = lubridate::ymd(CLOSE_DT)
  )

# Check how many practices are included
map_dates <- tibble(map_date = seq(ymd("2018-01-01"), ymd("2025-01-01"), by = "months"))
open_pracs <- epraccur %>%
  full_join(map_dates, by = join_by(OPEN_DT <= map_date)) %>%
  filter(is.na(CLOSE_DT) | (CLOSE_DT > map_date)) %>%
  group_by(map_date) %>%
  summarise(n.pracs = n())
# numbers look fairly reasonable

# pull monthly list size/%male/%over 65 data from https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice
clean_data <- tibble()
for (m in str_to_lower(month.name)) {
  q <- switch(m,
    "january" = 1,
    "february" = 1,
    "march" = 1,
    "april" = 2,
    "may" = 2,
    "june" = 2,
    "july" = 3,
    "august" = 3,
    "september" = 3,
    "october" = 4,
    "november" = 4,
    "december" = 4
  )
  for (y in 2018:2024) {
    # pull monthly list size data
    if ((m == "april") & (y == 2018)) {
      URL <- "https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/patients-registered-at-a-gp-practice-april-2018-special-topic---registered-patients-compared-to-the-projected-resident-population-in-england"
    } else {
      URL <- str_glue("https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/{m}-{y}")
    }
    pg <- read_html(URL)
    links <- html_attr(html_nodes(pg, "a"), "href")

    link <- links[str_detect(links, "prac-all")]
    list_size <- pull_data(link, "gp-reg-pat-prac-all.csv")

    # pull male numbers + ages
    link <- links[str_detect(links, "age-male")]
    male_nums <- pull_data(link, "gp-reg-pat-prac-sing-age-male.csv")

    age_nums_m <- male_nums %>%
      filter(AGE != "ALL") %>%
      mutate(
        AGE = ifelse(AGE == "95+", "95", AGE),
        AGE = as.numeric(AGE),
        NUMBER65 = if_else(AGE > 65, NUMBER_OF_PATIENTS, 0)
      ) %>%
      group_by(ORG_CODE) %>%
      summarise(NUMBER_M_65 = sum(NUMBER65))

    male_nums <- male_nums %>%
      filter(AGE == "ALL") %>%
      select(ORG_CODE, NUMBER_OF_PATIENTS) %>%
      rename(NUMBER_OF_MALES = NUMBER_OF_PATIENTS)

    list_size <- list_size %>%
      left_join(male_nums, by = join_by(CODE == ORG_CODE)) %>%
      mutate(
        NUMBER_OF_MALES = replace_na(NUMBER_OF_MALES, 0),
        PERC_MALE = 100 * NUMBER_OF_MALES / NUMBER_OF_PATIENTS
      )

    # pull female ages
    link <- links[str_detect(links, "age-female")]
    female_nums <- pull_data(link, "gp-reg-pat-prac-sing-age-female.csv")

    age_nums_f <- female_nums %>%
      filter(AGE != "ALL") %>%
      mutate(
        AGE = ifelse(AGE == "95+", "95", AGE),
        AGE = as.numeric(AGE),
        NUMBER65 = if_else(AGE > 65, NUMBER_OF_PATIENTS, 0)
      ) %>%
      group_by(ORG_CODE) %>%
      summarise(NUMBER_F_65 = sum(NUMBER65))

    # combine age numbers to calculate percentage
    list_size <- list_size %>%
      left_join(age_nums_m, by = join_by(CODE == ORG_CODE)) %>%
      left_join(age_nums_f, by = join_by(CODE == ORG_CODE)) %>% # replace_na doesn't work as expected with native pipe
      replace_na(list(NUMBER_M_65 = 0, NUMBER_F_65 = 0)) %>%
      mutate(PERC_65 = 100 * (NUMBER_M_65 + NUMBER_F_65) / NUMBER_OF_PATIENTS)

    # clean up and append to final dataframe
    rm(age_nums_f, age_nums_m, female_nums, male_nums)
    if (nrow(list_size)) {
      list_size <- list_size %>%
        select(EXTRACT_DATE, CODE, NUMBER_OF_PATIENTS, PERC_MALE, PERC_65) %>%
        rename(PRACTICE_CODE = CODE) %>%
        mutate(quarter = str_glue("{q}Q{str_remove(y, '20')}")) # useful for joining later on
    }

    clean_data <- clean_data %>%
      rbind(list_size)
  }
}
# NB will throw error on last it as dec 2024 data doesn't exist yet
# clean up
rm(list_size, pg, link, links, m, URL, y, q)

# add basic practice details in
clean_data <- clean_data %>%
  left_join(epraccur, by = join_by(PRACTICE_CODE)) %>%
  mutate(CLEAN_DATE = lubridate::parse_date_time(str_to_upper(EXTRACT_DATE), c("dmy", "ymd")))

# save
write_csv(clean_data, glue("{results_folder}/GP_demographics_monthly_{analysis_dt}.csv"))

# 3. Area demographics ----
# Get area demographics from ONS mid-year estimates - https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimatesnationalstatistics
# latest data is 2022 mid-year estimates - with 2021 LSOA codes
# apply estimates from 1st July in relevant year to 1st June in next year
# at each mid-year point estimate %male and %65+ in each LSOA and use to compute weighted average for GP pop

# First pull all population demographics data
# This has 2021 LSOAs
pop_lsoa_data <- tibble()
URL <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimatesnationalstatistics/mid2011tomid2022/sapelsoabroadage20112022.xlsx"
for (y in 2017:2022) {
  pop_dem <- pull_data(URL, sheet = y - 2006)
  names(pop_dem) <- pop_dem[3, ]
  pop_dem <- pop_dem[-(1:3), ] %>%
    janitor::clean_names() %>%
    rename(LSOA21CD = lsoa_2021_code) %>%
    mutate(across(total:m65_and_over, as.numeric))

  pop_dem <- pop_dem %>%
    # calculate male total
    mutate(M_total = m0_to_15 + m16_to_29 + m30_to_44 + m45_to_64 + m65_and_over) %>%
    mutate(
      age_nums = m65_and_over + f65_and_over,
      POP_65 = 100 * age_nums / total
    ) %>%
    mutate(POP_MALE = 100 * M_total / total) %>%
    select(LSOA21CD, POP_65, POP_MALE, total) %>%
    mutate(year = y)

  pop_lsoa_data <- pop_lsoa_data %>%
    rbind(pop_dem)
}
rm(pop_dem)

# 2021 IMD
# built in 1_extract_IMD.R
weighted_imd <- read_csv(glue("{results_folder}/weighted_imd_{analysis_dt}.csv"))

# 2021 Census ethnicity data (TS021) downloaded from nomis www.nomisweb.co.uk
# Assume using 2021 LSOAs
ethnicity_dt <- read_csv(glue("{data_folder}/census2021_ts021.csv"), skip = 6) %>%
  rename(
    LSOA21CD = mnemonic,
    POP_WHITE = White
  ) %>%
  select(LSOA21CD, POP_WHITE)

# 2021 Census household language data
# Assume using 2021 LSOAs
language_dt <- read_csv(glue("{data_folder}/census2021_ts029.csv"), skip = 6) %>%
  janitor::clean_names() %>%
  rename(LSOA21CD = mnemonic) %>%
  mutate(
    POP_ENG2 = 100 - main_language_is_english_english_or_welsh_in_wales,
    POP_NOENG = main_language_is_not_english_english_or_welsh_in_wales_cannot_speak_english_well + main_language_is_not_english_english_or_welsh_in_wales_cannot_speak_english
  ) %>%
  select(LSOA21CD, POP_ENG2, POP_NOENG)

# get map from 2011 to 2021 LSOA codes - SOM code
# Import the CSV
mapping <- read_csv(glue("{data_folder}/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Exact_Fit_Lookup_for_EW_(V3).csv"))

# only need to calculate a weight if the LSOA CHNGID M or X
weighted_mapping <- mapping %>%
  group_by(LSOA21CD) %>%
  mutate(WEIGHT = 1 / n()) %>%
  ungroup()

# And prepare rurality data - https://geoportal.statistics.gov.uk/datasets/803b5eba7f6f4c998b7d2c5be6729693_0/explore
ruc <- read_csv(glue("{data_folder}/RUC_2011.csv"))
# Rurality uses 2011 LSOAs so need to use mapping to convert to 2021 LSOAs
# if 2021 LSOA was merger of any rural 2011 LSOA with any other LSOA then call it rural
weighted_ruc <- ruc %>%
  mutate(
    rural = str_detect(RUC11, "Rural"),
    rural = ifelse(rural, 1, 0)
  ) %>%
  left_join(weighted_mapping, by = join_by(LSOA11CD)) %>%
  mutate(weighted_rural = WEIGHT * rural) %>%
  group_by(LSOA21CD) %>%
  summarise(rural = ceiling(sum(weighted_rural)))
rm(ruc)

# Now have rurality, IMD, and population characteristics of every 2021 LSOA

# Get numbers of patients per LSOA for each practice from quarterly LSOA data - these use 2011 LSOAs
# Assume that where 2011 LSOAs are mapped to multiple 2021 LSOAs patients are equally distributed amongst them

# Then calculate practice-level characteristics of rurality/IMD/pop demographics by weighting by number of patients in each 2021 LSOA

# practices only appear to contribute quarterly LSOA data in first full quarter after they start reporting list size
practice_pop_dem <- tibble()
for (m in c("january", "april", "july", "october")) {
  q <- switch(m,
    "january" = 1,
    "april" = 2,
    "july" = 3,
    "october" = 4
  )
  for (y in 2018:2024) {
    # Get numbers of patients
    if ((m == "april") & (y == 2018)) {
      URL <- "https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/patients-registered-at-a-gp-practice-april-2018-special-topic---registered-patients-compared-to-the-projected-resident-population-in-england"
    } else {
      URL <- str_glue("https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/{m}-{y}")
    }
    pg <- read_html(URL)
    links <- html_attr(html_nodes(pg, "a"), "href")

    if ((m == "july") & (y == 2018)) {
      link <- "https://files.digital.nhs.uk/D2/1D35F3/gp-reg-pat-prac-lsoa-all-females-males-jul-18.zip"
    } else {
      link <- links[str_detect(links, "lsoa")]
    }

    if (length(link) > 1) {
      link <- link[str_detect(link, "2011")]
    }

    if ((m == "october") & (y == 2018)) {
      csv_name <- "gp-reg-pat-prac-lsoa-all-oct-18.csv"
    } else if ((m == "july") & (y == 2024)) {
      csv_name <- "gp-reg-pat-prac-lsoa-all-2011.csv"
    } else {
      csv_name <- "gp-reg-pat-prac-lsoa-all.csv"
    }

    lsoa_dt <- pull_data(link, csv_name)

    if (!("Number.of.Patients" %in% names(lsoa_dt))) {
      lsoa_dt <- lsoa_dt %>%
        rename(Number.of.Patients = NUMBER_OF_PATIENTS)
    }

    # join to map to get 2021 LSOAs
    weighted_lsoa <- lsoa_dt %>%
      left_join(weighted_mapping,
        by = join_by(LSOA_CODE == LSOA11CD),
        relationship = "many-to-many"
      ) %>%
      mutate(weighted_npats = Number.of.Patients * WEIGHT) %>%
      select(LSOA21CD, PRACTICE_CODE, weighted_npats) %>%
      filter(!is.na(weighted_npats))

    # Add population demographics
    # make sure to join to relevant period of pop demographics data
    pop_year <- y
    if (m %in% c("january", "april") & (y <= 2022)) {
      pop_year <- pop_year - 1
    } else if (y > 2022) {
      pop_year <- 2022
    }
    weighted_lsoa <- weighted_lsoa %>%
      left_join(pop_lsoa_data %>%
        filter(year == pop_year) %>%
        select(LSOA21CD, POP_65, POP_MALE), by = join_by(LSOA21CD))

    weighted_lsoa <- weighted_lsoa %>%
      left_join(ethnicity_dt, by = join_by(LSOA21CD))

    weighted_lsoa <- weighted_lsoa %>%
      left_join(language_dt, by = join_by(LSOA21CD))

    # add in rurality data here
    weighted_lsoa <- weighted_lsoa %>%
      left_join(weighted_ruc, by = join_by(LSOA21CD))

    # add IMD data
    weighted_lsoa <- weighted_lsoa %>%
      left_join(weighted_imd %>% select(LSOA_code, IMD_score),
        by = join_by(LSOA21CD == LSOA_code)
      )

    # now group by practice and compute weighted average of these demographics
    # N.B. There are some practices which have patients coming from Wales
    # we don't have IMD data for these patients so just exclude from that calculation
    weighted_lsoa <- weighted_lsoa %>%
      group_by(PRACTICE_CODE) %>%
      summarise(
        POP_MALE = sum(POP_MALE * weighted_npats) / sum(weighted_npats),
        POP_65 = sum(POP_65 * weighted_npats) / sum(weighted_npats),
        POP_WHITE = sum(POP_WHITE * weighted_npats) / sum(weighted_npats),
        POP_ENG2 = sum(POP_ENG2 * weighted_npats) / sum(weighted_npats),
        POP_NOENG = sum(POP_NOENG * weighted_npats) / sum(weighted_npats),
        POP_RURAL = 100 * sum(rural * weighted_npats) / sum(weighted_npats),
        IMD_score = sum(IMD_score * weighted_npats, na.rm = T) / (sum(weighted_npats, na.rm = T) - sum(is.na(IMD_score)))
      ) %>%
      mutate(quarter = str_glue("{q}Q{str_remove(y, '20')}"))

    practice_pop_dem <- practice_pop_dem %>%
      rbind(weighted_lsoa)
  }
}

# clean up and save
rm(pg, lsoa_dt, pop_lsoa_data, csv_name, link, links, pop_year, URL)

write_csv(practice_pop_dem, glue("{results_folder}/population_demographics_quarterly_{analysis_dt}.csv"))
