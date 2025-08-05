## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: 0_prereqs.R

# Corresponding author: Emma Whitfield (emma.whitfield@health.org.uk)

# Description:
# Extracts QOF scores for achievement and six relevant conditions

# Dependencies:
# 0_prereqs.R

# Inputs:
# public data on QOF scores

# Outputs:
# Dataset of QOF scores

# Notes:
## ========================================================================== ##

library(tidyverse)
library(rvest) # to pull from urls
library(xml2)
library(glue)

source("0_prereqs.R")

# 1. Useful functions ----
# pass in link to file for download
# if link is to a zip file pass in csv_name (or part of csv_name) from folder
pull_data_qof <- function(year, csv_name = "") {
  if (year == 2018) {
    URL <- "https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-prevalence-and-exceptions-data/2018-19-pas"
  } else {
    URL <- glue("https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-prevalence-and-exceptions-data/{year}-{year-1999}")
  }

  pg <- read_html(URL)
  links <- html_attr(html_nodes(pg, "a"), "href")

  # get link with raw data
  link <- links[str_detect(links, ".zip")]
  if (substr(link, nchar(link) - 3, nchar(link)) == ".zip") {
    temp <- tempfile()
    download.file(link, temp)

    if (!str_detect(csv_name, ".csv")) {
      # filename might vary year to year
      # go fishing for file containing csv_name
      poss_files <- zip::zip_list(temp)
      csv_name <- poss_files$filename[str_detect(poss_files$filename, csv_name)]
    }

    file <- read.csv(unz(temp, csv_name))
    unlink(temp)
  } else {
    file <- ""
  }
  return(file)
}

QOF <- tibble()

for (y in (2017:2023)) {
  if (y >= 2022) {
    annual_achievement <- tibble()
    for (place in c("EAST_OF_ENGLAND", "LONDON", "MIDLANDS", "NORTH_EAST", "NORTH_WEST", "SOUTH_EAST", "SOUTH_WEST")) {
      annual_achievement <- annual_achievement %>%
        rbind(pull_data_qof(y, str_glue("ACHIEVEMENT_{place}")))
    }
  } else {
    annual_achievement <- pull_data_qof(y, "ACHIEVEMENT")
  }

  annual_achievement <- annual_achievement %>%
    filter(MEASURE == "ACHIEVED_POINTS") %>%
    group_by(PRACTICE_CODE) %>%
    summarise(ACHIEVEMENT = sum(VALUE))

  # also collect prevalence data for six conditions
  annual_prevalence <- pull_data_qof(y, "PREVALENCE")
  group_var <- names(annual_prevalence)[str_detect(names(annual_prevalence), "GROUP")]
  list_var <- names(annual_prevalence)[str_detect(names(annual_prevalence), "LIST_SIZE")]
  annual_prevalence <- annual_prevalence %>%
    filter(!!as.name(group_var) %in% c("AST", "CKD", "COPD", "CHD", "DM", "HYP")) %>%
    mutate(PREV = REGISTER / (!!as.name(list_var))) %>%
    select(PRACTICE_CODE, !!group_var, PREV) %>%
    pivot_wider(names_from = !!group_var, values_from = PREV)

  # some practices do not have achievement data
  annual_QOF <- annual_achievement %>%
    left_join(annual_prevalence, by = join_by(PRACTICE_CODE)) %>%
    mutate(year = y)
  QOF <- QOF %>%
    rbind(annual_QOF)
}

# save
write_csv(QOF, glue("{results_folder}/QOF_summary_{analysis_dt}.csv"))
