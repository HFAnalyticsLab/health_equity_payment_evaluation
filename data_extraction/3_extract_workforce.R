## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: 3_extract_workforce.R

# Corresponding author: Emma Whitfield (emma.whitfield@health.org.uk)

# Description:
# Extracts practice staffing data

# Dependencies:
# 0_prereqs.R

# Inputs:
# public data on primary care staffing

# Outputs:
# Dataset of primary care staffing levels for GPs, nurses, and administrative
# staff

# Notes:
## ========================================================================== ##

library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(zoo)
library(janitor)
library(rvest)
library(glue)

source("0_prereqs.R")

# 1. Useful functions ----

pull_data_workforce <- function(month, year) {
  d <- switch(month,
    "january" = 31,
    "february" = 28,
    "march" = 31,
    "april" = 30,
    "may" = 31,
    "june" = 30,
    "july" = 31,
    "august" = 31,
    "september" = 30,
    "october" = 31,
    "november" = 30,
    "december" = 31
  )

  if ((y == 2024) & (m == "february")) {
    # leap year
    d <- 29
  }

  # Import the CSV
  URL <- str_glue("https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services/{d}-{month}-{year}")
  pg <- read_html(URL)
  links <- html_attr(html_nodes(pg, "a"), "href")

  link <- links[str_detect(links, "PracticeCSV")]

  if (year >= 2023) {
    csv_name <- "Practice Level - Detailed"
  } else {
    csv_name <- "General Practice –"
  }

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
  } else if (substr(link, nchar(link) - 3, nchar(link)) == ".csv") {
    file <- read_csv(link)
  } else {
    file <- ""
  }
  return(file)
}


# 2. Scraping data ----
# empty list to store imported datasets
# snapshot relates to last calendar day of each month
datasets <- list()
cols_to_include <- c("prac_code", "sub_icb_code", "icb_code", "extracted_date", "total_gp_extg_fte", "total_gp_sen_ptnr_hc", "total_gp_ptnr_prov_hc", "total_nurses_fte", "total_admin_fte", "gp_source", "nurse_source", "admin_source")

# first iterate from August 2022 to August 2024
for (m in str_to_lower(month.name)) {
  for (y in 2022:2024) {
    
    if ((y == 2022) & (m %in% str_to_lower(month.name)[1:7])) {
      # skip
      next
    } else if ((y == 2024) & (m %in% str_to_lower(month.name)[11:12])) {
      next
    }
    
    extracted_date <- str_glue("{m}_{y}")
    data <- pull_data_workforce(m, y)

    # Add a column with the extracted date & select only relevant variables
    data <- data %>%
      mutate(extracted_date = extracted_date) %>%
      clean_names() %>%
      select(any_of(cols_to_include)) %>%
      mutate(across(starts_with("total"), as.double))

    if (!("icb_code" %in% names(data))) {
      data <- data %>%
        mutate(
          sub_icb_code = NA,
          icb_code = NA
        )
    }

    # Append the dataset to the list
    datasets[[extracted_date]] <- data
  }
}


# 3. Add remaining data ----
# then get all remaining data from July 2022 zip files
# Downloaded from https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services/31-july-2022

# data for earlier periods all comes from Dec 2021 page
csv_paths <- list.files(workforce_folder, ".csv", full.names = T)

# Loop through each file, import it, and extract the date from its file name
for (file in csv_paths) {
  # Extract the filename without the path
  file_name <- basename(file)

  # Extract the date part (e.g., April 2024) and convert to "April_2024" format
  extracted_date <- str_to_lower(str_extract(file_name, "\\b[A-Za-z]+\\s\\d{4}\\b")) %>%
    str_replace_all(" ", "_")

  # Import the CSV and add a column with the extracted date & select only relevant variables
  data <- read_csv(file) %>%
    mutate(extracted_date = extracted_date) %>%
    clean_names() %>%
    select(any_of(cols_to_include)) %>%
    mutate(across(starts_with("total"), as.double))

  # Append the dataset to the list
  datasets[[file_name]] <- data
}

# extract all the colnames from list of datasets - can also check if there's differences here
colnames_list <- lapply(datasets, colnames)

# Iterate through each of the datasets and check if there are any differences between any pairwise combinations
pairwise_differences <- combn(seq_along(colnames_list), 2, function(indices) {
  i <- indices[1]
  j <- indices[2]

  if (!identical(colnames_list[[i]], colnames_list[[j]])) {
    list(
      dataset_1 = names(datasets)[i],
      dataset_2 = names(datasets)[j],
      columns_in_dataset_1 = colnames_list[[i]],
      columns_in_dataset_2 = colnames_list[[j]]
    )
  } else {
    NULL
  }
}, simplify = FALSE)

# Filter out NULL (no differences)
pairwise_differences <- pairwise_differences[!sapply(pairwise_differences, is.null)]

# Combine all datasets into one
final_dataset <- bind_rows(datasets)

# format date properly (uses zoo package)
# EKW - these are snapshot data from last day of month need mapping
final_dataset$date <- lubridate::parse_date_time(final_dataset$extracted_date, orders = c("%B_%Y")) + months(1) - days(1)

final_dataset <- final_dataset %>%
  mutate(
    gp_source = str_to_sentence(gp_source),
    nurse_source = str_to_sentence(nurse_source),
    admin_source = str_to_sentence(admin_source)
  )

# save
write_csv(final_dataset, glue("{results_folder}/GP_workforce_{analysis_dt}.csv"))
