## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: preamble.R

# Corresponding author:Sarah Opie-Martin (sarah.opie-martin@health.org.uk)

# Description:
# Set up working directory and import packages

# Notes:
## ========================================================================== ##

# clean data.R

library(tidyverse)
library(scales)
library(lubridate)

# matching_and_estimation
library(MatchIt)
library(Matching)
library(cobalt)
library(purrr)
library(marginaleffects)
library(sandwich)
library(broom)

# table 1.Rmd and table S1
library(flextable)
library(table1)

# crude_comparisons.R
library(cowplot)

# sensitivity.R
library(stringr)

# main_model.R
library(forestplot)


setwd("your working directory")
