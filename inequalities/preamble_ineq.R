## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: preamble_ineq.R

# Corresponding author: Emma Whitfield (emma.whitfield@health.org.uk)

# Description:
# Set up working directory and import packages

# Notes:
## ========================================================================== ##

# clean data.R

library(tidyverse)
library(scales)
library(lubridate)

# sii_analysis.R / LLR_SII_analysis.R

library(boot)
library(glue)

# plot_results.R

library(patchwork)
library(gtsummary)

setwd("your working directory")

resfile <- "directory to save results"
sinkfile <- "directory to save logfiles"