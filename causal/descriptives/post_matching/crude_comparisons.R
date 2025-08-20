## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: crude_comparisons.R

# Corresponding author:Sarah Opie-Martin (sarah.opie-martin@health.org.uk)

# Description:
# Create figure shoing crude comparisons of outcome measures between treated
# and matched control groups - creates figure 2

## ========================================================================== ##

source("~/casual/preamble.R")

### read in match data object from matching_and_estmation.R
matched_data <- readRDS("matched_data_noPMS.rds")

preintervention_outcomes <- c("q18_12pct", "q28_12pct", "q90_12pct", "qof_percent", "total_gp_extg_fte_weighted", "total_nurses_fte_weighted", "total_admin_fte_weighted")

branding_colours <- c("#EB003B", "#189EDC")

# Changes here due to going from bool to numeric
matched_ids <- matched_data %>%
  mutate(status = case_when(
    treated == 1 ~ "Treated",
    TRUE ~ "Matched control"
  )) %>%
  dplyr::select(PRACTICE_CODE, status)

clean_df <- readRDS("cleaned_data/complete_cleaned.rds")

# rescale qof_percent to 0 to 1
clean_df <- clean_df %>%
  mutate(qof_percent = qof_percent / 100)

matched_practices_only <- clean_df %>%
  left_join(matched_ids, by = "PRACTICE_CODE") %>%
  filter((treated == 1 & status == "Treated") | (treated == 0 & status == "Matched control")) %>%
  dplyr::select(all_of(preintervention_outcomes), status, map_date) %>%
  pivot_longer(-c(status, map_date), names_to = "outcome", values_to = "value")


matched_practices_gpps <- matched_practices_only %>%
  mutate(survey_month = month(map_date)) %>%
  filter(outcome %in% c("q28_12pct", "q18_12pct", "q90_12pct")) %>%
  filter(survey_month == 3) %>%
  mutate(survey_year = paste0(year(map_date))) %>%
  group_by(outcome, status, survey_year) %>%
  summarise(
    mean_value = mean(value, na.rm = T),
    standard_deviation = sd(value, na.rm = T)
  )

gpps <- ggplot(data = matched_practices_gpps, aes(x = survey_year, group = status, col = status)) +
  geom_line(data = matched_practices_gpps %>% filter(survey_year < 2024), aes(y = mean_value, col = status)) +
  geom_vline(xintercept = "2022", lty = "dotted", color = "#666666") +
  geom_vline(xintercept = "2024", lty = "dashed", color = "#666666") +
  geom_point(aes(col = status, y = mean_value)) +
  # geom_errorbar(aes(y = mean_value, ymin = mean_value - standard_deviation, ymax = mean_value + standard_deviation, group = status), alpha = 0.2) +
  scale_color_manual(values = branding_colours) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0.25, 1)) +
  labs(x = "Survey year", y = "Mean percentage good overall", col = "") +
  theme_classic() +
  theme(text = element_text(size = 12), legend.position = "none") +
  facet_wrap(. ~ outcome, labeller = labeller(outcome = c("q18_12pct" = "Experience contacting practice", "q28_12pct" = "Overall experience of GP practice", "q90_12pct" = "Needs met during appointment")))

matched_practices_qof <- matched_practices_only %>%
  mutate(survey_month = month(map_date)) %>%
  filter(outcome %in% c("qof_percent")) %>%
  filter(survey_month == 3) %>%
  mutate(survey_year = paste0(year(map_date))) %>%
  group_by(outcome, status, survey_year) %>%
  summarise(
    mean_value = mean(value, na.rm = T),
    standard_deviation = sd(value, na.rm = T)
  )

qof <- ggplot(data = matched_practices_qof, aes(x = survey_year, y = mean_value, group = status, col = status)) +
  geom_line(aes(y = mean_value, col = status)) +
  geom_vline(xintercept = "2019", lty = "dotted", color = "#666666") +
  geom_vline(xintercept = "2023", lty = "dashed", color = "#666666") +
  geom_point(aes(col = status, y = mean_value)) +
  # geom_errorbar(aes(y = mean_value, ymin = mean_value - standard_deviation, ymax = mean_value + standard_deviation, group = status), alpha = 0.2) +
  scale_color_manual(values = branding_colours) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0.75, 1)) +
  labs(x = "QOF year", y = "Mean percentage achievement", col = "") +
  theme_classic() +
  theme(text = element_text(size = 12)) +
  facet_wrap(. ~ outcome, labeller = labeller(outcome = c("qof_percent" = "Percentage of total QOF points achieved")))


matched_practices_staff <- matched_practices_only %>%
  filter(outcome %in% c("total_gp_extg_fte_weighted", "total_nurses_fte_weighted", "total_admin_fte_weighted")) %>%
  group_by(outcome, status, map_date) %>%
  summarise(
    mean_value = mean(value, na.rm = T),
    standard_deviation = sd(value, na.rm = T)
  )

staff <- ggplot(data = matched_practices_staff, aes(x = map_date, y = mean_value, group = status, col = status)) +
  geom_line(aes(y = mean_value, col = status)) +
  geom_vline(xintercept = as.Date("2021-06-01"), lty = "dotted", color = "#666666") +
  geom_vline(xintercept = as.Date("2023-04-01"), lty = "dashed", color = "#666666") +
  geom_point(aes(col = status, y = mean_value)) +
  # geom_errorbar(aes(y = mean_value, ymin = mean_value - standard_deviation, ymax = mean_value + standard_deviation, group = status), alpha = 0.2) +
  scale_color_manual(values = branding_colours) +
  labs(x = "Year", y = "Mean FTE staff/ 1000 weighted patients", col = "") +
  theme_classic() +
  theme(text = element_text(size = 12), legend.position = "none") +
  facet_wrap(. ~ outcome, labeller = labeller(outcome = c("total_gp_extg_fte_weighted" = "GPs excluding training grade", "total_nurses_fte_weighted" = "Nurses", "total_admin_fte_weighted" = "Admin")))

tiff("crude_comparisons.tiff", units = "in", width = 10, height = 10, res = 300)
plot_grid(gpps, qof, staff, nrow = 3)

dev.off()
