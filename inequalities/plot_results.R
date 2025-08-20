## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: plot_results.R

# Corresponding author: Emma Whitfield (emma.whitfield@health.org.uk)

# Description:
# Plot results of inequalities analysis

# Dependencies:
#

# Inputs:
# Processed results files from sii_analysis and LLR_sii_analysis

# Outputs:
# plots

# Notes:
## ========================================================================== ##

source("~/iaheq1/inequalities/preamble_ineq.R")

td <- today()
save_fig <- function(filename, p, width = 10) {
  file1 <- str_glue("{resfile}/figures/{td}/{filename}")
  ggsave(file1, p, width = width, height = 8)
}

# 1. National picture ----
plot_national <- function(df, title, x, outcome = "staffing", linebreaks = c(), sz = 0.6) {
  df <- df %>%
    mutate(
      loc_highlight = (loc %in% c("QK1", "National")),
      loc = case_when(
        loc == "QK1" ~ "LLR ICS",
        T ~ loc
      )
    )

  avg_ics <- df %>%
    filter(!loc_highlight) %>%
    group_by(time_var) %>%
    summarise(
      mean.t0 = mean(t0),
      median.t0 = median(t0)
    ) %>%
    ungroup() %>%
    mutate(loc = "Average comparator ICS")

  remaining_df <- df
  remaining_avg <- avg_ics

  dummy_df <- tibble(time_var = c(), t0 = c(), loc = c())
  group.colours <- c("#00BA38", "#4672bd", "#F8766D")
  group.fills <- c(NA, "#4672bd", "#F8766D")
  p <- ggplot(dummy_df, aes(x = time_var, y = t0, group = loc, col = loc)) +
    geom_hline(yintercept = 0, linetype = "dashed", col = "#074089")

  if (ymd("2021-07-01") %in% df$time_var) {
    p <- p +
      geom_vline(xintercept = ymd("2021-07-01"), linetype = "dotted", col = "#A675E7", linewidth = 1)
  } else {
    p <- p +
      geom_vline(xintercept = 2021 + (7 / 12), linetype = "dotted", col = "#A675E7", linewidth = 1)
  }

  if (length(linebreaks)) {
    # iterate through each linebreak plotting all the points up to the next linebreak
    for (b in linebreaks) {
      before_df <- remaining_df %>%
        filter(time_var < b)
      before_avg <- remaining_avg %>%
        filter(time_var < b)

      p <- p +
        geom_point(data = before_df %>% filter(!loc_highlight), col = "gray", alpha = 0.6, size = sz) + # comparator ICSs
        geom_line(data = before_df %>% filter(!loc_highlight), col = "gray", alpha = 0.6) +
        geom_point(data = before_df %>% filter(loc_highlight), size = sz) +
        geom_line(data = before_df %>% filter(loc_highlight))

      # add confidence intervals
      if (outcome == "staffing") {
        p <- p +
          geom_ribbon(data = before_df %>% filter(loc_highlight), aes(ymin = lci, ymax = uci, fill = loc), alpha = 0.2, linetype = 0) +
          geom_ribbon(data = remaining_avg, aes(y = mean.t0, ymin = mean.t0, ymax = mean.t0, fill = loc), alpha = 0.2, linetype = 0, show.legend = F)
      } else {
        p <- p +
          geom_errorbar(data = before_df %>% filter(loc_highlight), aes(ymin = lci, ymax = uci), width = 0.4)
      }

      # add average of comparator ICSs
      p <- p +
        geom_point(data = before_avg, aes(y = mean.t0), size = sz) +
        geom_line(data = before_avg, aes(y = mean.t0))

      remaining_df <- remaining_df %>%
        filter(time_var >= b)
      remaining_avg <- remaining_avg %>%
        filter(time_var >= b)
    }
  }

  # make sure to do last remaining chunk of df
  # or the first if there were no linebreaks
  p <- p +
    geom_point(data = remaining_df %>% filter(!loc_highlight), col = "gray", alpha = 0.6, size = sz) +
    geom_line(data = remaining_df %>% filter(!loc_highlight, !(time_var %in% linebreaks)), col = "gray", alpha = 0.6) +
    geom_point(data = remaining_df %>% filter(loc_highlight), size = sz) +
    geom_line(data = remaining_df %>% filter(loc_highlight, !(time_var %in% linebreaks)))

  if (outcome == "staffing") {
    p <- p +
      geom_ribbon(data = remaining_df %>% filter(loc_highlight), aes(ymin = lci, ymax = uci, fill = loc), alpha = 0.2, linetype = 0, show.legend = F) +
      geom_ribbon(data = remaining_avg, aes(y = mean.t0, ymin = mean.t0, ymax = mean.t0, fill = loc), alpha = 0.2, linetype = 0, show.legend = F) +
      scale_fill_manual(values = group.colours, labels = c("Average comparator ICS", "National", "QK1"), name = "Geography")
  } else {
    p <- p +
      geom_errorbar(data = remaining_df %>% filter(loc_highlight), aes(ymin = lci, ymax = uci), width = 0.4)
  }

  p <- p +
    geom_point(data = remaining_avg, aes(y = mean.t0), size = sz) +
    geom_line(data = remaining_avg %>% filter(!(time_var %in% linebreaks)), aes(y = mean.t0))

  p <- p +
    theme_bw() +
    scale_colour_manual(values = group.colours, name = "Geography") +
    labs(
      title = title,
      x = x,
      y = "SII"
    ) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

  if (outcome == "gpps") {
    xbreaks <- 2018:2024
    xticks <- c("2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")
    p <- p +
      scale_x_continuous(breaks = xbreaks, labels = xticks)
  } else if (outcome == "qof") {
    xbreaks <- 2017:2023
    xticks <- c("2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")
    p <- p +
      scale_x_continuous(breaks = xbreaks, labels = xticks)
  }

  return(p)
}

## a. GPPS ----
# map to percentage scale
gpps_18_res <- readRDS(glue("{resfile}/processed_results/q18_12pct_2025-07-03.rds")) %>%
  mutate(time_var = as.double(time_var)) %>%
  mutate(
    t0 = 100 * t0,
    lci = 100 * lci,
    uci = 100 * uci
  )

p18 <- plot_national(gpps_18_res, "Slope index of inequality for % of 'very good/fairly good' responses to overall experience of making appointment", "Survey year", outcome = "gpps", linebreaks = c(2024))
save_fig("national_q18_12pct_2025-07-03.png", p18)
save_fig("national_q18_12pct_2025-07-03.svg", p18)
# reduce title for subplot
p18 <- plot_national(gpps_18_res, str_wrap("% of 'very good/fairly good' responses to overall experience of making appointment", 35), "Survey year", outcome = "gpps", linebreaks = c(2024))

gpps_28_res <- readRDS(glue("{resfile}/processed_results/q28_12pct_2025-07-03.rds")) %>%
  mutate(time_var = as.double(time_var)) %>%
  mutate(
    t0 = 100 * t0,
    lci = 100 * lci,
    uci = 100 * uci
  )

p28 <- plot_national(gpps_28_res, "Slope index of inequality for % of 'very good/fairly good' responses to overall experience of GP practice", "Survey year", outcome = "gpps", linebreaks = c(2024))
save_fig("national_q28_12pct_2025-07-03.png", p28)
save_fig("national_q28_12pct_2025-07-03.svg", p28)
p28 <- plot_national(gpps_28_res, str_wrap("% of 'very good/fairly good' responses to overall experience of GP practice", 35), "Survey year", outcome = "gpps", linebreaks = c(2024))

gpps_90_res <- readRDS(glue("{resfile}/processed_results/q90_12pct_2025-07-03.rds")) %>%
  mutate(time_var = as.double(time_var)) %>%
  mutate(
    t0 = 100 * t0,
    lci = 100 * lci,
    uci = 100 * uci
  )

p90 <- plot_national(gpps_90_res, "Slope index of inequality for % of 'yes, definitely/yes, to some extent' responses to needs met at last appointment", "Survey year", outcome = "gpps", linebreaks = c(2024))
save_fig("national_q90_12pct_2025-07-03.png", p90)
save_fig("national_q90_12pct_2025-07-03.svg", p90)
p90 <- plot_national(gpps_90_res, str_wrap("% of 'yes, definitely/yes, to some extent' responses to needs met at last appointment", 35), "Survey year", outcome = "gpps", linebreaks = c(2024))

# combine all into one plot
combined <- (p18 + theme(plot.title = element_text(size = 10))) + (p28 + theme(plot.title = element_text(size = 10))) + (p90 + theme(plot.title = element_text(size = 10))) +
  plot_layout(axis_titles = "collect", guides = "collect") +
  plot_annotation(title = "Slope index of inequality for GPPS questions")

# make y scale the same
current_ranges <- c(ggplot_build(combined[[1]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined[[2]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined[[3]])$layout$panel_scales_y[[1]]$range$range)

all_gpps <- combined & ylim(min(current_ranges), max(current_ranges))

save_fig("national_gpps_2025-07-03.png", all_gpps, width = 13)
save_fig("national_gpps_2025-07-03.svg", all_gpps, width = 13)

## b. Staffing ----
gp_res <- readRDS(glue("{resfile}/processed_results/total_gp_extg_fte_weighted_2025-07-03.rds")) %>%
  mutate(time_var = ymd(time_var))
gp <- plot_national(gp_res, "Slope index of inequality for FTE GPs (excluding training) per 1000 weighted patients", "Year", outcome = "staffing")
save_fig("national_total_gp_extg_fte_weighted_2025-07-03.png", gp)
gp <- plot_national(gp_res, "GPs (excluding training)", "Year", outcome = "staffing")

nurse_res <- readRDS(glue("{resfile}/processed_results/total_nurses_fte_weighted_2025-07-03.rds")) %>%
  mutate(time_var = ymd(time_var))
nurse <- plot_national(nurse_res, "Slope index of inequality for FTE nurses per 1000 weighted patients", "Year", outcome = "staffing")
save_fig("national_total_nurses_fte_weighted_2025-07-03.png", nurse)
nurse <- plot_national(nurse_res, "Nurses", "Year", outcome = "staffing")

admin_res <- readRDS(glue("{resfile}/processed_results/total_admin_fte_weighted_2025-07-03.rds")) %>%
  mutate(time_var = ymd(time_var))
admin <- plot_national(admin_res, "Slope index of inequality for FTE administrative staff per 1000 weighted patients", "Year", outcome = "staffing")
save_fig("national_total_admin_fte_weighted_2025-07-03.png", admin)
admin <- plot_national(admin_res, "Administrative staff", "Year", outcome = "staffing")

# combine all into one plot
combined <- (admin + theme(plot.title = element_text(size = 10))) + (gp + theme(plot.title = element_text(size = 10))) + (nurse + theme(plot.title = element_text(size = 10))) +
  plot_layout(axis_titles = "collect", guides = "collect") +
  plot_annotation(title = "Slope index of inequality for FTE staff per 1000 weighted patients")

# make y scale the same
current_ranges <- c(ggplot_build(combined[[1]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined[[2]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined[[3]])$layout$panel_scales_y[[1]]$range$range)

all_staffing <- combined & ylim(min(current_ranges), max(current_ranges))

save_fig("national_staffing_2025-07-03.png", all_staffing, width = 13)

## b2. Staffing - quarterly ----
# staffing data only released quarterly up to July 2021 so restrict to one data point every three months prior to that - take results from first month in each quarter
gp_res_quarter <- (gp_res %>%
  filter(
    time_var < ymd("2021-07-01"),
    month(time_var) %in% c(1, 4, 7, 10)
  )) %>%
  rbind(gp_res %>%
    filter(time_var >= ymd("2021-07-01")))

gp_quarter <- plot_national(gp_res_quarter, "Slope index of inequality for FTE GPs (excluding training) per 1000 weighted patients", "Year", outcome = "staffing")
save_fig("national_total_gp_extg_fte_weighted_quarterly_2025-07-03.png", gp_quarter)
gp_quarter <- plot_national(gp_res_quarter, "GPs (excluding training)", "Year", outcome = "staffing")

nurse_res_quarter <- (nurse_res %>%
  filter(
    time_var < ymd("2021-07-01"),
    month(time_var) %in% c(1, 4, 7, 10)
  )) %>%
  rbind(nurse_res %>%
    filter(time_var >= ymd("2021-07-01")))

nurse_quarter <- plot_national(nurse_res_quarter, "Slope index of inequality for FTE nurses per 1000 weighted patients", "Year", outcome = "staffing")
save_fig("national_total_nurses_fte_weighted_quarterly_2025-07-03.png", nurse_quarter)
nurse_quarter <- plot_national(nurse_res_quarter, "Nurses", "Year", outcome = "staffing")

admin_res_quarter <- (admin_res %>%
  filter(
    time_var < ymd("2021-07-01"),
    month(time_var) %in% c(1, 4, 7, 10)
  )) %>%
  rbind(admin_res %>%
    filter(time_var >= ymd("2021-07-01")))

admin_quarter <- plot_national(admin_res_quarter, "Slope index of inequality for FTE administrative staff per 1000 weighted patients", "Year", outcome = "staffing")
save_fig("national_total_admin_fte_weighted_quarterly_2025-07-03.png", admin_quarter)
admin_quarter <- plot_national(admin_res_quarter, "Administrative staff", "Year", outcome = "staffing")

# combine all into one plot
combined_staffing <- (admin_quarter + theme(plot.title = element_text(size = 10))) + (gp_quarter + theme(plot.title = element_text(size = 10))) + (nurse_quarter + theme(plot.title = element_text(size = 10))) +
  plot_layout(axis_titles = "collect", guides = "collect") +
  plot_annotation(title = "Slope index of inequality for FTE staff per 1000 weighted patients")

# make y scale the same
current_ranges <- c(ggplot_build(combined_staffing[[1]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined_staffing[[2]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined_staffing[[3]])$layout$panel_scales_y[[1]]$range$range)

all_staffing <- combined_staffing & ylim(min(current_ranges), max(current_ranges))

save_fig("national_staffing_quarterly_2025-07-03.png", all_staffing, width = 18)
save_fig("national_staffing_quarterly_2025-07-03.svg", all_staffing, width = 18)

## c. QOF ----
# map to percentage scale
qof_percent_res <- readRDS(glue("{resfile}/processed_results/qof_percent_2025-07-03.rds")) %>%
  mutate(time_var = as.double(time_var)) %>%
  mutate(
    t0 = 100 * t0,
    lci = 100 * lci,
    uci = 100 * uci
  )

p <- plot_national(qof_percent_res, "Slope index of inequality for % of QOF achievement points", "QOF year", outcome = "qof", linebreaks = c(2021, 2022, 2023))
save_fig("national_qof_percent_2025-07-03.png", p)
save_fig("national_qof_percent_2025-07-03.svg", p)

## d. All combined ----
p1a <- plot_national(gpps_18_res, str_wrap("% of 'very good/fairly good' responses to overall experience of making appointment", 80), "Survey year", outcome = "gpps", linebreaks = c(2024), sz = 1.0)
p1b <- plot_national(gpps_28_res, str_wrap("% of 'very good/fairly good' responses to overall experience of GP practice", 80), "Survey year", outcome = "gpps", linebreaks = c(2024), sz = 1.0)
p1c <- plot_national(gpps_90_res, str_wrap("% of 'yes, definitely/yes, to some extent' responses to needs met at last appointment", 80), "Survey year", outcome = "gpps", linebreaks = c(2024), sz = 1.0)

p2a <- plot_national(admin_res_quarter, str_wrap("FTE administrative staff per 1000 weighted patients", 80), "Year", outcome = "staffing")
p2b <- plot_national(gp_res_quarter, str_wrap("FTE GPs (excluding training) per 1000 weighted patients", 80), "Year", outcome = "staffing")
p2c <- plot_national(nurse_res_quarter, str_wrap("FTE nurses per 1000 weighted patients", 80), "Year", outcome = "staffing")

p3a <- plot_national(qof_percent_res, str_wrap("% of QOF achievement points", 80), "QOF year", outcome = "qof", linebreaks = c(2021, 2022, 2023), sz = 1.0)

combined_1 <- p1a + p1b + p1c + plot_layout(axis_titles = "collect")

# make y scale the same
current_ranges_1 <- c(ggplot_build(combined_1[[1]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined_1[[2]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined_1[[3]])$layout$panel_scales_y[[1]]$range$range)

combined_2 <- p2a + p2b + p2c + plot_layout(axis_titles = "collect")

# make y scale the same
current_ranges_2 <- c(ggplot_build(combined_2[[1]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined_2[[2]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined_2[[3]])$layout$panel_scales_y[[1]]$range$range)

all_national <- (p1a + p1b + p1c + plot_layout(axis_titles = "collect") & ylim(min(current_ranges_1), max(current_ranges_1)) & theme(legend.position = "none", plot.title = element_text(size = 10), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), axis.text.x = element_text(angle = 0, hjust = 0.5))) /
  (p2a + p2b + p2c + plot_layout(axis_titles = "collect") & ylim(min(current_ranges_2), max(current_ranges_2)) & theme(legend.position = "none", plot.title = element_text(size = 10), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), axis.text.x = element_text(angle = 0, hjust = 0.5))) /
  (p3a + theme(plot.title = element_text(size = 10), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), axis.text.x = element_text(angle = 0, hjust = 0.5)) + plot_spacer() + guide_area() + plot_layout(guides = "collect")) + plot_annotation(title = "Slope index of inequality derived using national deprivation scores")

save_fig("all_national_2025-07-03.png", all_national, width = 18)
save_fig("all_national_2025-07-03.svg", all_national, width = 18)

# clean up
rm(all_gpps, all_national, all_staffing, combined, combined_1, combined_2, combined_staffing, p, p18, p1a, p1b, p1c, p28, p2a, p2b, p2c, p3a, p90, current_ranges, current_ranges_1, current_ranges_2)

# 2. LLR only ----
## a. GPPS ----
GPPS_results <- tibble()

for (q in c("q18", "q28", "q90")) {
  LLR_quintile <- readRDS(glue("{resfile}/processed_results/LLR_only/LLR_quintile_{q}_12pct_2025-07-07.rds"))

  amount <- readRDS(glue("{resfile}/processed_results/LLR_only/amount_{q}_12pct_2025-07-07.rds"))

  national_quintile <- readRDS(glue("{resfile}/processed_results/{q}_12pct_2025-07-03.rds")) %>%
    filter(loc == "QK1") %>%
    select(-loc)

  GPPS_results <- GPPS_results %>%
    rbind(LLR_quintile %>% mutate(need_var = "LLR quintiles", GPPS_var = q)) %>%
    rbind(amount %>% mutate(need_var = "Payment bands", GPPS_var = q)) %>%
    rbind(national_quintile %>% mutate(need_var = "National quintiles", GPPS_var = q))
}

GPPS_results <- GPPS_results %>%
  mutate(
    GPPS_desc = case_when(
      GPPS_var == "q18" ~ "Overall experience of making appointment",
      GPPS_var == "q28" ~ "Overall experience of GP practice",
      GPPS_var == "q90" ~ "Needs met at last appointment",
      T ~ NA
    ),
    time_var = as.double(time_var)
  ) %>%
  mutate(
    t0 = 100 * t0,
    lci = 100 * lci,
    uci = 100 * uci
  )

p <- GPPS_results %>%
  mutate(time_var = as.double(time_var)) %>%
  ggplot(aes(time_var, t0)) +
  facet_grid(need_var ~ GPPS_desc) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "#074089") +
  geom_vline(xintercept = 2021 + (7 / 12), linetype = "dotted", col = "#A675E7", linewidth = 1) +
  geom_point(data = . %>% filter(time_var < 2024)) +
  geom_line(data = . %>% filter(time_var < 2024)) +
  geom_point(data = . %>% filter(time_var == 2024)) +
  geom_line(data = . %>% filter(time_var == 2024)) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    x = "Survey year",
    y = "SII",
    title = "Slope index of inequality for GPPS outcomes in LLR by deprivation measure"
  ) +
  scale_x_continuous(breaks = 2018:2024, labels = c("2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24"))

save_fig("LLR_GPPS_2025-07-03.png", p)
save_fig("LLR_GPPS_2025-07-03.svg", p)

## b. Staffing ----
staffing_results <- tibble()

for (q in c("gp_extg", "nurses", "admin")) {
  LLR_quintile <- readRDS(glue("{resfile}/processed_results/LLR_only/LLR_quintile_total_{q}_fte_weighted_2025-07-07.rds"))

  amount <- readRDS(glue("{resfile}/processed_results/LLR_only/amount_total_{q}_fte_weighted_2025-07-07.rds"))

  national_quintile <- readRDS(glue("{resfile}/processed_results/total_{q}_fte_weighted_2025-07-03.rds")) %>%
    filter(loc == "QK1") %>%
    select(-loc)

  staffing_results <- staffing_results %>%
    rbind(LLR_quintile %>% mutate(need_var = "LLR quintiles", staff_role = q)) %>%
    rbind(amount %>% mutate(need_var = "Payment bands", staff_role = q)) %>%
    rbind(national_quintile %>% mutate(need_var = "National quintiles", staff_role = q))
}

staffing_results <- staffing_results %>%
  mutate(staff_desc = case_when(
    staff_role == "gp_extg" ~ "GPs (excluding training)",
    staff_role == "nurses" ~ "Nurses",
    staff_role == "admin" ~ "Administrative staff",
    T ~ NA
  ))

p <- staffing_results %>%
  mutate(time_var = ymd(time_var)) %>%
  ggplot(aes(time_var, t0)) +
  facet_grid(need_var ~ staff_desc) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "#074089") +
  geom_vline(xintercept = ymd("2021-07-01"), linetype = "dotted", col = "#A675E7", linewidth = 1) +
  geom_point(size = 0.6) +
  geom_line() +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4) +
  theme_bw() +
  labs(
    x = "Year",
    y = "SII",
    title = "Slope index of inequality for FTE staff per 1000 weighted patients in LLR by deprivation measure"
  )

save_fig("LLR_staffing_2025-07-03.png", p)
save_fig("LLR_staffing_2025-07-03.svg", p)


## b2. Staffing - quarterly ----
staffing_results_quarter <- (staffing_results %>%
  filter(
    time_var < ymd("2021-07-01"),
    month(time_var) %in% c(1, 4, 7, 10)
  )) %>%
  rbind(staffing_results %>%
    filter(time_var >= ymd("2021-07-01")))

p <- staffing_results_quarter %>%
  mutate(time_var = ymd(time_var)) %>%
  ggplot(aes(time_var, t0)) +
  facet_grid(need_var ~ staff_desc) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "#074089") +
  geom_vline(xintercept = ymd("2021-07-01"), linetype = "dotted", col = "#A675E7", linewidth = 1) +
  geom_point(size = 0.6) +
  geom_line() +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4) +
  theme_bw() +
  labs(
    x = "Year",
    y = "SII",
    title = "Slope index of inequality for staffing outcomes in LLR by deprivation measure"
  )

save_fig("LLR_staffing_quarterly_2025-07-03.png", p)
save_fig("LLR_staffing_quarterly_2025-07-03.svg", p)

## c. QOF ----
QOF_results <- tibble()

LLR_quintile <- readRDS(glue("{resfile}/processed_results/LLR_only/LLR_quintile_qof_percent_2025-07-07.rds"))

amount <- readRDS(glue("{resfile}/processed_results/LLR_only/amount_qof_percent_2025-07-07.rds"))

national_quintile <- readRDS(glue("{resfile}/processed_results/qof_percent_2025-07-03.rds")) %>%
  filter(loc == "QK1") %>%
  select(-loc)

QOF_results <- QOF_results %>%
  rbind(LLR_quintile %>% mutate(need_var = "LLR quintiles")) %>%
  rbind(amount %>% mutate(need_var = "Payment bands")) %>%
  rbind(national_quintile %>% mutate(need_var = "National quintiles")) %>%
  mutate(
    time_var = as.double(time_var),
    t0 = 100 * t0,
    lci = 100 * lci,
    uci = 100 * uci
  )

p <- QOF_results %>%
  ggplot(aes(time_var, t0)) +
  facet_grid(need_var ~ .) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "#074089") +
  geom_vline(xintercept = 2021 + (7 / 12), linetype = "dotted", col = "#A675E7", linewidth = 1) +
  geom_point(data = . %>% filter(time_var < 2021)) +
  geom_line(data = . %>% filter(time_var < 2021)) +
  geom_point(data = . %>% filter(time_var == 2021)) +
  geom_line(data = . %>% filter(time_var == 2021)) +
  geom_point(data = . %>% filter(time_var == 2022)) +
  geom_line(data = . %>% filter(time_var == 2022)) +
  geom_point(data = . %>% filter(time_var == 2023)) +
  geom_line(data = . %>% filter(time_var == 2023)) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    x = "QOF year",
    y = "SII",
    title = "Slope index of inequality for % of QOF achievement points in LLR by deprivation measure"
  ) +
  scale_x_continuous(breaks = 2017:2023, labels = c("2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24"))

save_fig("LLR_qof_percent_2025-07-03.png", p)
save_fig("LLR_qof_percent_2025-07-03.svg", p)

## d. All LLR ----
p1a <- GPPS_results %>%
  filter(need_var == "Payment bands", GPPS_var == "q18") %>%
  mutate(time_var = as.double(time_var)) %>%
  ggplot(aes(time_var, t0)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "#074089") +
  geom_vline(xintercept = 2021 + (7 / 12), linetype = "dotted", col = "#A675E7", linewidth = 1) +
  geom_point(data = . %>% filter(time_var < 2024)) +
  geom_line(data = . %>% filter(time_var < 2024)) +
  geom_point(data = . %>% filter(time_var == 2024)) +
  geom_line(data = . %>% filter(time_var == 2024)) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4) +
  theme_bw() +
  labs(
    x = "Survey year",
    y = "SII",
    title = str_wrap("% of 'very good/fairly good' responses to overall experience of making appointment", 80)
  ) +
  scale_x_continuous(breaks = 2018:2024, labels = c("2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24"))

p1b <- GPPS_results %>%
  filter(need_var == "Payment bands", GPPS_var == "q28") %>%
  mutate(time_var = as.double(time_var)) %>%
  ggplot(aes(time_var, t0)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "#074089") +
  geom_vline(xintercept = 2021 + (7 / 12), linetype = "dotted", col = "#A675E7", linewidth = 1) +
  geom_point(data = . %>% filter(time_var < 2024)) +
  geom_line(data = . %>% filter(time_var < 2024)) +
  geom_point(data = . %>% filter(time_var == 2024)) +
  geom_line(data = . %>% filter(time_var == 2024)) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4) +
  theme_bw() +
  labs(
    x = "Survey year",
    y = "SII",
    title = str_wrap("% of 'very good/fairly good' responses to overall experience of GP practice", 80)
  ) +
  scale_x_continuous(breaks = 2018:2024, labels = c("2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24"))

p1c <- GPPS_results %>%
  filter(need_var == "Payment bands", GPPS_var == "q90") %>%
  mutate(time_var = as.double(time_var)) %>%
  ggplot(aes(time_var, t0)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "#074089") +
  geom_vline(xintercept = 2021 + (7 / 12), linetype = "dotted", col = "#A675E7", linewidth = 1) +
  geom_point(data = . %>% filter(time_var < 2024)) +
  geom_line(data = . %>% filter(time_var < 2024)) +
  geom_point(data = . %>% filter(time_var == 2024)) +
  geom_line(data = . %>% filter(time_var == 2024)) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4) +
  theme_bw() +
  labs(
    x = "Survey year",
    y = "SII",
    title = str_wrap("% of 'yes, definitely/yes, to some extent' responses to needs met at last appointment", 80)
  ) +
  scale_x_continuous(breaks = 2018:2024, labels = c("2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24"))

p2a <- staffing_results_quarter %>%
  filter(need_var == "Payment bands", staff_role == "admin") %>%
  mutate(time_var = ymd(time_var)) %>%
  ggplot(aes(time_var, t0)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "#074089") +
  geom_vline(xintercept = ymd("2021-07-01"), linetype = "dotted", col = "#A675E7", linewidth = 1) +
  geom_point(size = 0.6) +
  geom_line() +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4) +
  theme_bw() +
  labs(
    x = "Year",
    y = "SII",
    title = str_wrap("FTE administrative staff per 1000 weighted patients", 80)
  )

p2b <- staffing_results_quarter %>%
  filter(need_var == "Payment bands", staff_role == "gp_extg") %>%
  mutate(time_var = ymd(time_var)) %>%
  ggplot(aes(time_var, t0)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "#074089") +
  geom_vline(xintercept = ymd("2021-07-01"), linetype = "dotted", col = "#A675E7", linewidth = 1) +
  geom_point(size = 0.6) +
  geom_line() +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4) +
  theme_bw() +
  labs(
    x = "Year",
    y = "SII",
    title = str_wrap("FTE GPs (excluding training) per 1000 weighted patients", 80)
  )

p2c <- staffing_results_quarter %>%
  filter(need_var == "Payment bands", staff_role == "nurses") %>%
  mutate(time_var = ymd(time_var)) %>%
  ggplot(aes(time_var, t0)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "#074089") +
  geom_vline(xintercept = ymd("2021-07-01"), linetype = "dotted", col = "#A675E7", linewidth = 1) +
  geom_point(size = 0.6) +
  geom_line() +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4) +
  theme_bw() +
  labs(
    x = "Year",
    y = "SII",
    title = str_wrap("FTE nurses per 1000 weighted patients", 80)
  )

p3a <- QOF_results %>%
  filter(need_var == "Payment bands") %>%
  ggplot(aes(time_var, t0)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "#074089") +
  geom_vline(xintercept = 2021 + (7 / 12), linetype = "dotted", col = "#A675E7", linewidth = 1) +
  geom_point(data = . %>% filter(time_var < 2021)) +
  geom_line(data = . %>% filter(time_var < 2021)) +
  geom_point(data = . %>% filter(time_var == 2021)) +
  geom_line(data = . %>% filter(time_var == 2021)) +
  geom_point(data = . %>% filter(time_var == 2022)) +
  geom_line(data = . %>% filter(time_var == 2022)) +
  geom_point(data = . %>% filter(time_var == 2023)) +
  geom_line(data = . %>% filter(time_var == 2023)) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4) +
  theme_bw() +
  labs(
    x = "QOF year",
    y = "SII",
    title = "% of QOF achievement points"
  ) +
  scale_x_continuous(breaks = 2017:2023, labels = c("2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24"))

combined_1 <- p1a + p1b + p1c + plot_layout(axis_titles = "collect")

# make y scale the same
current_ranges_1 <- c(ggplot_build(combined_1[[1]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined_1[[2]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined_1[[3]])$layout$panel_scales_y[[1]]$range$range)

combined_2 <- p2a + p2b + p2c + plot_layout(axis_titles = "collect")

# make y scale the same
current_ranges_2 <- c(ggplot_build(combined_2[[1]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined_2[[2]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined_2[[3]])$layout$panel_scales_y[[1]]$range$range)

all_LLR <- (p1a + p1b + p1c + plot_layout(axis_titles = "collect") & ylim(min(current_ranges_1), max(current_ranges_1)) & theme(legend.position = "none", plot.title = element_text(size = 10), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), axis.text.x = element_text(angle = 0, hjust = 0.5))) /
  (p2a + p2b + p2c + plot_layout(axis_titles = "collect") & ylim(min(current_ranges_2), max(current_ranges_2)) & theme(legend.position = "none", plot.title = element_text(size = 10), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), axis.text.x = element_text(angle = 0, hjust = 0.5))) /
  (p3a + theme(plot.title = element_text(size = 10), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), axis.text.x = element_text(angle = 0, hjust = 0.5)) + plot_spacer() + plot_spacer()) + plot_annotation(title = "Slope index of inequality in LLR derived using HEP payment bands")

save_fig("all_LLR_2025-07-03.png", all_LLR, width = 18)
save_fig("all_LLR_2025-07-03.svg", all_LLR, width = 18)


# 3. Appendix figures ----
clean_df <- readRDS("cleaned_data/complete_cleaned.rds") %>%
  mutate(
    q18_12pct = 100 * q18_12pct,
    q28_12pct = 100 * q28_12pct,
    q90_12pct = 100 * q90_12pct,
    clean_quintile = case_match(
      clean_quintile,
      1 ~ "1 - Most deprived",
      2 ~ "2",
      3 ~ "3",
      4 ~ "4",
      5 ~ "5 - Least deprived"
    )
  )

LLR_df <- clean_df %>%
  filter(icb_code == "QK1") %>%
  mutate(subicb = case_match(
    sub_icb_code,
    "03W" ~ "East Leicestershire & Rutland",
    "04C" ~ "Leicester City",
    "04V" ~ "West Leicestershire"
  ))

# N.B. dataframe variables are handled slightly differently here as time variable is no longer called 'time_var' in the dataframe
LLR_context <- function(outcome_var, time_var, title, x, linebreaks = c(), quarterly = F) {
  # for staffing variables quarterly may be used
  if (quarterly) {
    LLR_df <- (LLR_df %>%
      filter(
        .data[[time_var]] < ymd("2021-07-01"),
        month(.data[[time_var]]) %in% c(1, 4, 7, 10)
      )) %>%
      rbind(LLR_df %>%
        filter(.data[[time_var]] >= ymd("2021-07-01")))
  }

  a <- LLR_df %>%
    group_by(.data[[time_var]], clean_quintile) %>%
    summarise(mean.out = mean(.data[[outcome_var]], na.rm = T)) %>%
    ungroup()

  b <- LLR_df %>%
    group_by(.data[[time_var]], subicb) %>%
    summarise(mean.out = mean(.data[[outcome_var]], na.rm = T)) %>%
    ungroup()

  remaining_a <- a
  remaining_b <- b

  a <- a %>%
    ggplot(aes(.data[[time_var]], mean.out, col = clean_quintile, group = clean_quintile))
  b <- b %>%
    ggplot(aes(.data[[time_var]], mean.out, col = subicb, group = subicb))
  if (length(linebreaks)) {
    for (i in linebreaks) {
      before_a <- remaining_a %>%
        filter(.data[[time_var]] < i)
      before_b <- remaining_b %>%
        filter(.data[[time_var]] < i)

      a <- a +
        geom_point(data = before_a) + geom_line(data = before_a)

      b <- b +
        geom_point(data = before_b) + geom_line(data = before_b)

      remaining_a <- remaining_a %>%
        filter(.data[[time_var]] >= i)
      remaining_b <- remaining_b %>%
        filter(.data[[time_var]] >= i)
    }
  }

  # add the last chunk
  a <- a +
    geom_point(data = remaining_a) + geom_line(data = remaining_a) +
    theme_bw() + theme(legend.position = "bottom") +
    scale_colour_manual(values = c("#FF495C", "#2EC4B6", "#470FF4", "#FFBE0B", "#C879FF")) +
    labs(
      title = "Nationally-assigned IMD quintiles in LLR",
      colour = "",
      y = "Mean outcome",
      x = x
    )

  b <- b +
    geom_point(data = remaining_b) + geom_line(data = remaining_b) +
    theme_bw() + theme(legend.position = "bottom") +
    labs(
      title = "LLR Sub-ICSs",
      colour = "",
      y = "Mean outcome",
      x = x
    )

  if (time_var == "map_date") {
    a <- a +
      geom_vline(xintercept = ymd("2021-07-01"), linetype = "dotted", col = "#A675E7", linewidth = 1)
    b <- b +
      geom_vline(xintercept = ymd("2021-07-01"), linetype = "dotted", col = "#A675E7", linewidth = 1)
  } else {
    a <- a +
      geom_vline(xintercept = 2021 + (7 / 12), linetype = "dotted", col = "#A675E7", linewidth = 1)
    b <- b +
      geom_vline(xintercept = 2021 + (7 / 12), linetype = "dotted", col = "#A675E7", linewidth = 1)
  }

  combined <- a + b +
    plot_layout(axis_titles = "collect") +
    plot_annotation(title = title)

  # make y scale the same
  current_ranges <- c(ggplot_build(combined[[1]])$layout$panel_scales_y[[1]]$range$range, ggplot_build(combined[[2]])$layout$panel_scales_y[[1]]$range$range)

  combined & ylim(min(current_ranges), max(current_ranges))
}

# N.B. dataframe variables are handled slightly differently here as time variable is no longer called 'time_var' in the dataframe
national_context <- function(outcome_var, time_var, title, x, linebreaks = c(), quarterly = F) {
  # for staffing variables quarterly may be used
  if (quarterly) {
    clean_df <- (clean_df %>%
      filter(
        .data[[time_var]] < ymd("2021-07-01"),
        month(.data[[time_var]]) %in% c(1, 4, 7, 10)
      )) %>%
      rbind(clean_df %>%
        filter(.data[[time_var]] >= ymd("2021-07-01")))
  }

  a <- clean_df %>%
    group_by(.data[[time_var]], clean_quintile) %>%
    summarise(mean.out = mean(.data[[outcome_var]], na.rm = T)) %>%
    ungroup()

  remaining_a <- a

  a <- a %>%
    ggplot(aes(.data[[time_var]], mean.out, col = clean_quintile, group = clean_quintile))
  if (length(linebreaks)) {
    for (i in linebreaks) {
      before_a <- remaining_a %>%
        filter(.data[[time_var]] < i)

      a <- a +
        geom_point(data = before_a) + geom_line(data = before_a)

      remaining_a <- remaining_a %>%
        filter(.data[[time_var]] >= i)
    }
  }

  # add the last chunk
  a <- a +
    geom_point(data = remaining_a) + geom_line(data = remaining_a) +
    theme_bw() + theme(legend.position = "bottom") +
    scale_colour_manual(values = c("#FF495C", "#2EC4B6", "#470FF4", "#FFBE0B", "#C879FF")) +
    labs(
      title = title,
      subtitle = "Nationally-assigned IMD quintiles",
      colour = "",
      y = "Mean outcome",
      x = x
    )

  if (time_var == "map_date") {
    a <- a +
      geom_vline(xintercept = ymd("2021-07-01"), linetype = "dotted", col = "#A675E7", linewidth = 1)
  } else {
    a <- a +
      geom_vline(xintercept = 2021 + (7 / 12), linetype = "dotted", col = "#A675E7", linewidth = 1)
  }

  return(a)
}

## a. GPPS ----
p <- LLR_context("q18_12pct", "survey_year", "Mean % of 'very good/fairly good' responses to overall experience of making appointment in GPPS in LLR practices", "Survey year", linebreaks = c(2024))
save_fig("appendix_LLR_q18_12pct_2025-07-03.png", p, width = 12)
save_fig("appendix_LLR_q18_12pct_2025-07-03.svg", p, width = 12)

p <- LLR_context("q28_12pct", "survey_year", "Mean % of 'very good/fairly good' responses to overall experience of GP practice in GPPS in LLR practices", "Survey year", linebreaks = c(2024))
save_fig("appendix_LLR_q28_12pct_2025-07-03.png", p, width = 12)
save_fig("appendix_LLR_q28_12pct_2025-07-03.svg", p, width = 12)

p <- LLR_context("q90_12pct", "survey_year", "Mean % of 'yes, definitely/yes, to some extent' responses to needs met at last appointment in GPPS in LLR practices", "Survey year", linebreaks = c(2024))
save_fig("appendix_LLR_q90_12pct_2025-07-03.png", p, width = 12)
save_fig("appendix_LLR_q90_12pct_2025-07-03.svg", p, width = 12)

p <- national_context("q18_12pct", "survey_year", "Mean % of 'very good/fairly good' responses to overall experience of making appointment in GPPS in general practices in England", "Survey year", linebreaks = c(2024))
save_fig("appendix_national_q18_12pct_2025-07-03.png", p, width = 12)
save_fig("appendix_national_q18_12pct_2025-07-03.svg", p, width = 12)

p <- national_context("q28_12pct", "survey_year", "Mean % of 'very good/fairly good' responses to overall experience of GP practice in GPPS in general practices in England", "Survey year", linebreaks = c(2024))
save_fig("appendix_national_q28_12pct_2025-07-03.png", p, width = 12)
save_fig("appendix_national_q28_12pct_2025-07-03.svg", p, width = 12)

p <- national_context("q90_12pct", "survey_year", "Mean % of 'yes, definitely/yes, to some extent' responses to needs met at last appointment in GPPS in general practices in England", "Survey year", linebreaks = c(2024))
save_fig("appendix_national_q90_12pct_2025-07-03.png", p, width = 12)
save_fig("appendix_national_q90_12pct_2025-07-03.svg", p, width = 12)

## b. Staffing ----
p <- LLR_context("total_gp_extg_fte_weighted", "map_date", "Mean FTE GPs (excluding training) per 1000 weighted patients in LLR practices", "Year")
save_fig("appendix_LLR_GPs_2025-07-03.png", p, width = 12)
save_fig("appendix_LLR_GPs_2025-07-03.svg", p, width = 12)

p <- LLR_context("total_nurses_fte_weighted", "map_date", "Mean FTE nurses per 1000 weighted patients in LLR practices", "Year")
save_fig("appendix_LLR_nurses_2025-07-03.png", p, width = 12)
save_fig("appendix_LLR_nurses_2025-07-03.svg", p, width = 12)

p <- LLR_context("total_admin_fte_weighted", "map_date", "Mean FTE administrative staff per 1000 weighted patients in LLR practices", "Year")
save_fig("appendix_LLR_admin_2025-07-03.png", p, width = 12)
save_fig("appendix_LLR_admin_2025-07-03.svg", p, width = 12)

p <- national_context("total_gp_extg_fte_weighted", "map_date", "Mean FTE GPs (excluding training) per 1000 weighted patients in general practices in England", "Year")
save_fig("appendix_national_GPs_2025-07-03.png", p, width = 12)
save_fig("appendix_national_GPs_2025-07-03.svg", p, width = 12)

p <- national_context("total_nurses_fte_weighted", "map_date", "Mean FTE nurses per 1000 weighted patients in general practices in England", "Year")
save_fig("appendix_national_nurses_2025-07-03.png", p, width = 12)
save_fig("appendix_national_nurses_2025-07-03.svg", p, width = 12)

p <- national_context("total_admin_fte_weighted", "map_date", "Mean FTE administrative staff per 1000 weighted patients in general practices in England", "Year")
save_fig("appendix_national_admin_2025-07-03.png", p, width = 12)
save_fig("appendix_national_admin_2025-07-03.svg", p, width = 12)

## b2. Staffing - quarterly ---
p <- LLR_context("total_gp_extg_fte_weighted", "map_date", "Mean FTE GPs (excluding training) per 1000 weighted patients in LLR practices", "Year", quarterly = T)
save_fig("appendix_LLR_GPs_quarterly_2025-07-03.png", p, width = 12)
save_fig("appendix_LLR_GPs_quarterly_2025-07-03.svg", p, width = 12)

p <- LLR_context("total_nurses_fte_weighted", "map_date", "Mean FTE nurses per 1000 weighted patients in LLR practices", "Year", quarterly = T)
save_fig("appendix_LLR_nurses_quarterly_2025-07-03.png", p, width = 12)
save_fig("appendix_LLR_nurses_quarterly_2025-07-03.svg", p, width = 12)

p <- LLR_context("total_admin_fte_weighted", "map_date", "Mean FTE administrative staff per 1000 weighted patients in LLR practices", "Year", quarterly = T)
save_fig("appendix_LLR_admin_quarterly_2025-07-03.png", p, width = 12)
save_fig("appendix_LLR_admin_quarterly_2025-07-03.svg", p, width = 12)


p <- national_context("total_gp_extg_fte_weighted", "map_date", "Mean FTE GPs (excluding training) per 1000 weighted patients in general practices in England", "Year", quarterly = T)
save_fig("appendix_national_GPs_quarterly_2025-07-03.png", p, width = 12)
save_fig("appendix_national_GPs_quarterly_2025-07-03.svg", p, width = 12)

p <- national_context("total_nurses_fte_weighted", "map_date", "Mean FTE nurses per 1000 weighted patients in general practices in England", "Year", quarterly = T)
save_fig("appendix_national_nurses_quarterly_2025-07-03.png", p, width = 12)
save_fig("appendix_national_nurses_quarterly_2025-07-03.svg", p, width = 12)

p <- national_context("total_admin_fte_weighted", "map_date", "Mean FTE administrative staff per 1000 weighted patients in general practices in England", "Year", quarterly = T)
save_fig("appendix_national_admin_quarterly_2025-07-03.png", p, width = 12)
save_fig("appendix_national_admin_quarterly_2025-07-03.svg", p, width = 12)

## c. QOF ----
p <- LLR_context("qof_percent", "qof_year", "Mean % QOF achievement points in LLR practices", "QOF year", linebreaks = c(2021, 2022, 2023))
save_fig("appendix_LLR_qof_percent_2025-07-03.png", p, width = 12)
save_fig("appendix_LLR_qof_percent_2025-07-03.svg", p, width = 12)

p <- national_context("qof_percent", "qof_year", "Mean % QOF achievement points in general practices in England", "QOF year", linebreaks = c(2021, 2022, 2023))
save_fig("appendix_national_qof_percent_2025-07-03.png", p, width = 12)
save_fig("appendix_national_qof_percent_2025-07-03.svg", p, width = 12)

# 4. Practice characteristics pre-HEQ ----
## a. National ----
national_chars <- clean_df %>%
  filter(map_date == "2021-06-01") %>%
  select(NUMBER_OF_PATIENTS, PERC_MALE, PERC_65, POP_WHITE, POP_ENG2, POP_NOENG, POP_RURAL, LLR, clean_quintile, total_gp_extg_fte_weighted, total_nurses_fte_weighted, total_admin_fte_weighted, q18_12pct, q28_12pct, q90_12pct, qof_percent) %>%
  tbl_summary(
    by = LLR,
    digits = list(PERC_MALE ~ 2, PERC_65 ~ 2, POP_WHITE ~ 2, POP_ENG2 ~ 2, POP_NOENG ~ 2, POP_RURAL ~ 2, total_gp_extg_fte_weighted ~ 2, total_nurses_fte_weighted ~ 2, total_admin_fte_weighted ~ 2, q18_12pct ~ 2, q28_12pct ~ 2, q90_12pct ~ 2, qof_percent ~ 2),
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_overall()

national_chars %>%
  as_gt() %>%
  gt::gtsave(filename = glue("{resfile}/figures/{td}/national_practice_chars_june_21.docx"))


## b. Comparator ICSs ----
ics_chars <- clean_df %>%
  filter(
    map_date == "2021-06-01",
    icb_code %in% c("QJ2", "QOQ", "QT1", "QHM", "QWO", "QF7", "QE1", "QWU", "QJM", "QNC")
  ) %>%
  select(NUMBER_OF_PATIENTS, PERC_MALE, PERC_65, POP_WHITE, POP_ENG2, POP_NOENG, POP_RURAL, LLR, clean_quintile, total_gp_extg_fte_weighted, total_nurses_fte_weighted, total_admin_fte_weighted, q18_12pct, q28_12pct, q90_12pct, qof_percent) %>%
  tbl_summary(
    digits = list(PERC_MALE ~ 2, PERC_65 ~ 2, POP_WHITE ~ 2, POP_ENG2 ~ 2, POP_NOENG ~ 2, POP_RURAL ~ 2, total_gp_extg_fte_weighted ~ 2, total_nurses_fte_weighted ~ 2, total_admin_fte_weighted ~ 2, q18_12pct ~ 2, q28_12pct ~ 2, q90_12pct ~ 2, qof_percent ~ 2),
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)")
  )

ics_chars %>%
  as_gt() %>%
  gt::gtsave(filename = glue("{resfile}/figures/{td}/comparator_ics_practice_chars_june_21.docx"))

## c. LLR ----
# add payment data to LLR df
LLR_payments <- readRDS("cleaned_data/practice_payments_cleaned.rds")

LLR_with_payments <- LLR_df %>%
  left_join(LLR_payments %>% select(Practice.Code, Amount_21_22), by = join_by(PRACTICE_CODE == Practice.Code)) %>%
  rename(amount = Amount_21_22) %>%
  mutate(received_funding = (amount <= 4)) %>%
  filter(map_date == "2021-06-01")

payment_key <- readRDS("cleaned_data/payment_key.rds")

### i. Sub ICB ----
tb_sub <- LLR_with_payments %>%
  select(NUMBER_OF_PATIENTS, PERC_MALE, PERC_65, POP_WHITE, POP_ENG2, POP_NOENG, POP_RURAL, subicb, received_funding, clean_quintile, total_gp_extg_fte_weighted, total_nurses_fte_weighted, total_admin_fte_weighted, q18_12pct, q28_12pct, q90_12pct, qof_percent) %>%
  tbl_summary(
    by = subicb,
    digits = list(PERC_MALE ~ 2, PERC_65 ~ 2, POP_WHITE ~ 2, POP_ENG2 ~ 2, POP_NOENG ~ 2, POP_RURAL ~ 2, total_gp_extg_fte_weighted ~ 2, total_nurses_fte_weighted ~ 2, total_admin_fte_weighted ~ 2, q18_12pct ~ 2, q28_12pct ~ 2, q90_12pct ~ 2, qof_percent ~ 2),
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_overall()

tb_sub %>%
  as_gt() %>%
  gt::gtsave(filename = glue("{resfile}/figures/{td}/LLR_practice_chars_subicb_june_21.docx"))

### ii. IMD quintile ----
tb_imd <- LLR_with_payments %>%
  select(NUMBER_OF_PATIENTS, PERC_MALE, PERC_65, POP_WHITE, POP_ENG2, POP_NOENG, POP_RURAL, subicb, received_funding, clean_quintile, total_gp_extg_fte_weighted, total_nurses_fte_weighted, total_admin_fte_weighted, q18_12pct, q28_12pct, q90_12pct, qof_percent) %>%
  tbl_summary(
    by = clean_quintile,
    digits = list(PERC_MALE ~ 2, PERC_65 ~ 2, POP_WHITE ~ 2, POP_ENG2 ~ 2, POP_NOENG ~ 2, POP_RURAL ~ 2, total_gp_extg_fte_weighted ~ 2, total_nurses_fte_weighted ~ 2, total_admin_fte_weighted ~ 2, q18_12pct ~ 2, q28_12pct ~ 2, q90_12pct ~ 2, qof_percent ~ 2),
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_overall()

tb_imd %>%
  as_gt() %>%
  gt::gtsave(filename = glue("{resfile}/figures/{td}/LLR_practice_chars_imd_quintile_june_21.docx"))

### iii. Fact of funding ----
tb_bin <- LLR_with_payments %>%
  select(NUMBER_OF_PATIENTS, PERC_MALE, PERC_65, POP_WHITE, POP_ENG2, POP_NOENG, POP_RURAL, subicb, received_funding, clean_quintile, total_gp_extg_fte_weighted, total_nurses_fte_weighted, total_admin_fte_weighted, q18_12pct, q28_12pct, q90_12pct, qof_percent) %>%
  tbl_summary(
    by = received_funding,
    digits = list(PERC_MALE ~ 2, PERC_65 ~ 2, POP_WHITE ~ 2, POP_ENG2 ~ 2, POP_NOENG ~ 2, POP_RURAL ~ 2, total_gp_extg_fte_weighted ~ 2, total_nurses_fte_weighted ~ 2, total_admin_fte_weighted ~ 2, q18_12pct ~ 2, q28_12pct ~ 2, q90_12pct ~ 2, qof_percent ~ 2),
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_overall()

tb_bin %>%
  as_gt() %>%
  gt::gtsave(filename = glue("{resfile}/figures/{td}/LLR_practice_chars_funding_binary_june_21.docx"))
