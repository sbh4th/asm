#  program:  descriptive-table.R
#  task:     means / SDs by treatment
#  input:    master data
#  output:   
#  project:  ASM
#  author:   sam harper \ 2024-11-12

# load libraries
library(here)
library(readxl)
library(tidyverse)
library(modelsummary)
library(tinytable)

# load data
d <- read_csv(here("data-clean", 
  "BHET_master_data_04Oct2024.csv"),
  col_select = c("hh_id", "ptc_id", "gender_health",
    "ban_status_no", "age_health", "education_health",
    "smoking", "freq_drink", "sys_brachial", 
    "dia_brachial", "waist_circ", "height",
    "weight", "temp", "wave", "PM25conc_exposureugm3",
    "freq_cough", "freq_phlegm", "freq_wheezing", 
    "freq_breath", "freq_no_chest",
    "BCconc_exposureugm3", "p_usable_pm",
    "p_usable_bc", "lived_with_smoker"))

# data for wealth index
dwi <- read_csv(here("data-clean", 
  "BHET_PCA_11Oct2023.csv"))

# create quantiles of wealth by wave
dwi <- dwi %>%
  group_by(wave) %>%
  mutate(wq = as.integer(cut(wealth_index, 
    quantile(wealth_index, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T), 
    labels = c(1:4), include.lowest = T))) %>%
  ungroup()

dt <- d %>%
  left_join(dwi, by = c("hh_id", "ptc_id", "wave")) %>%
  # restrict to baseline
  filter(wave==1) %>%
  # select variables for inclusion
  # select(ban_status_no, age_health, )
  mutate(ever_trt = ifelse(ban_status_no==0, 1 ,0),
    et = recode_factor(ever_trt, `0` = "Never treated", 
      `1` = "Ever treated"),
    `Age (years)` = age_health,
    `Female (%)` = ifelse(gender_health==2, 100, 0),
    #`No education (%)` = ifelse(education_health==4, 100, 0),
    #`Primary education (%)` = ifelse(education_health==1, 100, 0),
    `Secondary+ education (%)` = ifelse(
      education_health %in% c(2,3), 100, 0),
    `Wealth index (bottom 25%)` = ifelse(wq==1, 100, 0),
    #`Wealth index (25-50%)` = ifelse(wq==2, 100, 0),
    #`Wealth index (50-75%)` = ifelse(wq==3, 100, 0),
    #`Wealth index (top 25%)` = ifelse(wq==4, 100, 0),
    # `Never smoker (%)` = ifelse(smoking==3 & lived_with_smoker==1, 100, 0),
    #`Passive smoker (%)` = ifelse(smoking==3 & lived_with_smoker %in% c(2,3), 100, 0),
    #`Former smoker (%)` = ifelse(smoking==2, 100, 0),
    `Current smoker (%)` = ifelse(smoking==1, 100, 0),
    # `Never drinker (%)` = ifelse(freq_drink==1, 100, 0),
    #`Occasional drinker (%)` = ifelse(freq_drink %in% 
     # c(2:8), 100, 0),
    `Daily drinker (%)` = ifelse(freq_drink==9, 100, 0),
    `Systolic (mmHg)` = sys_brachial,
    `Diastolic (mmHg)` = dia_brachial,
    # `Waist circumference (cm)` = waist_circ,
    `Body mass index (kg/m2)` = weight / (height/100)^2,
    #`Frequency of coughing (%)` = 
    #  if_else(freq_cough < 3, 100, 0),
    #`Frequency of phlegm (%)` = 
    #  if_else(freq_phlegm < 3, 100, 0),
    #`Frequency of wheezing (%)` = 
    #  if_else(freq_wheezing < 3, 100, 0),
    #`Shortness of breath (%)` = 
    #  if_else(freq_breath < 3, 100, 0),
    #`Chest trouble (%)` = 
    #  if_else(freq_no_chest < 3, 100, 0),
    `Any respiratory problem (%)` = if_else(
      freq_cough < 3 |
      freq_phlegm < 3 |
      freq_wheezing < 3 |
      freq_breath < 3 |
      freq_no_chest < 3, 100, 0),
    `Temperature (°C)` = temp,
    pe = case_when(
      p_usable_pm <= 0 ~ NA,
      p_usable_pm == 1 ~ PM25conc_exposureugm3,
      p_usable_pm == 0 ~ NA,
      p_usable_pm == -1 ~ NA),
    bc = case_when(
      BCconc_exposureugm3 <= 0 ~ NA,
      p_usable_bc == 1 ~ BCconc_exposureugm3,
      p_usable_bc == 0 ~ NA,
      p_usable_bc == 2 ~ NA),
    `Personal PM2.5 (ug/m3)` = pe) %>%
    # `Black carbon (ug/m3)` = bc) %>%
  select(et, `Age (years)`, 
    `Female (%)`, 
    `Secondary+ education (%)`,
    `Wealth index (bottom 25%)`,
    `Current smoker (%)`,
    `Daily drinker (%)`,
    `Systolic (mmHg)`, 
    `Diastolic (mmHg)`,
    `Body mass index (kg/m2)`, 
    # `Waist circumference (cm)`,
    `Any respiratory problem (%)`,
    `Temperature (°C)`,
    `Personal PM2.5 (ug/m3)`)

t1 <- datasummary_balance(~et, data = dt)

colnames(t1@table_dataframe) <- c("", "Mean", "SD", 
  "Mean", "SD", "Diff", "SE")

write_rds(t1, here("data-clean", "t_balance.rds"))

t1f <- tt(t1@table_dataframe) %>%
  group_tt(j = list(`Never enrolled (N=603)` = 2:3, 
    `Ever enrolled (N=400)` = 4:5))
t1f

  style_tt(j = 1:7, fontsize = 0.9, align = "lrrrrrr") %>%
  style_tt(i = c(1,11,29), bold=TRUE) %>%

# Old code
#  select(et, `Age (years)`, `Female (%)`, `No education (%)`, 
#    `Primary education (%)`, `Secondary+ education (%)`,
#    `Wealth index (bottom 25%)`, `Wealth index (25-50%)`,
#    `Wealth index (50-75%)`, `Wealth index (top 25%)`,
#    `Never smoker (%)`, `Former smoker (%)`, 
#    `Passive smoker (%)`, 
#    `Current smoker (%)`, `Never drinker (%)`, 
#    `Occasional drinker (%)`, `Daily drinker (%)`,
#    `Systolic (mmHg)`, `Diastolic (mmHg)`,
#    `Waist circumference (cm)`,
#    `Body mass index (kg/m2)`, 
#    `Frequency of coughing (%)`,
#    `Frequency of phlegm (%)`,
#    `Frequency of wheezing (%)`,
#    `Shortness of breath (%)`,
#    `Chest trouble (%)`,
#    `Any respiratory problem (%)`,
#    `Temperature (°C)`,
#    `Personal PM2.5 (ug/m3)`,
#    `Black carbon (ug/m3)`)
    
theme_asm <- function() {
  theme_classic() + 
    theme(axis.title = element_text(size=18),
      axis.text = element_text(size = 18),
      strip.background = element_blank(),
      strip.text = element_text(size = 16),
      axis.text.x = element_text(color="gray50"),
      axis.title.x = element_text(color="gray50"),
      axis.line.x = element_line(color="gray50"),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank())
}

    
otm <- read_xlsx(here("data-clean",
  "adjusted_overall_results.xlsx")) %>%
  filter(str_detect(bp, "PP", 
                    negate = TRUE)) %>%
  filter(str_detect(bp, "amplification", 
                    negate = TRUE)) %>%
  filter(str_detect(bp, "Brachial",
                    negate = FALSE)) %>%
  mutate(m = c("Adjusted Total Effect", 
    "Adjusted Total Effect", "Mediator: Indoor PM",
    "Mediator: Indoor PM", "Mediator: Indoor Temperature",
    "Mediator: Indoor Temperature", "Mediator: Indoor PM & Temp",
    "Mediator: Indoor PM & Temp"),
    o = rep(c("Systolic", "Diastolic"), times = 4),
    n = rep(1:4, each=2),
    label = fct_reorder(m, desc(n)))
  
write_rds(otm, here("data-clean", "cde-bp.rds"))
  
p_te <- ggplot(subset(otm, m == "Adjusted Total Effect"), 
  aes(x = theta_bar, y = label)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = lower_95CI, 
    xmax = upper_95CI), width=0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(bp)) + labs(y = " ", x = "Total Effect (mmHg)") +
  scale_x_continuous(limits = c(-3.5, 2.6)) +
  theme_asm()

p_cde <- ggplot(subset(otm, m != "Adjusted Total Effect"), 
  aes(x = theta_bar, y = label)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = lower_95CI, 
    xmax = upper_95CI), width=0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(bp)) + labs(y = " ") +
  theme(plot.title = element_text(hjust =-0.6)) +
  scale_x_continuous(limits = c(-3.5, 2.6)) +
  xlab("Controlled Direct Effect (mmHg)") + 
  theme_asm()

cde_plot <- p_te / p_cde + plot_layout(heights = c(1, 2))

ggsave(here("images", "cde-plot.png"), cde_plot, 
  width = 9, height = 4.5)


op <- read_rds(here("data-clean", 
  "op-table.rds")) %>%
  filter(category %in% c("Self-reported (pp)",
    "Measured", "Outdoor")) %>%
  select(-estimate_1, -ci_1)
  
colnames(op) <- c(" ", " ", "Obs", "ATT", "(95% CI)")

tt(op) %>%
  group_tt(
    i = list("Respiratory outcomes" = 1,
             "Inflammatory markers" = 8)) %>%
  style_tt(i = c(1, 9), align = "l", bold=T) %>%
  style_tt(
    i = 2, j = 1, rowspan = 6, alignv = "t") %>%
  style_tt(
    i = 10, j = 1, rowspan = 4, alignv = "t") %>%
  format_tt(j=4, sprintf = "%.1f") 
