#  program:  resp-fig.R
#  task:     respiratory estimates
#  input:    
#  output:   
#  project:  ASM
#  author:   sam harper \ 2024-11-20

# load libraries
library(here)
library(readxl)
library(tidyverse)
library(modelsummary)
library(tinytable)

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
      axis.ticks.y = element_blank(),
      plot.title = element_text(size = 18))
}

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

cde_resp <- read_rds(here("data-clean",
  "resp-cdes.rds")) %>%
  filter(outcome=="Any symptom") %>%
  rename(est0 = est, ci0 = ci) %>%
  pivot_longer(!outcome,
    cols_vary = "slowest", 
    names_to = c(".value", "model"),
    names_pattern = "([A-Za-z]+)(\\d+)") %>%
  mutate(
    # Extract lower bound
    ci_lower = as.double(str_extract(ci, "(?<=\\().+?(?=,)")),         
    # Extract upper bound
    ci_upper = as.double(str_extract(ci, "(?<=, ).+?(?=\\))")),
    model = factor(model, labels = c("Adjusted Total Effect", 
    "Mediator: Indoor PM", "Mediator: Indoor Temperature",
    "Mediator: Indoor PM & Temp")))

ggplot(cde_resp, 
  aes(x = est, y = model)) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = ci_lower, 
    xmax = ci_upper), height=0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y = " ", x = "Total Effect (pp)") +
  scale_x_continuous(limits = c(-25, 5)) +
  theme_asm() +
  theme(axis.text.y = element_text(margin = margin(0,20,0,0)))


