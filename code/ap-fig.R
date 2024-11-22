#  program:  ap-fig.R
#  task:     temperature estimates
#  input:    
#  output:   
#  project:  ASM
#  author:   sam harper \ 2024-11-15

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

apt <- read_rds(here("data-clean", 
  "ap-etwfe-table.rds")) %>%
  select(category, outcome, estimate_2, ci_2) %>%
    mutate(
    # Extract lower bound
    ci_lower = as.double(str_extract(ci_2, "(?<=\\().+?(?=,)")),         
    # Extract upper bound
    ci_upper = as.double(str_extract(ci_2, "(?<=, ).+?(?=\\))")))

pplot <- ggplot(subset(apt, category=="Personal"), 
  aes(x = estimate_2, y = outcome)) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = ci_lower, 
    xmax = ci_upper), height=0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y = " ", x = bquote('Total Effect'~(µg/m^3))) +
  scale_x_continuous(limits = c(-50, 20)) +
  ggtitle("Personal Exposure") +
  theme_asm() + 
  theme(plot.title = element_text(hjust = 0))

iplot <- ggplot(subset(apt, category=="Indoor"), 
  aes(x = estimate_2, y = outcome)) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = ci_lower, 
    xmax = ci_upper), height=0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y = " ", x = "Total Effect (µg/m^3^)") +
  scale_x_continuous(limits = c(-50, 20)) +
  ggtitle("Indoor") +
  theme_asm() + 
  theme(plot.title = element_text(hjust = 0))

oplot <- ggplot(subset(apt, category=="Outdoor"), 
  aes(x = estimate_2, y = outcome)) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = ci_lower, 
    xmax = ci_upper), height=0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y = " ", x = "Total Effect (µg/m^3^)") +
  scale_x_continuous(limits = c(-50, 20)) +
  ggtitle("Outdoor") +
  theme_asm() + 
  theme(plot.title = element_text(hjust = 0))

pplot / iplot / oplot + plot_layout(heights = c(1, 1, 1))

ggplot(apt, 
  aes(x = estimate_2, y = outcome)) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = ci_lower, 
    xmax = ci_upper), height=0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y = " ", x = "Total Effect (µg/m^3^)") +
  scale_x_continuous(limits = c(-50, 20)) +
  facet_wrap(vars(category), nrow=3, scales="free") +
  theme_asm() + 
  theme(plot.title = element_text(hjust = 0))
