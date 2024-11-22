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

# bp results
bp <- read_xlsx(here("data-clean",
  "overall_effects-table-2024Mar26.xlsx")) %>% 
  mutate(order = row_number(),
   model = rep(c("Total Effect", 
     "Adjusted Total Effect"), each = 8)) %>%
  select(order, bp, type, theta_bar, 
         lower_95CI, upper_95CI, model) %>%
  filter(order %in% c(1,3,9,11))

ggplot(bp, 
  aes(x = theta_bar, y = model)) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = lower_95CI, 
    xmax = upper_95CI), height=0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y = " ", x = "Total Effect (µg/m^3^)") +
  scale_x_continuous(limits = c(-4, 4)) +
  facet_wrap(vars(bp), ncol=2) +
  theme_asm() +
  theme(axis.text.y = element_text(margin = margin(0,20,0,0)))


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
