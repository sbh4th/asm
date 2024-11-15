#  program:  temp-fig.R
#  task:     temperature estimates
#  input:    
#  output:   
#  project:  ASM
#  author:   sam harper \ 2024-11-14

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
      axis.ticks.y = element_blank())
}

stemp_table <- read_xlsx(here("data-clean",
  "marginal_temp_results.xlsx")) %>%
  mutate(
    # Extract mean
    estimate = as.double(str_extract(att, "^[^\\(]+")),                 # Extract lower bound
    ci_lower = as.double(str_extract(att, "(?<=\\().+?(?=,)")),         # Extract upper bound
    ci_upper = as.double(str_extract(att, "(?<=, ).+?(?=\\))")),
    # rename model for reshape
    model = case_when(
      `Covariate adjustment` == "DiD" ~ 1,
      `Covariate adjustment` != "DiD" ~ 2,),
    n = rep(c(2:7, 1), each=2),
    label = fct_reorder(`Outcome metric`, desc(n))) %>%
  filter(model == 2)

p_te <- ggplot(stemp_table, 
  aes(x = estimate, y = label)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = ci_lower, 
    xmax = ci_upper), width=0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y = " ", x = "Total Effect (°C)") +
  scale_x_continuous(limits = c(-1, 6.1)) +
  theme_asm()
