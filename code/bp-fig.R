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