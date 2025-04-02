library(tidyverse)
library(here)
library(readxl)

raw_data <- read_xlsx(here('data', 'raw-data-NZILBB-workshop.xlsx'), sheet=3)
corrected_data <- read_xlsx(here('data', 'NZILBB-workshop-R-exp-data.xlsx'))
