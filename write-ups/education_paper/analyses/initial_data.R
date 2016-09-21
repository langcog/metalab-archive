## READ IN DATA ####
source("../../dashboard/global.R", chdir = TRUE)



## LOAD PACKAGES ####
library(metafor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)
library(knitr)
library(broom)
library(pwr)
library(lme4)


## CLEAN DATA ####
all_data = droplevels(all_data[all_data$dataset!="Statistical word segementation",])

#TODO: Remove outliers?
# clean_data = all_data %>%
#   nest(-dataset, .key = information) %>%
#   mutate(model = map(information, ~rma.mv(d_calc, sei=d_var_calc, random = ~ 1 | study_ID, data=.))) %>%
#   mutate(d = map(model, "b")) %>%
#   mutate(se = map(model, "se")) 
