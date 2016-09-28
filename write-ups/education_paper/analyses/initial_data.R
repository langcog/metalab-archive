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
all_data = all_data[all_data$dataset!="Statistical word segementation",]


#This is a fix for mis-formatted data, waiting for the actual data to update.
all_data[all_data$dataset=="Sound symbolism",]$infant_type="typical"

all_data = all_data[all_data$infant_type == "typical",]

all_data = all_data %>%
  mutate(year = as.numeric(unlist(lapply(strsplit(unlist(study_ID),
                                                  "[^0-9]+"),  function(x) unlist(x)[2])))) %>%
  mutate(year = ifelse(grepl("submitted",study_ID), 2016, year)) %>%
  mutate(year = ifelse(dataset == "Phonotactic learning" | dataset == "Statistical sound category learning", 
                       as.numeric(unlist(lapply(strsplit(unlist(short_cite),"[^0-9]+"),  function(x) unlist(x)[2]))), year))

#Remove outliers

clean_data = all_data %>%
  group_by(dataset) %>%
  mutate(mean_es = median(d_calc)) %>%
  mutate(sd_es = sd(d_calc)) %>%
  ungroup() %>%
  mutate(no_outlier = ifelse(d_calc < mean_es+3*sd_es, ifelse(d_calc > mean_es-3*sd_es, TRUE, FALSE), FALSE))  %>%
  filter(no_outlier)

#Comment out if you do not want to remove outliers
all_data = clean_data
