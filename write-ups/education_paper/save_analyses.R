## LOAD DATA AND PACKAGES ####
source("analyses/initial_data.R")


## RUN ANALYSES ####
source("analyses/sample_size.R")
source("analyses/power.R")
source("analyses/method.R")


## SAVE ENVIRONMENT FOR USE IN PAPER ####
save.image("educationpaper_environment.RData") # may need to change this file path slightly

