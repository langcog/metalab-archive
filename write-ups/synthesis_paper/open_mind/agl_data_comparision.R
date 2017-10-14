source("../../../dashboard/global.R", chdir = TRUE)

m <- filter(all_data,dataset == "Statistical sound category learning")

m_clean <- m %>%
  select(study_ID, 
         #EffectSize, 
         #ES.SE, 
         #ES.w, 
         #ABS_EffectSize, 
         #SDpooled, 
         es_method,
         d_calc, 
         d_var_calc)