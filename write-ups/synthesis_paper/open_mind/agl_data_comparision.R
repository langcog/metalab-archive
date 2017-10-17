source("../../../dashboard/global.R", chdir = TRUE)

#m <- filter(all_data,dataset == "Statistical sound category learning")

m <- filter(all_data, short_name == "symbolism") %>%
  select(study_ID, x_1, x_2, d_calc, d_var_calc, condition_type, infant_type)

model = metafor::rma(d_calc, d_var_calc, data = m)
data.frame(dataset = ma_data$short_name[1],
           overall.d = model$b,
           ci_lower = model$ci.lb,
           ci_upper = model$ci.ub)

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