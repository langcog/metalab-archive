## LOAD DATA AND PACKAGES ####
#source("analyses/initial_data.R") # only do this if running script alone, otherwise leave commented


## METHOD VS EXCLUDED ####
# Centering mean age
method_exclude_data = all_data %>%
  mutate(ageC = ifelse(participant_design == "between",
                       (((mean_age_1 - mean(mean_age_1)) + (mean_age_1 - mean(mean_age_1)))/2)/30.42,
                       (((mean_age_1 - mean(mean_age_1))))/30.42)) %>%
  mutate(keep = ifelse(is.na(n_2), n_1, n_1 + n_2)) %>%
  mutate(dropout = ifelse(is.na(n_excluded_1), NA, ifelse(is.na(n_excluded_2), n_excluded_1, n_excluded_1+n_excluded_2))) %>%
  mutate(total_run = keep + dropout) %>%
  filter(!is.na(dropout)) %>%
  mutate(percent_dropout = dropout / total_run) %>%
  group_by(method) %>%
  mutate(number = n()) %>%
  ungroup() %>%
  filter(number > 10) %>%
  mutate(method = factor(method)) %>%
  select(percent_dropout, keep, dropout, total_run, dataset, ageC, method, mean_age_months)

# Build LMER model
method_exclude.m <- lmer(percent_dropout ~ method * ageC +
                           (1|dataset), data = method_exclude_data)

# Save summary of model 
method_exclude.m_sum = summary(method_exclude.m)


## PLOT OF EFFECT OF METHOD ON DROPOUT RATE ####
# Make plot
method_exclude.plot = ggplot(method_exclude_data, aes(x = mean_age_months, y = percent_dropout, color = method)) +
  geom_smooth(method = "lm") +
  xlab("Mean age of participants in months") +
  ylab("Percent Dropout") +
  labs(color = "Method") +
  theme_classic() +
  theme(axis.line.x = element_line(), axis.line.y = element_line(),
        legend.position = "top")


## EFFECT OF METHOD ####
# Centering mean age
method_data = all_data %>%
  mutate(ageC = ifelse(participant_design == "between",
                       (((mean_age_1 - mean(mean_age_1)) + (mean_age_1 - mean(mean_age_1)))/2)/30.42,
                       (((mean_age_1 - mean(mean_age_1))))/30.42)) %>%
  group_by(dataset) %>%
  mutate(number = length(levels(as.factor(method)))) %>%
  ungroup() %>%
  filter(number > 2) %>%
  mutate(method = factor(method))

# Build model
method.rma <- rma.mv(d_calc, d_var_calc, mods = ~ageC * relevel(method, "central fixation"), random = ~ short_cite |  dataset, data = method_data)

# Save summary of model
method.rma_sum = summary(method.rma)


## PLOT OF EFFECT OF METHOD ####
# Make plot
method.plot = ggplot(method_data, aes(x = mean_age_months, y = d_calc, color = method)) +
  # Commenting out points for now since they make the rest of the graph kind of hard to see
  #geom_point() +
  geom_smooth(method = "lm") +
  xlab("Mean age of participants in months") +
  ylab("Effect size (d)") +
  xlim(0, 40) +
  ylim(-1.5, 3.3) +
  labs(color = "Method") +
  theme_classic() +
  theme(axis.line.x = element_line(), axis.line.y = element_line(),
        legend.position = "top")
