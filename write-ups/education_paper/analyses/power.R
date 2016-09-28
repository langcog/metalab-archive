## LOAD DATA AND PACKAGES ####
#source("analyses/initial_data.R") # only do this if running script alone, otherwise leave commented


## COMPUTE POWER FOR ALL PAPERS ####

get_power_oldest = function(df){
  pwr.t.test(n = df$n_dataset, d = df$largest_d, sig.level = 0.05)
}

# Compute oldest paper
oldest = all_data %>%
  group_by(dataset, short_cite) %>%
  summarise(year = max(year),
            largest_d = max(d_calc)) %>%
  ungroup() %>%
  group_by(dataset) %>%
  arrange(year) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Combine summary with oldest paper
d_comparison = inner_join(oldest, MA_summary) %>%
  select(dataset, short_cite, largest_d, n_dataset, power)

# Include power
d_comparison_power = d_comparison %>%
  nest(-dataset, .key = descriptives) %>%
  mutate(power = map(descriptives, get_power_oldest)) %>%
  mutate(old_power = map(power, "power")) %>%
  select(dataset, old_power) %>%
  mutate(old_power = as.numeric(as.character(old_power)))

# Save overall summary
d_comparison_summary = inner_join(d_comparison, d_comparison_power) %>%
  mutate(difference = old_power-as.numeric(power)) %>%
  select(-power)


## PLOT OF DIFFERENCE OF D VALUES ####
# Get data ready for figure
d_comparison_full = inner_join(d_comparison, MA_summary) %>%
  select(dataset, n_dataset, d, largest_d) %>%
  mutate(diff_d = abs(largest_d) - abs(d))

# Make plot
d_comparison_diff.plot = ggplot(d_comparison_full, aes(x = largest_d, y = diff_d)) +
  geom_point(aes(color = dataset)) +
  geom_smooth(method = "lm", color = "black") +
  xlab("Largest d for Oldest Paper") +
  ylab("Difference between largest d and meta-analytic d") +
  labs(color = "Meta-analysis") +
  theme_classic() +
  theme(axis.line.x = element_line(), axis.line.y = element_line(),
        legend.position = "top")


## Power / ES over time ##






# Very simplistic, just look for a general effect of year.
power_year = rma.mv(d_calc, d_var_calc, mods = ~year, random = ~ short_cite | dataset, data = all_data)

# Make plot

power_year.plot = ggplot(all_data , aes(x = year, y = d_calc, color = dataset)) +
  geom_smooth(method = "lm") +
  xlab("Publication year") +
  ylab("Effect size (d)") +
  theme_classic() +
  theme(axis.line.x = element_line(), axis.line.y = element_line(),
        legend.position = "top")


# Alternative analysis taking into account method and age

full.model = rma.mv(d_calc, d_var_calc, mods = ~mean_age_1+method,
                    random = ~ short_cite | dataset, data = all_data)

predicted = predict(full.model)

all_data = all_data %>%
  bind_cols(as.data.frame(predicted$pred),
            as.data.frame(predicted$se)) %>%
  rename(predicted_d = `predicted$pred`, 
         predicted_se = `predicted$se`) 

power_estimate = pwr.t.test(n = all_data$n, d = all_data$predicted_d, sig.level = 0.05)$power

all_data = cbind(all_data, power_estimate)


power_year.plot = ggplot(all_data , aes(x = year, y = power_estimate, color = dataset)) +
  geom_smooth(method = "lm") +
  xlab("Publication year") +
  ylab("Estimated Power") +
  theme_classic() +
  theme(axis.line.x = element_line(), axis.line.y = element_line(),
        legend.position = "top")

