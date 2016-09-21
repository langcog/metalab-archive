## LOAD DATA AND PACKAGES ####
#source("analyses/initial_data.R") # only do this if running script alone, otherwise leave commented


## COMPUTE META-ANALYTIC D FOR ALL PAPERS ####
# Make function
overall_es <- function(ma_data){
  model = metafor::rma.mv(ma_data$d_calc, ma_data$d_var_calc, , random = ~ 1 | study_ID, method = "REML",
                          control = list(maxiter = 1000, stepadj = 0.5))
  data.frame(dataset = ma_data$dataset[1],
             overall.d = model$b,
             ci_lower = model$ci.lb,
             ci_upper = model$ci.ub)
}

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

# Get subset of summary data
MA_summary_sub = select(MA_summary, c(dataset, n_dataset))

# Combine summary with oldest paper
d_comparison = inner_join(oldest, MA_summary_sub) %>%
  select(dataset, short_cite, largest_d, n_dataset)

# Include power
d_comparison_power = d_comparison %>%
  nest(-dataset, .key = descriptives) %>%
  mutate(power = map(descriptives, get_power_oldest)) %>%
  mutate(power = map(power, "power")) %>%
  select(dataset, power) %>%
  mutate(power = as.numeric(as.character(power)))

# Save overall summary
d_comparison_summary = inner_join(d_comparison, d_comparison_power)


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