#### LOAD DATA AND PACKAGES ####
#source("analyses/initial_data.R") # only do this if running script alone, otherwise leave commented


#### Recompute p-values ####
#This code is based on p-curve caulculations, but I am not sure how reliable it is. So I am using the simple version below
# pbound <- function(p) pmin(pmax(p, 2.2e-16), 1-2.2e-16)
# all_data = all_data %>% # convert rs and ts to fs, then get p-values
#   mutate(f.value = ifelse(!is.na(r), (r ^ 2 * (n_1 -2))/ (1 - r ^ 2), F)) %>% # from Sirkin ("Statistics for the Social Sciences", pg. 505)
#   mutate(f.value = ifelse(is.na(f.value) & !is.na(t), t**2, f.value)) %>%
#   mutate(N_total = ifelse(is.na(n_2), n_1*2, n_1 + n_2),
#          f.transform = ifelse(is.na(f.value), ((d_calc/2)^2) * N_total, NA)) %>% # convert missing Fs from d_calc
#   mutate(f.value = ifelse(is.na(f.value), f.transform, f.value)) %>%
#   filter(!is.na(f.value)) %>%
#   mutate(df2 = ifelse(participant_design == "between", (n_1 + n_2)-2, n_1-1),
#          df1 = 1,
#          p_calc2 = pbound(1 - pf(f.value, df1 = df1, df2 = df2))) %>%
#   mutate(sig = ifelse(p < .05, TRUE, FALSE))



all_data = all_data %>%
mutate(t_calc = (r_calc/(sqrt((1-(r_calc^2))/(n-2))))) %>%
mutate(p_calc = 2-(2*(pt(abs(t_calc), n-1)))) %>%
mutate(sig = ifelse(p_calc < .05, TRUE, FALSE))


#plotting effect sizes against p-values to display their relationship

sig_plot <- ggplot(all_data, aes(p_calc, abs(d_calc)),group_by(sig)) +
  geom_hline(yintercept = 1, size = 8, colour = "lightgrey") +
  geom_vline(xintercept = .05) +
  geom_point(size = all_data$n/20, alpha = .5) +
  xlab("Recalculated p-value") +
  ylab("Absolute value of Cohen's d") +
  theme_classic() +
  theme(axis.line.x = element_line(), axis.line.y = element_line())


#### P-curve ####

p_data = all_data %>%
  filter(p_calc < .2) %>%
  group_by(dataset) %>%
  mutate(number = n()) %>%
  mutate(n_below = sum(p_calc<.05))%>%
  mutate(n_above = sum(p_calc>.05))%>%
  mutate(percent_below = n_below/number) %>%
  ungroup() %>%
  filter(number > 10) 

pcurve.plot <- ggplot(p_data, aes(p_calc)) +
  facet_wrap(~dataset, scales = "free") +
  geom_vline(xintercept = .05) +
  geom_density() +
  xlim(0, .2) +
  xlab("p-value (recalculated)")  +
  ylab("Counts")  +
  theme_classic()

pcurve.plot
