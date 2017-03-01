#### LOAD DATA AND PACKAGES ####
#source("analyses/initial_data.R") # only do this if running script alone, otherwise leave commented


#### Effect size by N ####

bias_grid <-ggplot(all_data, aes(x = n, y = abs(d_calc))) +
  facet_wrap(~dataset, scales = "free") +
  xlab("Sample size")  +
  ylab("Effect Size")  +
  geom_smooth(method = 'lm', se = F, colour = "darkgrey") +
  geom_point(size = .5) +
  theme_classic()

# needs tweaking

data_bias = all_data %>%
  nest(-dataset, .key = information) %>%
  mutate(model = map(information, ~cor.test(abs(.$d_calc), .$n, method = "kendall"))) %>%
  mutate(p = map(model, "p.value")) %>%
  mutate(tau = map(model, "estimate"))  %>%
  select(dataset, tau, p) %>%
  unnest() %>%
  mutate(p = as.numeric(as.character(p))) %>%
  mutate(p = ifelse(p < .001, "< .001", as.character(round(p, 3)))) %>%
  mutate(tau = as.numeric(as.character(tau))) 

