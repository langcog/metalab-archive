#### LOAD DATA AND PACKAGES ####
#source("analyses/initial_data.R") # only do this if running script alone, otherwise leave commented


#### Effect size by N ####

bias_grid <-ggplot(all_data, aes(x = n, y = g_calc)) +
  facet_wrap(~dataset, scales = "free") +
  xlab("Sample size")  +
  ylab("Effect Size")  +
  geom_smooth(method = 'lm', se = F, colour = "darkgrey") +
  geom_point(size = .5) +
  theme_classic()