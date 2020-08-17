# cascade bias: bias in one analyst's brain
rm(list = ls())

library(ggplot2)
N = seq(2, 50, by = 1)
delta = c(.1, .5, 1.5, 2, 5)

expand.grid(n_suspects = N, delta = delta) %>%
  mutate( # True probabilities
    Pi = .15,
    Pi_E = Pi_E,
    Pi_EC = Pi_EC
  ) %>%
  mutate(
    PAi_E =  delta * Pi, #analyst's belief (biased)
    PAi_EC = Pi_EC, #analyst's belief (unbiased)
    Pyx_IE = .5, # P(match|same source, I)
    Pyx_IEC = .05, # P(match|diff sources, I)
    Prior = 1/n_suspects
  ) %>%
  mutate(
    Posterior = (Pyx_IE/Pyx_IEC)*(Pi_E/Pi_EC)*Prior, # True Posterior
    Biased_Posterior = (Pyx_IE/Pyx_IEC)*(PAi_E/PAi_EC)*Prior # Biased Posterior
  ) %>% 
  mutate(delta = as.factor(delta)) %>% 
  ggplot(., aes(x = Posterior, y = Biased_Posterior, col = delta, group = delta)) + 
  geom_line() +
  scale_color_viridis_d(option = "magma", end = .75) +
  geom_abline(slope = 1, intercept = 0, lty = "dashed", col = "darkgray") + 
  theme_bw() 

ggsave(filename = 'outputs/CascadeFigure.png')

