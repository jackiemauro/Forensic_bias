library(tidyverse)

PS = 15
PF = 10
k = 30
n = 100
bias = 1.25 # multiplicative factor for how often suspects convicted

set.seed(1950)
longrun_bias = tibble(
  x = seq(.001, .999, .001), ##Set up for creating the distributions
  prior_unobscured = dbeta(x, PS, PF), # data for prior curve
  posterior_unobscured = dbeta(x, PS + k, PF + n - k), # data for posterior curve
  likelihood_unobscured = dbeta(x, 1 + k, 1 + n - k), # data for likelihood curve, plotted as the posterior from a beta(1,1)
  likelihood_obscured = dbeta(x, 1 + bias*k, 1 + n - bias*k),
  posterior_obscured = dbeta(x, PS + bias*k, PF + n - bias*k)
) %>%
  mutate(prior_obscured = prior_unobscured)

longrun_bias %>%
  pivot_longer(-x, names_to = "distribution", values_to = "y") %>%
  separate(distribution, into = c("distribution", "type")) %>%
  mutate(
    type_label = ifelse(type == "unobscured", "100 rounds: Full Exposition", "100 rounds: Truth Obscured")
  ) %>%
  ggplot(aes(x = x, y = y, col = distribution, linetype = distribution)) + 
  geom_line() + 
  facet_wrap(vars(type_label)) + 
  geom_vline(xintercept = .3, col = "gray50") + 
  annotate("text", x = .32, y = 11, 
           label = "p[true]== .3", 
           parse = TRUE, 
           col = "gray50",
           hjust = 0) + 
  theme_minimal() +
  scale_color_grey(start = .1, end = .6) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 12)) +
  ylim(0,12) +
  labs(
    x = "",
    y = "Density",
    col = "",
    linetype = ""
  )

ggsave(filename = '~/Forensic_bias/outputs/long_run_posterior_bias.eps',
       width = 8,
       height = 3,
       dpi = 300)


## Old version: base R

plot(x, y1, xlim=c(0,1), ylim=c(0, 10), type = "l", ylab= "Density", lty = 2,
     xlab= "Probability of success", las=1, main= Title,lwd=3,
     cex.lab=1.5, cex.main=1.5, col = "skyblue")
lines(x, y2, type = "l", col = "darkorange", lwd = 2, lty = 3)
lines(x, y3, type = "l", col = "darkorchid1", lwd = 5)
legend("topleft", c("Prior", "Posterior", "Likelihood"), col = c("skyblue", "darkorchid1", "darkorange"), 
       lty = c(2,1,3), lwd = c(3,5,2), bty = "n", y.intersp = .55, x.intersp = .1, seg.len=.7)

plot(x, y1, xlim=c(0,1), ylim=c(0, 10), type = "l", ylab= "Density", lty = 2,
     xlab= "Probability of success", las=1, main= Title,lwd=3,
     cex.lab=1.5, cex.main=1.5, col = "skyblue")
lines(x, y2.bias, type = "l", col = "darkorange", lwd = 2, lty = 3)
lines(x, y3.bias, type = "l", col = "darkorchid1", lwd = 5)
legend("topleft", c("Prior", "Posterior", "Likelihood"), col = c("skyblue", "darkorchid1", "darkorange"), 
       lty = c(2,1,3), lwd = c(3,5,2), bty = "n", y.intersp = .55, x.intersp = .1, seg.len=.7)
