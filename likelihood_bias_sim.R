# Likelihood bias simulation
library(ggplot2)

# simple poisson
x_full <- rpois(100,2)
y <- rpois(100,5)
x <- x_full
missing <- sample(1:100, size = 90, replace = F)
x[missing] <- NA
filled.x <- x
filled.x[missing] <- sapply(missing, function(k) sample(c(y[k], NA), size = 1))

df <- data.frame(dat = c(x, filled.x, y), names = rep(c('1.obs', '2.filled','3.y'), each = 100))
levels(df$names) <- c("Observed","Observed After Imputation","Complete")

g <- ggplot(data = df) + 
  geom_histogram(aes(x = dat, y = ..density..), binwidth = 2) + 
  geom_density(aes(x = dat)) + 
  facet_wrap(~names) + 
  xlab("Comparing Y with X before and after imputing half of missing data points")+
  theme_bw()
ggsave(g, filename = '~/repos/Forensic_bias/outputs/likelihood_sim.png')
