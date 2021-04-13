# Likelihood bias simulation
library(ggplot2)
set.seed(123)

# simple poisson
x_full <- rpois(100,2)
y <- rpois(100,5)
x <- x_full
missing <- sample(1:100, size = 25, replace = F)
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


miss_plot_df <- data.frame(Var1 = rep(rep(1:10, 10),3), 
                           Var2 = rep(rep(1:10, each = 10),3),
                           value = df$dat,
                           Type = df$names)

ggplot(miss_plot_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + xlab(NULL) + ylab(NULL) + 
  facet_grid(~Type) +
  ggtitle("Prints before and after imputation")

#### changing to binary #####
x_full <- rbinom(100,1,.25)
y <- rbinom(100,1,.25)
x <- x_full
missing <- sample(1:100, size = 25, replace = F)
x[missing] <- NA
filled.x <- x
filled.x[missing] <- sapply(missing, function(k) sample(c(2*y[k], NA), size = 1, prob = c(.75,.25)))
filled.x[missing] <- sapply(missing, function(k) 2*y[k])

df <- data.frame(dat = c(x, filled.x, y,x_full), names = rep(c('1.obs', '2.filled','3.true x','4.y'), each = 100))
levels(df$names) <- c("Latent (Y)","Imputed Latent (Y*)", "True Criminal Print (Ytrue)","Exemplar (X)")

miss_plot_df <- data.frame(Var1 = rep(rep(1:10, 10),4), 
                           Var2 = rep(rep(1:10, each = 10),4),
                           value = df$dat,
                           Type = df$names)

g1 <- ggplot(miss_plot_df[miss_plot_df$Type %in% c("True Criminal Print (Ytrue)","Exemplar (X)"),]
             , aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() + xlab(NULL) + ylab(NULL) + 
    facet_grid(~Type) +
    theme(legend.position = "none") +
    ggtitle("Full information prints")

g2 <- ggplot(miss_plot_df[miss_plot_df$Type %in% c("Latent (Y)","Exemplar (X)"),]
             , aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + xlab(NULL) + ylab(NULL) + 
  facet_grid(~Type) +
  theme(legend.position = "none") +
  ggtitle("Exemplar vs. Crime Scene Print")

g3 <- ggplot(miss_plot_df[miss_plot_df$Type %in% c("Imputed Latent (Y*)","Exemplar (X)"),]
             , aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + xlab(NULL) + ylab(NULL) + 
  facet_grid(~Type) +
  theme(legend.position = "none") +
  ggtitle("Exemplar vs. Imputed Print")

ggsave(g1, filename = '~/repos/Forensic_bias/outputs/simple_imputation1.png')
ggsave(g2, filename = '~/repos/Forensic_bias/outputs/simple_imputation2.png')
ggsave(g3, filename = '~/repos/Forensic_bias/outputs/simple_imputation3.png')
