# Likelihood bias simulation
library(ggplot2)
library(tidyverse)

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

df <- df %>%
  mutate(
    labels = fct_recode(names,
                        "Latent (Y)" = "1.obs",
                        "Imputed Latent (Y*)" = "2.filled",
                        "True Criminal Print (Y)" = "3.true x",
                        "Exemplar (X)" = "4.y")
  ) 

miss_plot_df <- data.frame(Var1 = rep(rep(1:10, 10),4), 
                           Var2 = rep(rep(1:10, each = 10),4),
                           value = df$dat,
                           Type = df$labels)

g1 <- miss_plot_df %>%
  filter(Type %in% c("True Criminal Print (Y)","Exemplar (X)")) %>%
  mutate(
    Type = ifelse(Type == "Exemplar (X)", " Exemplar (X)", as.character(Type)), # for ordering facets
    minutiae = ifelse(value == 1, "*", NA) # Mark minutiae with *
  ) %>%
  ggplot(aes(x = Var1, y = Var2, fill = is.na(value))) +
    geom_tile() + xlab(NULL) + ylab(NULL) + 
    facet_grid(~Type) +
    geom_text(aes(label = minutiae)) +
    xlab(NULL) + 
    ylab(NULL) +
    ggtitle("Full information prints") + 
    scale_fill_grey(start = .8, end = 0, na.value = "black") +
    theme_minimal() +
    theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

g2 <- miss_plot_df %>%
  filter(Type %in% c("Latent (Y)","Exemplar (X)")) %>%
  mutate(
    Type = ifelse(Type == "Exemplar (X)", " Exemplar (X)", as.character(Type)), # for ordering facets
    minutiae = ifelse(value == 1, "*", NA) # Mark minutiae with *
  ) %>%
  ggplot(aes(x = Var1, y = Var2, fill = is.na(value))) +
  geom_tile() + xlab(NULL) + ylab(NULL) + 
  geom_text(aes(label = minutiae)) +
  facet_grid(~Type) +
  ggtitle("Full information prints") + 
  scale_fill_grey(start = .8, end = 0, na.value = "black") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

g3 <- miss_plot_df %>%
  filter(Type %in% c("Imputed Latent (Y*)","Exemplar (X)")) %>%
  mutate(
    Type = ifelse(Type == "Exemplar (X)", " Exemplar (X)", as.character(Type)), # for ordering facets
    minutiae = ifelse(value == 1, "*", NA) # Mark minutiae with *
  ) %>%
  ggplot(aes(x = Var1, y = Var2, fill = is.na(value))) +
  geom_tile() + xlab(NULL) + ylab(NULL) + 
  geom_text(aes(label = minutiae)) +
  facet_grid(~Type) +
  ggtitle("Exemplar vs. Imputed Print") + 
  scale_fill_grey(start = .8, end = 0, na.value = "black") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave(g1, filename = '~/repos/Forensic_bias/outputs/simple_imputation1.png')
ggsave(g2, filename = '~/repos/Forensic_bias/outputs/simple_imputation2.png')
ggsave(g3, filename = '~/repos/Forensic_bias/outputs/simple_imputation3.png')


#AL Save
ggsave(g1, filename = '~/Forensic_bias/outputs/simple_imputation1.png')
ggsave(g2, filename = '~/Forensic_bias/outputs/simple_imputation2.png')
ggsave(g3, filename = '~/Forensic_bias/outputs/simple_imputation3.png')
