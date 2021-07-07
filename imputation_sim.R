# new imputation bias simulation
# i want 20 minutiae in x and y
# fingerprints are represented by 50 sections, each made up of 4 pixels
# pixels ~ bern(I(is minutia)*.75 + I(no_minutia)*.1)

library(ggplot2)
library(tidyverse)
library(reshape2)

set.seed(123)

sections <- 50
num_min <- 20 
num_bits <- sections*4

x_big <- sample(c(1:sections), num_min)
y_big <- sample(c(1:sections), num_min)

gen_yi <- function(j){
  has_min <- as.numeric(ceiling(j/4) %in% y_big)
  p <- .9*has_min + .1*(1-has_min)
  rbinom(1,1,p)
}
y <- sapply(1:num_bits, gen_yi)
y_miss <- y
y_miss[sample(c(1:num_bits), 50)] <- NA

miss_plot_df <- data.frame(Var1 = rep(1:40, 5), 
                           Var2 = rep(1:5, each = 40),
                           value = y_miss,
                           Type = "1. Crime Scene Print (Y)")

ggplot(miss_plot_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + xlab(NULL) + ylab(NULL) + 
  ggtitle("Suspect print with missing pixels shown")

gen_xi <- function(j){
  has_min <- as.numeric(ceiling(j/4) %in% x_big)
  p <- has_min
  rbinom(1,1,p)
}
x <- sapply(1:num_bits, gen_xi)

y_impute <- sapply(1:num_bits, function(j) ifelse(is.na(y_miss[j]), x[j], y_miss[j]))
rnd.mean <- function(x) round(mean(x, na.rm = T))

X <- matrix(sapply(1:50, function(j) (j %in% x_big)*1), ncol = 5, byrow = T)
Y <- matrix(sapply(1:50, function(j) (j %in% y_big)*1), ncol = 5, byrow = T)
Y_est <- matrix(apply(matrix(y, ncol = 4, byrow = T), 1, rnd.mean), ncol = 5, byrow = T)
Y_miss <- matrix(apply(matrix(y_miss, ncol = 4, byrow = T), 1, rnd.mean), ncol = 5, byrow = T)
Y_imp <- matrix(apply(matrix(y_impute, ncol = 4, byrow = T), 1, rnd.mean), ncol = 5, byrow = T)


plot.df <- data.frame(rbind(melt(X),melt(Y_est),melt(Y_miss),melt(Y_imp)))
plot.df$Type <- rep(c("2. Marked Suspect Print (X)",
                      "3. Full Info Marked Y", 
                      "4. Marked Y without Imputation", 
                      "5. Marked Y with Imputation"), each = sections)
plot.df <- rbind(plot.df, miss_plot_df)
ggplot(plot.df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + xlab(NULL) + ylab(NULL) +
  facet_wrap(~Type, scales = "free_x") + theme_bw()

# break out figures
plot.df$type <- rep(c("Latent print (y)",
                      "Exemplar (x)",
                      "Criminal's true print", 
                      "4. Marked Y without Imputation", 
                      "Criminal's print marked"), each = sections)

plot.df$type <- ifelse(plot.df$Type == "2. Marked Suspect Print (X)", "Exemplar (x)",
                       ifelse(plot.df$Type == "3. Full Info Marked Y","Criminal's true print",
                              ifelse(plot.df$Type == "4. Marked Y without Imputation","4. Marked Y without Imputation",
                                     ifelse(plot.df$Type == "5. Marked Y with Imputation", "Criminal's marked print",
                                            "Criminal's latent print (y)"))))

plot.df %>%
  filter(type %in% c("Criminal's latent print (y)","Exemplar (x)")) %>%
  mutate(
    type = ifelse(type == "Exemplar (x)", " Exemplar (x)", type), # for ordering facets
    label = ifelse(value == 1, "*", NA) # Mark minutiae with *
  ) %>%
  ggplot(aes(x = Var1, y = Var2, fill = is.na(value))) +
  geom_tile() + 
  geom_text(aes(label = label)) +
  xlab(NULL) + 
  ylab(NULL) +
  facet_wrap(vars(type), scales = "free_x") + 
  scale_fill_grey(start = .8, end = 0, na.value = "black") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave("latentVsexemplar.png",width = 4, height = 2.5)

plot.df %>%
  filter(type %in% c("Criminal's true print","Exemplar (x)")) %>%
  mutate(
    type = ifelse(type == "Exemplar (x)", " Exemplar (x)", type), # for ordering facets
    label = ifelse(value == 1, "*", NA) # Mark minutiae with *
  ) %>%
  ggplot(aes(x = Var1, y = Var2, fill = is.na(value))) +
  geom_tile() + 
  geom_text(aes(label = label)) +
  xlab(NULL) + 
  ylab(NULL) +
  scale_fill_grey(start = .8, end = 0, na.value = "black") +
  facet_wrap(~type, scales = "free_x") + 
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave("trueVsexemplar.png",width = 4, height = 2.5)

plot.df %>%
  filter(type %in% c("Criminal's marked print","Exemplar (x)")) %>%
  mutate(
    type = ifelse(type == "Exemplar (x)", " Exemplar (x)", type), # for ordering facets
    label = ifelse(value == 1, "*", NA) # Mark minutiae with *
  ) %>%
  ggplot(aes(x = Var1, y = Var2, fill = is.na(value))) +
  geom_tile() + 
  geom_text(aes(label = label)) +
  xlab(NULL) + 
  ylab(NULL) +
  scale_fill_grey(start = .8, end = .2, na.value = "black") +
  facet_wrap(~type, scales = "free_x") + 
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave("markedVsexemplar.png",width = 4, height = 2.5)

sum(which(X==1)%in%which(Y==1)) # true matches
sum(which(X==1)%in%which(Y_est==1)) # matches marked without missingness
sum(which(X==1)%in%which(Y_miss==1)) # matches in the presence of missingness
sum(which(X==1)%in%which(Y_imp==1)) # matches with imputation

# show where improper match is
temp = Y
temp[(X==Y) &(X==1)] <- 10
temp[(X==1)&(Y_imp==1)&(Y_miss==0)] <- (-10)
ggplot(melt(temp), aes(x = Var1, y = Var2, fill = value)) + geom_tile()