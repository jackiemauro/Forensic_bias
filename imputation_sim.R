# new imputation bias simulation
# i want 20 minutiae in x and y
# fingerprints are represented by 50 sections, each made up of 4 pixels
# pixels ~ bern(I(is minutia)*.75 + I(no_minutia)*.1)

library(ggplot2)
library(tidyverse)
library(reshape2)

set.seed(123)

sections <- 50
num_min <- 15 
num_bits <- sections # change to sections*4 if want finer grid

x_big <- sample(c(1:sections), num_min)
y_big <- sample(c(1:sections), num_min)

gen_yi <- function(j){
  has_min <- as.numeric(j %in% y_big)
  p <- has_min # changed from .9*has_min + .1*(1-has_min) for paper
  rbinom(1,1,p)
}
y <- sapply(1:num_bits, gen_yi)
y_miss <- y
y_miss[sample(c(1:num_bits), num_bits/4)] <- NA

miss_plot_df <- data.frame(Var1 = rep(1:10, 5), 
                           Var2 = rep(1:5, each = 10),
                           value = y_miss,
                           Type = "1. Crime Scene Print (Y)")

ggplot(miss_plot_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + xlab(NULL) + ylab(NULL) + 
  ggtitle("Suspect print with missing pixels shown")

gen_xi <- function(j){
  has_min <- as.numeric(j %in% x_big)
  p <- has_min
  rbinom(1,1,p)
}
x <- sapply(1:num_bits, gen_xi)

# old y_impute: 
y_impute <- sapply(1:num_bits, function(j) ifelse(is.na(y_miss[j]), x[j], y_miss[j]))
rnd.mean <- function(x) round(mean(x, na.rm = T))

X <- matrix(sapply(1:50, function(j) (j %in% x_big)*1), ncol = 5, byrow = T)
Y <- matrix(sapply(1:50, function(j) (j %in% y_big)*1), ncol = 5, byrow = T)
#Y_est <- matrix(apply(matrix(y, ncol = 4, byrow = T), 1, rnd.mean), ncol = 5, byrow = T)
#Y_miss <- matrix(apply(matrix(y_miss, ncol = 4, byrow = T), 1, rnd.mean), ncol = 5, byrow = T)
#Y_imp <- matrix(apply(matrix(y_impute, ncol = 4, byrow = T), 1, rnd.mean), ncol = 5, byrow = T)

Y_est <- matrix(y_miss, ncol = 5, byrow = T)
Y_est[is.na(Y_est)] = 0 # without imputation, missing areas are filled with 0
Y_miss <- matrix(y_miss, ncol = 5, byrow = T)
Y_imp <- matrix(y_impute, ncol = 5, byrow = T)

# data.frame(rbind(melt(X),melt(Y_est),melt(Y_miss),melt(Y_imp))) 
plot.df <- tibble(
  x = x, 
  y = y,
  y_est = y_miss,
  y_miss = y_miss,
  y_imp = y_impute,
  x_pos = rep(1:5, each = 10),
  y_pos = rep(1:10, times = 5)
)

#plot.df$Type <- rep(c("2. Marked Suspect Print (X)",
#                      "3. Full Info Marked Y", 
#                      "4. Marked Y without Imputation", 
#                      "5. Marked Y with Imputation"), each = sections)
#plot.df <- rbind(plot.df, miss_plot_df)
#ggplot(plot.df, aes(x = Var2, y = Var1, fill = value)) +
#  geom_tile() + xlab(NULL) + ylab(NULL) +
#  facet_wrap(~Type, scales = "free") + theme_bw()

plot.df = plot.df %>% 
  pivot_longer(-c(x_pos, y_pos), names_to = "Type")


plot.df$type <- ifelse(plot.df$Type == "x", "x, exemplar",
                       ifelse(plot.df$Type == "y","y, true print",
                              ifelse(plot.df$Type == "y_est","4. Marked Y without Imputation",
                                     ifelse(plot.df$Type == "y_imp", "y, latent (marked)",
                                            "y, crime scene print"))))

p1 = plot.df %>%
  filter(type %in% c("y, true print","x, exemplar")) %>%
  mutate(
    #type = ifelse(type == "Exemplar (x)", " Exemplar (x)", type), # for ordering facets
    label = ifelse(value == 1, "m", NA) # Mark minutiae with *
  ) %>%
  ggplot(aes(x = x_pos, y = y_pos, fill = is.na(value))) +
  geom_tile(col = "black") + 
  geom_text(aes(label = label)) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Actual Prints.") +
  scale_fill_grey(start = .8, end = 0, na.value = "black") +
  facet_wrap(~type, scales = "free_x") + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

p2 = plot.df %>%
  filter(type %in% c("y, crime scene print","x, exemplar")) %>%
  mutate(
    type = ifelse(type == "x, exemplar", " x, exemplar", type), # for ordering facets
    label = ifelse(value == 1, "m", NA) # Mark minutiae with *
  ) %>%
  ggplot(aes(x = x_pos, y = y_pos, fill = is.na(value))) +
  geom_tile(col = "black") + 
  geom_text(aes(label = label)) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Evidence given to analyst.") +
  facet_wrap(vars(type), scales = "free") + 
  scale_fill_grey(start = .8, end = 0, na.value = "black") +
  scale_color_manual(values = c("lightgray", "black")) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

p3 = plot.df %>%
  filter(type %in% c("y, latent (marked)","x, exemplar")) %>%
  mutate(
    #type = ifelse(type == "Exemplar (x)", " Exemplar (x)", type), # for ordering facets
    label = ifelse(value == 1, "m", NA) # Mark minutiae with *
  ) %>%
  ggplot(aes(x = x_pos, y = y_pos, fill = is.na(value))) +
  geom_tile(col = "black") + 
  geom_text(aes(label = label)) +
  ggtitle("Evidence marked by analyst \n(by observing both prints simultaneously).") +
  xlab(NULL) + 
  ylab(NULL) +
  scale_fill_grey(start = .8, end = .2, na.value = "black") +
  facet_wrap(~type, scales = "free_x") + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

(p1 + p2 + p3) & theme(plot.title = element_text(hjust =.5))

ggsave(filename = 'outputs/imputation-sim.eps', width = 10, height = 3, units = "in", dpi = 300)


sum(which(X==1)%in%which(Y==1)) # true matches
sum(which(X==1)%in%which(Y_est==1)) # matches marked without missingness
sum(which(X==1)%in%which(Y_miss==1)) # matches in the presence of missingness
sum(which(X==1)%in%which(Y_imp==1)) # matches with imputation

# show where improper match is
temp = Y
temp[(X==Y) &(X==1)] <- 10
temp[(X==1)&(Y_imp==1)&(Y_miss==0)] <- (-10)
ggplot(melt(temp), aes(x = Var1, y = Var2, fill = value)) + geom_tile()