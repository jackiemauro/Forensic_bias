# new imputation bias simulation
# i want 20 minutiae in x and y
# fingerprints are represented by 50 sections, each made up of 4 pixels
# pixels ~ bern(I(is minutia)*.75 + I(no_minutia)*.1)

library(ggplot2)
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
plot.df$Type <- rep(c("X","True Y", "Missing Y", "Imputed Y"), each = sections)
ggplot(plot.df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + xlab(NULL) + ylab(NULL) +
  facet_wrap(~Type)

sum(which(X==1)%in%which(Y==1))
sum(which(X==1)%in%which(Y_est==1))
sum(which(X==1)%in%which(Y_miss==1))
sum(which(X==1)%in%which(Y_imp==1))