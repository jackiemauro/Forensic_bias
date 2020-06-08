# Snowball bias

rm(list = ls())
library(ggplot2)
library(dplyr)

# basics
N = 10 #number of suspects
Pi = .15 #share in pop w I=1
Pi_E = Pi #prob I=1 if E0 (set to constant over pieces of evidence for simplicity)
Pi_EC = Pi #prob I=1 if E0^C (set to constant over pieces of evidence for simplicity)
Prior = 1/N

# 6 kinds of bias

# how do they incorporate true m? (assume corectly incorporates true matches (so non-tilde biases are the same as for cascade)
mtrue.func <- function(m) 0 

# bias if see true matches
d_impute <- function(i,m, share.miss){ share.miss + i/2 + 1 + mtrue.func(m)}
d_I <- function(i,m){ (2-(1-i)/(1-Pi)) + mtrue.func(m) }
d_peer <- function(m){1 + mtrue.func(m)}
delta.func <- function(i,m, share.miss) {d_impute(i, m, share.miss)*d_I(i,m)*d_peer(m)}

# how do they incorporate estimated m? 
m.func <- function(m) sum(m>=1) 

# bias if see estimated matches
dt_impute <- function(i,m,share.miss){ 1 }
dt_I <- function(i,m){ 1 }
dt_peer <- function(m){m.func(m) + 1}
delta.tilde.func <- function(i,m,share.miss) {dt_impute(i, m, share.miss)*dt_I(i,m)*dt_peer(m)}

total.bias <- function(i,m,mhat,share.miss){delta.func(i,m,share.miss)*delta.tilde.func(i,mhat,share.miss)}

g = 0 # 0 means not guilty
i = 1 # 1 means has characteristic
k = 10 # pieces of evidence

# evidence is binom(10,mu1) if guilty, binom(10,mu0) if not guilty
mu1 = .5; mu0=.25

nsims = 1000
output = list()
for(sim.round in 1:nsims){
  xy = sapply(1:k, function(j) ifelse(g==1, sum(rbinom(10,1,mu1)), sum(rbinom(10,1,mu0))))
  lik.ratio = sapply(xy, function(j) dbinom(j,10,mu1)/dbinom(j,10,mu0))
  
  # missing evidence (decides degree of imputation)
  share.miss = rbeta(k,1,3)
  
  mhat <- m <- m.cascade <- rep(NA,k)
  m <- sapply(1:k, function(x) lik.ratio[x] * (Pi_E/Pi_EC) * 1/N)
  m.cascade <- sapply(1:k, function(x) delta.func(i,m[x],share.miss[x])*m[x])
  mhat[1] <- m.cascade[1]
  
  for(j in 2:k){mhat[j] <- total.bias(i,m[c(1:(j-1))],mhat[c(1:(j-1))], share.miss[j])*m[j]}
  
  output[[sim.round]] <- data.frame(posterior = c(m,m.cascade,mhat),
                       type = rep(c("Unbiased","Cascade","Snowball"), each = k),
                       evidence = rep(1:k,3),
                       round = rep(i,k*3))
}
sim.out = do.call(rbind, output)
summary = sim.out %>%
  group_by(type,evidence) %>%
  summarise(share_declared_match = mean(posterior>=1))

g <- ggplot(summary, aes(x = evidence, y = share_declared_match, fill = type))+
  geom_bar(stat = 'identity', position = 'dodge')+
  xlab("Piece of Evidence") + ylab("Share declared a match")+
  theme_bw()

g

#ggsave(filename = '~/repos/Forensic_bias/outputs/SnowballFigure.png')
