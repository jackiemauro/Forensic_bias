# Snowball bias

################ simple simulation ##########
# fix this to match math in paper

# truth
guilty <- 0
I <- 1
p1 <- .5 #prob evidence|guilty=1 (true positive)
p2 <- .05 #prob evidence|guilty=0 (false positive)
p_guilty <- .05 #baseline prob of guilty (1/N, eg)
i_bias <- .05 #bias from I
e_bias <- .2 #bias spilling over from other evidence

p_evidence <- function(g){p1*g + p2*(1-g)}
post_guilty <- function(e1,e2,e3,p_guilty){
  #posterior prob
  (p1*e1 + (1-p1)*(1-e1))*(p1*e2 + (1-p1)*(1-e2))*(p1*e3 + (1-p1)*(1-e3))*p_guilty/
    ((p1*e1 + (1-p1)*(1-e1))*(p1*e2 + (1-p1)*(1-e2))*(p1*e3 + (1-p1)*(1-e3))*p_guilty+
       (p2*e1 + (1-p2)*(1-e1))*(p2*e2 + (1-p2)*(1-e2))*(p2*e3 + (1-p2)*(1-e3))*(1-p_guilty))
}
bound_01 <- function(x){min(max(x,0),1)}

# across n parameter values & N "trials"
n <- 10
N <- 1000
out_biased <- out_unbiased <- matrix(rep(NA,n*4), ncol = 4)
param <- seq(0,1,length.out=n)
for(j in 1:n){
  e_bias <- param[j]
  errs <- errs_unbiased <- matrix(rep(NA,N*4), ncol = 4)
  for(i in 1:N){
    # biased evidence
    e1 <- rbinom(1,1,bound_01(p_evidence(guilty) + i_bias*I))
    e2 <- rbinom(1,1,bound_01(p_evidence(guilty) + e_bias*e1 + i_bias*I))
    e3 <- rbinom(1,1,bound_01(p_evidence(guilty) + e_bias*e1 + e_bias*e2 + i_bias*I))
    
    # unbiased evidence
    e1_unbiased <- rbinom(1,1,bound_01(p_evidence(guilty)))
    e2_unbiased <- rbinom(1,1,bound_01(p_evidence(guilty)))
    e3_unbiased <- rbinom(1,1,bound_01(p_evidence(guilty)))
    
    # final determination
    determined_guilt <- post_guilty(e1,e2,e3,p_guilty)>.5
    determined_guilt_unbiased <- post_guilty(e1_unbiased,e2_unbiased,e3_unbiased,p_guilty)>.5
    
    # errors
    errs[i,] <- c((guilty-determined_guilt)^2,e1,e2,e3)
    errs_unbiased[i,] <- c((guilty-determined_guilt_unbiased)^2,e1_unbiased,e2_unbiased,e3_unbiased)
  }
  
  out_biased[j,] <- apply(errs,2,mean)
  out_unbiased[j,] <- apply(errs_unbiased,2,mean)
}
library(ggplot2)
ggplot(data = data.frame(posterior = c(out_biased[,1],out_unbiased[,1]), type = rep(c('biased','unbiased'), each = n), x = rep(param,2)))+
  geom_line(aes(x=x,y=posterior,color=type))+
  theme_bw()+
  xlab("Peer Influence Parameter") + ylab("Posterior")


######### alternate #########
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

# how do they incorporate true m (assume corectly incorporates true matches (so non-tilde biases are the same as for cascade)
mtrue.func <- function(m) 0 

# bias if see true matches
d_impute <- function(i,m, share.miss){ share.miss + i/2 + 1 + mtrue.func(m)}
d_I <- function(i,m){ (2-(1-i)/(1-Pi)) + mtrue.func(m) }
d_peer <- function(m){1 + mtrue.func(m)}
delta.func <- function(i,m, share.miss) {d_impute(i, m, share.miss)*d_I(i,m)*d_peer(m)}

# how do they incorporate estimated m? 
m.func <- function(m) .2*sum(m>=1) 

# bias if see estimated matches
dt_impute <- function(i,m,share.miss){ share.miss + i/2 + m.func(m)}
dt_I <- function(i,m){ 1 - (1-i)/(1-Pi) + 2*m.func(m) }
dt_peer <- function(m){m.func(m) + 1}
delta.tilde.func <- function(i,m,share.miss) {dt_impute(i, m, share.miss)*dt_I(i,m)*dt_peer(m)}

total.bias <- function(i,m,mhat,share.miss){delta.func(i,m,share.miss)*delta.tilde.func(i,mhat,share.miss)}

g = 0 # 0 means not guilty
i = 1 # 1 means has characteristic
k = 5 # pieces of evidence

# evidence is binom(10,mu1) if guilty, binom(10,mu0) if not guilty
mu1 = .6; mu0=.4

nsims = 100
output = list()
temp = c()
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
  theme_bw()

g
