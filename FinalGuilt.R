# Final guilt
# not done

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
k = 5 # pieces of evidence

# evidence is binom(10,mu1) if guilty, binom(10,mu0) if not guilty
mu1 = .5; mu0=.25

check_probs <- function(){
  nsims = 100
  lik1 <- lik0 <- m1 <- m0 <- mc1 <- mc0 <- mhat1 <- mhat0 <- data.frame(matrix(rep(NA,nsims*k),ncol=k))
  for(sim.round in 1:nsims){
    xyG1 = sapply(1:k, function(k) sum(rbinom(10,1,mu1))) #data if guilty
    xyG0 = sapply(1:k, function(k) sum(rbinom(10,1,mu0))) #data if not guilty
    lik.ratioG1 = sapply(xyG1, function(j) dbinom(j,10,mu1)/dbinom(j,10,mu0))
    lik.ratioG0 = sapply(xyG0, function(j) dbinom(j,10,mu1)/dbinom(j,10,mu0))
    
    lik1[sim.round,]<-lik.ratioG1
    lik0[sim.round,]<-lik.ratioG0
    
    # missing evidence (decides degree of imputation)
    share.miss = rbeta(k,1,3)
    
    mhatG1 <- mhatG0 <- rep(NA,k)
    
    # matches if guilty
    mG1 <- sapply(1:k, function(x) lik.ratioG1[x] * (Pi_E/Pi_EC) * 1/N)
    m.cascadeG1 <- sapply(1:k, function(x) delta.func(i,mG1[x],share.miss[x])*mG1[x])
    mhatG1[1] <- m.cascadeG1[1]
    for(j in 2:k){mhatG1[j] <- total.bias(i,mG1[c(1:(j-1))],mhatG1[c(1:(j-1))], share.miss[j])*mG1[j]}
    
    # matches if not guilty
    mG0 <- sapply(1:k, function(x) lik.ratioG0[x] * (Pi_E/Pi_EC) * 1/N)
    m.cascadeG0 <- sapply(1:k, function(x) delta.func(i,mG0[x],share.miss[x])*mG0[x])
    mhatG0[1] <- m.cascadeG0[1]
    for(j in 2:k){mhatG0[j] <- total.bias(i,mG0[c(1:(j-1))],mhatG0[c(1:(j-1))], share.miss[j])*mG0[j]}
    
    m1[sim.round,]<-mG1
    m0[sim.round,]<-mG0
    mc1[sim.round,]<-m.cascadeG1
    mc0[sim.round,]<-m.cascadeG0
    mhat1[sim.round,]<-mhatG1
    mhat0[sim.round,]<-mhatG0
  }
  return(list(m1,m0,mc1,mc0,mhat1,mhat0))
}


# say M = (1,0,0,0,1)
# what is prob(M|G)/prob(M|G^C)? ie, if matches were unbiased 
# prob(M|G) = P(m1|G)P(m2|G)...P(m5|G)
# = P(m1=1|G)P(m2=0|G)...P(m4=0|G)P(m5=1|G)
# P(mk=1|G) = P [ likelihoodRatio > N |G ]
# = P [ dbinom( rbinom(10,1,mu1), 10, mu1 )/dbinom( rbinom(10,1,mu1), 10, mu0 ) > N ]
# P(mk=0|G) = P [ likelihoodRatio <= N |G ]
# = P [ dbinom( rbinom(10,1,mu1), 10, mu1 )/dbinom( rbinom(10,1,mu1), 10, mu0 ) <= N ]

# P(mhatk=1|G) = P [ likelihoodRatio > N*bias |G ]
# = P [ dbinom( rbinom(10,1,mu1), 10, mu1 )/dbinom( rbinom(10,1,mu1), 10, mu0 ) > N*bias ]

PM <- PMhat <- num_matches <- c()
nsims = 100
for(sim.round in 1:nsims){
  #generate observed matches (biased)
  xy = sapply(1:k, function(j) ifelse(g==1, sum(rbinom(10,1,mu1)), sum(rbinom(10,1,mu0))))
  lik.ratio = sapply(xy, function(j) dbinom(j,10,mu1)/dbinom(j,10,mu0))
  
  # missing evidence (decides degree of imputation)
  share.miss = rbeta(k,1,3)
  mhat <- m <- m.cascade <- rep(NA,k)
  m <- sapply(1:k, function(x) lik.ratio[x] * (Pi_E/Pi_EC) * 1/N)
  m.cascade <- sapply(1:k, function(x) delta.func(i,m[x],share.miss[x])*m[x])
  mhat[1] <- m.cascade[1]
  for(j in 2:k){mhat[j] <- total.bias(i,m[c(1:(j-1))],mhat[c(1:(j-1))], share.miss[j])*m[j]}
  
  decl.matches <- sapply(mhat, function(k) (k>1)*1)
  num_matches[sim.round] = sum(decl.matches)
  
  #probs of seeing decl matches under guilt/not guilt
  variants <- check_probs()
  matching <- lapply(variants, function(j) mean(apply(j,1,function(x) all((1*(x>=1))==decl.matches))))
  PM[sim.round] = matching[[1]]/matching[[2]]
  PMhat[sim.round] = matching[[5]]/matching[[6]]
}x

#PM is higher than PMhat because PM is the chances of guilt if you don't adjust for bias
PM[is.infinite(PM)] <- 50
PMhat[is.infinite(PMhat)] <- 50
max.val = max(c(PM,PMhat),na.rm = T)
plot(PM,PMhat,xlab='Unadjusted',ylab='Adjusted',xlim=c(0,max.val),ylim=c(0,max.val))
abline(0,1)

mean(PM>1, na.rm = T)
mean(PMhat>1, na.rm = T)

df <- data.frame(posterior = c(PM,PMhat), num_matches = rep(num_matches,2), type = rep(c("Unadjusted","Adjusted"),each = nsims))
summary <- df %>% group_by(type, num_matches) %>%
  summarise(num_convicted = sum(posterior>1, na.rm = T),
            rate= 100*sum(posterior>1, na.rm = T)/length(posterior))

p1 <- ggplot(summary, aes(x = num_matches, y = num_convicted, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_text(summary, mapping = aes(x = num_matches, y = num_convicted, label = paste(round(rate,2),"%",sep="")))+
  xlab("Number of Pieces of Evidence Declared a Match") +
  ylab("Number of Individuals Falsely Found Guilty") +
  theme_bw()
p1

ggsave(p1, filename = '~/repos/Forensic_bias/outputs/FinalGuilt.png')
