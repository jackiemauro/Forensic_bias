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

ggplot(summary, aes(x = evidence, y = share_declared_match, fill = type))+
  geom_bar(stat = 'identity', position = 'dodge')+
  xlab("Piece of Evidence") + ylab("Share declared a match")+
  theme_bw()



#ggsave(filename = '~/repos/Forensic_bias/outputs/SnowballFigure.png')

## AL's version -- use different # suspects and % missing evidence instead of simulating

N = 10 #number of suspects
Pi = .15 #share in pop w I=1
Pi_E = Pi #prob I=1 if E0 (set to constant over pieces of evidence for simplicity)
Pi_EC = Pi #prob I=1 if E0^C (set to constant over pieces of evidence for simplicity)
Prior = 1/N
g = 0 # 0 means not guilty
i = 1 # 1 means has characteristic
k = 5 # pieces of evidence

mu1 = .5; mu0=.25

nsims = 1000

cascade_snowball_sim = function(nsims, share_missing, n_suspects){
output = list()
for(sim.round in 1:nsims){
  xy = sapply(1:k, function(j) ifelse(g==1, sum(rbinom(k,1,mu1)), sum(rbinom(k,1,mu0))))
  lik.ratio = sapply(xy, function(j) dbinom(j,k,mu1)/dbinom(j,k,mu0))
  
  # missing evidence (decides degree of imputation)
  share.miss = rep(share_missing, 10)
  
  mhat <- m <- m.cascade <- rep(NA,k)
  m <- sapply(1:k, function(x) lik.ratio[x] * (Pi_E/Pi_EC) * (1/n_suspects))
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
return(summary)
}

set.seed(081720)
nsims = 1000
share_missing = c(0, .01, .1, .25,.5)
n_suspects = c(2, 5, 10)

sim_params = expand.grid(share_missing = share_missing, n_suspects = n_suspects)

results = c()
for(ii in 1:nrow(sim_params)){
  sim_sum = cascade_snowball_sim(1000, sim_params$share_missing[ii], sim_params$n_suspects[ii]) %>%
    mutate(share_missing = sim_params$share_missing[ii], n_suspects = sim_params$n_suspects[ii])
  results = rbind(results, sim_sum)
}

results %>%
  ggplot(., aes(x = evidence, y = share_declared_match, col = type)) +
  facet_grid(rows = vars(share_missing), cols = vars(n_suspects), labeller = label_both) +
  geom_line() +
  xlab("Piece of Evidence") + ylab("Share declared a match")+
  theme_bw() +
  theme(legend.position = "bottom") + 
  scale_color_viridis_d(option = "magma", end = .75) 

ggsave(filename = 'outputs/SnowballFigure2.png')

## V3 -- simplified 

cascade_snowball_sim = function(nsims, share_missing, n_suspects, n_evidence){
  output = list()
  for(sim.round in 1:nsims){
    xy = sapply(1:n_evidence, function(j) ifelse(g==1, sum(rbinom(n_evidence,1,mu1)), sum(rbinom(n_evidence,1,mu0))))
    lik.ratio = sapply(xy, function(j) dbinom(j,n_evidence,mu1)/dbinom(j,n_evidence,mu0))
    
    # missing evidence (decides degree of imputation)
    share.miss = rep(share_missing, n_evidence)
    
    mhat <- m <- m.cascade <- rep(NA,n_evidence)
    m <- sapply(1:n_evidence, function(x) lik.ratio[x] * (Pi_E/Pi_EC) * (1/n_suspects))
    m.cascade <- sapply(1:n_evidence, function(x) delta.func(i,m[x],share.miss[x])*m[x])
    mhat[1] <- m.cascade[1]
    
    for(j in 2:n_evidence){mhat[j] <- total.bias(i,m[c(1:(j-1))],mhat[c(1:(j-1))], share.miss[j])*m[j]}
    
    output[[sim.round]] <- data.frame(posterior = c(m,m.cascade,mhat),
                                      type = rep(c("Unbiased","Cascade","Snowball"), each = n_evidence),
                                      evidence = rep(1:n_evidence,3),
                                      round = rep(i,n_evidence*3))
  }
  sim.out = do.call(rbind, output)
  summary = sim.out %>%
    group_by(type,evidence) %>%
    summarise(share_declared_match = mean(posterior>=1))
  return(summary)
}



set.seed(081720)
nsims = 1000
share_missing = .01
n_suspects = 2
n_evidence = 5

sim_params = expand.grid(share_missing = share_missing, n_suspects = n_suspects, n_evidence = n_evidence)

results = c()
for(ii in 1:nrow(sim_params)){
  sim_sum = cascade_snowball_sim(1000, 
                                 sim_params$share_missing[ii], 
                                 sim_params$n_suspects[ii],
                                 sim_params$n_evidence[ii]) %>%
    mutate(share_missing = sim_params$share_missing[ii], 
           n_suspects = sim_params$n_suspects[ii],
           n_evidence = sim_params$n_evidence[ii])
  results = rbind(results, sim_sum)
}

bias_gg_df = results %>%
  pivot_wider(id_cols = c("evidence", "share_missing", "n_suspects"), names_from = type, values_from = share_declared_match) %>%
  mutate(
    `Bias Cascade` = Cascade - Unbiased,
    `Bias Snowball` = Snowball - Unbiased,
    `No Bias` = Unbiased - Unbiased,
    analyst = LETTERS[evidence]
  ) %>%
  pivot_longer(c(`Bias Cascade`, `Bias Snowball`, `No Bias`), names_to = "type", values_to = "bias")

snowball_df = bias_gg_df %>% 
  filter(type == "Bias Snowball") %>%
  mutate(xend = c(2:5, NA), 
         yend = c(bias[2:5], NA))

colors = c("#999999", "#41424C", "#ADADC9")

bias_gg_df %>%
  ggplot(., aes(x = analyst, y = bias, col = type)) +
  geom_point(size = 5) +
  geom_segment(data = snowball_df, 
               aes(xend = xend, yend = yend), 
               arrow = arrow(length = unit(0.5, "cm"),
                             type = "open")) +
  labs(x = "Analyst (sequential order)",
       y = "Bias in Posterior",
       col = "") +
  annotate("errorbar", x = 0.8, ymin = 0, ymax = .25, col = "darkorange", width = .1) +
  annotate("text", x = 0.8, y = .125, hjust = 1.5, label = expression(I[t[i]]), col = "darkorange", size = 6) +
  annotate("errorbar", x = 2.1, ymin = .3, ymax = .42, col = "darkorange", width = .1) +
  annotate("text", x = "B", y = .35, hjust = -.5, label = expression(LR[A]), col = "darkorange", size = 5) +
  annotate("errorbar", x = 3.1, ymin = .26, ymax = .5, col = "darkorange", width = .1) +
  annotate("text", x = "C", y = .38, hjust = -.5, label = expression(LR[B]), col = "darkorange", size = 5) +
  annotate("errorbar", x = 4.1, ymin = .25, ymax = .54, col = "darkorange", width = .1) +
  annotate("text", x = "D", y = .38, hjust = -.5, label = expression(LR[C]), col = "darkorange", size = 5) +
  annotate("errorbar", x = 5.1, ymin = .25, ymax = .57, col = "darkorange", width = .1) +
  annotate("text", x = "E", y = .38, hjust = -.5, label = expression(LR[D]), col = "darkorange", size = 5) +
  annotate("text", x = "E", y = .577, hjust = -.15, label = "Bias Snowball", col = colors[2]) +
  annotate("text", x = "E", y = .244, hjust = -.15, label = "Bias Cascade", col = colors[1]) +
  annotate("text", x = "E", y = 0, hjust = -.15, label = "No Bias", col = colors[3]) +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_x_discrete(expand = c(.01,.9)) +
  scale_color_manual(values = colors)

ggsave(filename = '~/Forensic_bias/outputs/snowball_vs_cascade.png',
       width = 8,
       height = 4)

