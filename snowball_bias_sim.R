# Snowball bias

################ simple simulation ##########
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
param <- seq(0.05,.8,length.out=n)
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
  xlab("Parameter value") + ylab("Posterior")

