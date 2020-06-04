# cascade bias: bias in one analyst's brain
rm(list = ls())

library(ggplot2)
N = 20

# true probabilities
Pi = .15 #share in pop w I=1
Pi_E = Pi #prob I=1 if E0
Pi_EC = Pi #prob I=1 if E0^C

# analyst's beliefs
PAi_E = 2*Pi
PAi_EC = Pi_EC

# true posterior
Pyx_IE = .5 #prob evidence match given from same source and I
Pyx_IEC = .05 #prob evidence match given not from same source and I
Prior = 1/N
Posterior = (Pyx_IE/Pyx_IEC)*(Pi_E/Pi_EC)*Prior

# biased posterior
Biased_Posterior = (Pyx_IE/Pyx_IEC)*(PAi_E/PAi_EC)*Prior