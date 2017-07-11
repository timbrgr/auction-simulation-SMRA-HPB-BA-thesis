###################################################################
### Analysis of the simulation results - VM PREDICT
### Bachelor's Thesis by Tim Berger
### A comparison of the impact of auction formats used in the 
### 2015 German spectrum auction "Mobiles Breitband - Projekt 2016" 
### Chair Decision Sciences & Systems (DSS)
### Technical University Munich (TUM)
###################################################################

#### PACKAGE MANAGEMENT
packages <- c('dplyr', 'ggplot2', 'grid', 'svglite', 
              'RColorBrewer', 'tidyr', 'stringr')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
source("multiplot.R")


#### VM PREDICT - DATA SETS

# beta900 & beta1800 moving together
vmp.bothmov <- read.csv("data/sim_results_both_moving_VM_PREDICT.csv", sep="\t")
vmp.bothmov$auction_format <- as.factor(vmp.bothmov$auction_format)

# beta900 moving, beta1800 fixed at 1
vmp.b900mov <- read.csv("data/sim_results_b900_moving_VM_PREDICT.csv", sep="\t")
vmp.b900mov$auction_format <- as.factor(vmp.b900mov$auction_format)

# beta900 fixed at 1, beta1800 moving
vmp.b1800mov <- read.csv("data/sim_results_b1800_moving_VM_PREDICT.csv", sep="\t")
vmp.b1800mov$auction_format <- as.factor(vmp.b1800mov$auction_format)



#### EFFICIENCY
y_low = 0.9
eff.vmp.b900mov.plot <- vmp.b900mov %>% filter(auction_format != 'sealed-MBP16') %>% 
  ggplot(aes(beta900, efficiency, group=auction_format)) +
  geom_point(aes(shape = auction_format, color=auction_format)) +
  geom_line(aes(color=auction_format)) +
  theme(text = element_text(size=18)) +
  xlab(expression(beta[900])) +
  expand_limits(x=0.5, y=y_low)
  # ggtitle("b900 moving, b1800 fixed at 1")
eff.b900mov.plot 

eff.vmp.b1800mov.plot <- vmp.b1800mov %>% filter(auction_format != 'sealed-MBP16') %>% 
  ggplot(aes(beta1800, efficiency, group=auction_format)) +
  geom_point(aes(shape = auction_format, color=auction_format)) + 
  geom_line(aes(color=auction_format)) +
  theme(text = element_text(size=18)) +
  xlab(expression(beta[1800])) +
  expand_limits(x=0.5, y=y_low)
  # ggtitle("b900 fixed at 1, b1800 moving")
eff.vmp.b1800mov.plot

eff.vmp.bothmov.plot <- vmp.bothmov %>% filter(auction_format != 'sealed-MBP16') %>% 
  ggplot(aes(beta900, efficiency, group=auction_format)) +
  geom_point(aes(shape = auction_format, color=auction_format)) + 
  geom_line(aes(color=auction_format)) +
  theme(text = element_text(size=18)) +
  expand_limits(x=0.5, y=y_low) +
  xlab(expression(paste(beta[900], " & ", beta[1800])))
# ggtitle("Both betas moving together")
eff.vmp.bothmov.plot
pdf("img/eff/eff_both_betas_moving_VMP.pdf", width=11, height=4)
eff.vmp.bothmov.plot
dev.off()


#### REVENUE
rev.vmp.b900mov.plot <- vmp.b900mov %>% 
  ggplot(aes(beta900, revenue, group=auction_format)) +
  geom_point(aes(shape = auction_format, color=auction_format)) + 
  geom_line() +
  geom_line(aes(color=auction_format)) +
  xlab(expression(beta[900])) +
  theme(text = element_text(size=18))
# expand_limits(x=0, y=y_low) +
# ggtitle("b900 moving, b1800 fixed at 1")
rev.vmp.b900mov.plot
pdf("img/rev_b900mov_VMP.pdf", width=11, height=4)
rev.vmp.b900mov.plot
dev.off()

rev.vmp.b1800mov.plot <- vmp.b1800mov %>% 
  ggplot(aes(beta1800, revenue, group=auction_format)) +
  geom_point(aes(shape = auction_format, color=auction_format)) + 
  geom_line() +
  geom_line(aes(color=auction_format)) +
  xlab(expression(beta[1800])) +
  theme(text = element_text(size=18))
rev.vmp.b1800mov.plot
pdf("img/rev_b1800mov_VMP.pdf", width=11, height=4)
rev.vmp.b1800mov.plot
dev.off()

# both betas moving together
rev.vmp.bothmov.plot <- vmp.bothmov %>% 
  ggplot(aes(beta900, revenue, group=auction_format)) +
  geom_point(aes(shape = auction_format, color=auction_format)) + 
  geom_line() +
  geom_line(aes(color=auction_format)) +
  xlab(expression(paste(beta[900], " & ", beta[1800]))) +
  theme(text = element_text(size=18))
# ggtitle("Both betas moving together")
rev.vmp.bothmov.plot
pdf("img/rev_both_beta_mov_VMP.pdf", width=11, height=4)
rev.vmp.bothmov.plot
dev.off()

### BIDDER STRENGTH

# s_DT & s_VOD moving together, Betas fixed
vmp.smov <- read.csv("data/sim_results_s_moving_together_betas_fixed_VMP.csv", sep="\t")
vmp.smov$auction_format <- as.factor(vmp.smov$auction_format)

# s_DT fixed & s_VOD moving, Betas fixed
vmp.svodmov <- read.csv("data/sim_results_s_vod_mov_s_dt_fixed_betas_fixed_VMP.csv", sep="\t")
vmp.svodmov$auction_format <- as.factor(vmp.svodmov$auction_format)

# s_DT, s_VOD in cross product (CR), betas fixed
vmp.cp <- read.csv("data/sim_results_s_moving_crproduct_betas_fixed_VMP.csv", sep="\t")
vmp.cp$auction_format <- as.factor(vmp.cp$auction_format)


#### EFFICIENCY
y_low = 0.9
eff.vmp.smov.plot <- vmp.smov %>% filter(auction_format != 'sealed-MBP16') %>% 
  ggplot(aes(sDT, efficiency, group=auction_format)) +
  geom_point(aes(shape=auction_format, color=auction_format)) +
  geom_line(aes(color=auction_format)) +
  expand_limits(y=y_low) +
  xlab(expression(paste(s[DT], " & ", s[VOD])))
# ggtitle("Efficiency in relation to bidder strength s_DT & s_VOD")
eff.vmp.smov.plot

## one fixed, the other moving
eff.vmp.svodmov.plot <- vmp.svodmov %>% filter(auction_format != 'sealed-MBP16') %>% 
  ggplot(aes(sVOD, efficiency, group=auction_format)) +
  geom_point(aes(shape=auction_format, color=auction_format)) +
  geom_line(aes(color=auction_format)) +
  expand_limits(y=y_low) +
  xlab(expression(s[VOD]))
  # ggtitle(expression(paste("Efficiency in relation to bidder strength [", s[DT]==1, "]" )))
eff.vmp.svodmov.plot



### Revenue
rev.vmp.smov.plot <- vmp.smov %>% filter(sDT >= 1) %>% 
  ggplot(aes(sDT, revenue, group=auction_format)) +
  geom_point(aes(shape=auction_format, color=auction_format)) +
  geom_line(aes(color=auction_format)) +
  # expand_limits(y=y_low) +
  xlab(expression(paste(s[DT], " & ", s[VOD]))) +
  theme(text = element_text(size=18))
# ggtitle(expression(paste("Revenue in relation to bidder strength ",  s[DT], " & ", s[VOD])))
rev.vmp.smov.plot
pdf("img/rsdfghjkov.pdf", width=11, height=4)
rev.vmp.smov.plot
dev.off()