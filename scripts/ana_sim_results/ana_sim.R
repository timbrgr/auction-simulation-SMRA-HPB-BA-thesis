###################################################################
### Analysis of the simulation results - BETA & BIDDER STRENGHT
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

#### BETA - DATA SETS

# beta900 & beta1800 moving together
sim <- read.csv("data/sim_results_both_moving.csv", sep="\t")
sim$auction_format <- as.factor(sim$auction_format)
str(sim)

# beta900 moving, beta1800 fixed at 1
sim.b900mov <- read.csv("data/sim_results_b900_moving.csv", sep="\t")
sim.b900mov$auction_format <- as.factor(sim.b900mov$auction_format)
str(sim.b900mov)

# beta900 fixed at 1, beta1800 moving
sim.b1800mov <- read.csv("data/sim_results_b1800_moving.csv", sep="\t")
sim.b1800mov$auction_format <- as.factor(sim.b1800mov$auction_format)
str(sim.b1800mov)


#### PREPARE GRAPHICS
spectrumColors <- brewer.pal(4, "Dark2")
names(spectrumColors) <- levels(sim$auction_format)
colScale <- scale_color_manual(name="auction_format", values=spectrumColors)

theme_set(theme_gray(base_size = 18))

#### EFFICIENCY
y_low = 0.9
eff.b900mov.plot <- sim.b900mov %>% filter(auction_format != 'sealed-MBP16') %>% 
  ggplot(aes(beta900, efficiency, group=auction_format)) +
  geom_point(aes(shape = auction_format, color=auction_format)) +
  geom_line(aes(color=auction_format)) +
  theme(text = element_text(size=18)) +
  xlab(expression(beta[900])) +
  expand_limits(x=0.5, y=y_low)
  # ggtitle("b900 moving, b1800 fixed at 1")
eff.b900mov.plot  

eff.b1800mov.plot <- sim.b1800mov %>% filter(auction_format != 'sealed-MBP16') %>% 
  ggplot(aes(beta1800, efficiency, group=auction_format)) +
  geom_point(aes(shape = auction_format, color=auction_format)) + 
  geom_line(aes(color=auction_format)) +
  theme(text = element_text(size=18)) +
  xlab(expression(beta[1800])) +
  expand_limits(x=0.5, y=y_low)
  # ggtitle("b900 fixed at 1, b1800 moving")
eff.b1800mov.plot  

eff.bothmov.plot <- sim %>% filter(auction_format != 'sealed-MBP16') %>% 
  ggplot(aes(beta900, efficiency, group=auction_format)) +
  geom_point(aes(shape = auction_format, color=auction_format)) + 
  geom_line(aes(color=auction_format)) +
  theme(text = element_text(size=18)) +
  expand_limits(x=0.5, y=y_low) +
  xlab(expression(paste(beta[900], " & ", beta[1800]))) +
  ggtitle("Both betas moving together")
eff.bothmov.plot
pdf("img/eff/eff_both_betas_moving.pdf", width=11, height=4)
eff.bothmov.plot
dev.off()

png("img/eff/eff_both_betas_moving.png")
eff.bothmov.plot
dev.off()

#### REVENUE
# y_low = 0
rev.b900mov.plot <- sim.b900mov %>% 
  ggplot(aes(beta900, revenue, group=auction_format)) +
  geom_point(aes(shape = auction_format, color=auction_format)) + 
  geom_line() +
  geom_line(aes(color=auction_format)) +
  xlab(expression(beta[900])) +
  theme(text = element_text(size=18))
  # expand_limits(x=0, y=y_low) +
  # ggtitle("b900 moving, b1800 fixed at 1")
rev.b900mov.plot
pdf("img/rev_b900mov.pdf", width=11, height=4)
rev.b900mov.plot
dev.off()

rev.b1800mov.plot <- sim.b1800mov %>%
  ggplot(aes(beta1800, revenue, group=auction_format)) +
  geom_point(aes(shape = auction_format, color=auction_format)) + 
  geom_line() +
  geom_line(aes(color=auction_format)) +
  expand_limits(x=0, y=y_low) +
  xlab(expression(beta[1800])) +
  theme(text = element_text(size=18))
  # ggtitle("b900 fixed at 1, b1800 moving")
rev.b1800mov.plot
pdf("img/rev_b1800mov.pdf", width=11, height=4)
rev.b1800mov.plot
dev.off()

rev.bothmov.plot <- sim %>% 
  ggplot(aes(beta900, revenue, group=auction_format)) +
  geom_point(aes(shape = auction_format, color=auction_format)) + 
  geom_line() +
  geom_line(aes(color=auction_format)) +
  # expand_limitys(x=0, y=y_low) +
  xlab(expression(paste(beta[900], " & ", beta[1800]))) +
  theme(text = element_text(size=18))
  # ggtitle("Both betas moving together")
rev.bothmov.plot
pdf("img/rev_both_beta_mov.pdf", width=11, height=4)
rev.bothmov.plot
dev.off()

#### FINAL PLOTS (for saving)
pdf("img/eff_betas_moving_each.pdf")
multiplot(eff.b900mov.plot, eff.b1800mov.plot, eff.bothmov.plot)
dev.off()

pdf("img/rev_betas_moving_each.pdf")
multiplot(rev.b900mov.plot, rev.b1800mov.plot, rev.bothmov.plot)
dev.off()


#### BACKUP
# TODO: show spread of revenue
y_low = 0.85
rev.bothmov.spread.plot <-  sim %>%  filter(auction_format != 'sealed-MBP16') %>% 
  ggplot(aes(auction_format, revenue, group=auction_format)) +
  geom_point(aes(shape = auction_format, color=auction_format)) + geom_line() +
  expand_limits(x=0, y=y_low) +
  ggtitle("Both betas moving together")
rev.bothmov.spread.plot