###################################################################
### Analysis of the simulation results - BIDDER STRENGTH
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

#### BIDDER STRENGTH - DATA SETS

# s_DT & s_VOD moving together, Betas fixed
sim.smov <- read.csv("data/sim_results_s_moving_together_betas_fixed.csv", sep="\t")
sim.smov$auction_format <- as.factor(sim.smov$auction_format)
str(sim.smov)

# s_DT fixed & s_VOD moving, Betas fixed
sim.svodmov <- read.csv("data/sim_results_s_vod_mov_s_dt_fixed_betas_fixed.csv", sep="\t")
sim.svodmov$auction_format <- as.factor(sim.svodmov$auction_format)
str(sim.svodmov)

# s_DT, s_VOD in cross product (CR), betas fixed
sim.cp <- read.csv("data/sim_results_s_moving_crproduct_betas_fixed.csv", sep="\t")
sim.cp$auction_format <- as.factor(sim.cp$auction_format)



#### PREPARE GRAPHICS PALETTE
formatColors <- brewer.pal(3, "Dark2")
names(formatColors) <- levels(sim.smov$auction_format)
colScale <- scale_color_manual(name="auction format", values=formatColors)


#### EFFICIENCY

## both moving together
y_low = 0.9
eff.smov.plot <- sim.smov %>% filter(auction_format != 'sealed-MBP16') %>% 
  filter(sDT >= 1) %>% 
  ggplot(aes(sDT, efficiency, group=auction_format)) +
  geom_point(aes(shape=auction_format, color=auction_format)) +
  geom_line(aes(color=auction_format)) +
  expand_limits(y=y_low) +
  xlab(expression(paste(s[DT], " & ", s[VOD])))
  # ggtitle("Efficiency in relation to bidder strength s_DT & s_VOD")
eff.smov.plot
pdf("img/eff/eff_s_both_mov.pdf", width=11, height=4)
eff.smov.plot
dev.off()

## one fixed, the other moving
eff.svodmov.plot <- sim.svodmov %>% filter(auction_format != 'sealed-MBP16') %>% 
  ggplot(aes(sVOD, efficiency, group=auction_format)) +
  geom_point(aes(shape=auction_format, color=auction_format)) +
  geom_line(aes(color=auction_format)) +
  expand_limits(y=y_low) +
  xlab(expression(s[VOD]))# +
  # ggtitle(expression(paste("Efficiency in relation to bidder strength [", s[DT]==1, "]" )))
eff.svodmov.plot
pdf("img/eff/eff_s_singlemov.pdf", width=11, height=4)
eff.svodmov.plot
dev.off()

## SAVING PLOTS
pdf("img/eff_s_ana.pdf")
multiplot(eff.smov.plot, eff.svodmov.plot, cols=2)
dev.off()

## cross product anaylsis - EFFICIENCY
eff.cp.hpb.plot <- sim.cp %>% filter(auction_format == 'hpb') %>% 
  ggplot() +
  geom_tile(aes(x=sDT, y=sVOD, fill=efficiency)) +
  scale_fill_gradientn(colours=rev(brewer.pal(10, "Spectral"))) +
  # ggtitle(expression(paste("Competition environment for HPB [rev ~ ", s[DT], ", ", s[VOD], "]"))) +
  xlab(expression(s[DT])) +
  ylab(expression(s[VOD])) +
  theme(text = element_text(size=18)) +
  coord_fixed()
eff.cp.hpb.plot
pdf("img/eff_hpb_comp_environment.pdf", height=5)
eff.cp.hpb.plot
dev.off()

eff.cp.smra.plot <- sim.cp %>% filter(auction_format == 'smra') %>% 
  # filter(sDT > 0.5, sVOD > 0.5) %>% 
  ggplot() +
  geom_tile(aes(x=sDT, y=sVOD, fill=efficiency)) +
  scale_fill_gradientn(colours=rev(brewer.pal(10, "Spectral"))) +
  # ggtitle(expression(paste("Competition environment for SMRA [rev ~ ", s[DT], ", ", s[VOD], "]"))) +
  xlab(expression(s[DT])) +
  ylab(expression(s[VOD])) +
  coord_fixed()
eff.cp.smra.plot
pdf("img/rev_smra_comp_environment.pdf")
rev.cp.smra.plot
dev.off()


### Difference in Efficiency
sim.cp.hpb <- sim.cp %>% 
  filter(auction_format == 'hpb')
sim.cp.smra <- sim.cp %>% 
  filter(auction_format == 'smra')
sim.cp.diff <- inner_join(sim.cp.hpb, sim.cp.smra, by=c("sDT", "sVOD"))

eff.diff <- sim.cp.diff %>% 
  mutate(diff = efficiency.x - efficiency.y) %>% 
  select(sDT, sVOD, diff)

eff.diff.plot <- eff.diff %>% 
  ggplot() +
  geom_tile(aes(x=sDT, y=sVOD, fill=diff)) +
  scale_fill_gradientn(colours=rev(brewer.pal(10, "Spectral")), guide = guide_legend(title=expression(paste(eff[HPB] - eff[SMRA])))) +
  ggtitle(expression(paste("Competition environment for HPB vs. SMRA [", eff[HPB] - eff[SMRA]," ~ " , s[DT], ", ", s[VOD], "]"))) +
  xlab(expression(s[DT])) +
  ylab(expression(s[VOD])) +
  coord_fixed()
eff.diff.plot



#### REVENUE
y_low = 0.5
rev.smov.plot <- sim.smov %>% filter(sDT >= 1) %>% 
  ggplot(aes(sDT, revenue, group=auction_format)) +
  geom_point(aes(shape=auction_format, color=auction_format)) +
  geom_line(aes(color=auction_format)) +
  # expand_limits(y=y_low) +
  xlab(expression(paste(s[DT], " & ", s[VOD]))) +
  theme(text = element_text(size=18))
  # ggtitle(expression(paste("Revenue in relation to bidder strength ",  s[DT], " & ", s[VOD])))
rev.smov.plot
pdf("img/rev_s_bothmov.pdf", width=11, height=4)
rev.smov.plot
dev.off()

## one fixed, the other moving
rev.svodmov.plot <-  sim.svodmov %>% 
  ggplot(aes(sVOD, revenue, group=auction_format)) +
  geom_point(aes(shape=auction_format, color=auction_format)) +
  geom_line(aes(color=auction_format)) +
  # expand_limits(y=y_low) +
  xlab(expression(s[VOD])) +
  theme(text = element_text(size=18))
  # ggtitle(expression(paste("Revenue in relation to bidder strength [", s[DT]==1, "]" )))
rev.svodmov.plot
pdf("img/rev_s_singlemov.pdf",  width=11, height=4)
rev.svodmov.plot
dev.off()

## SAVING PLOTS
# pdf("img/rev_s_ana.pdf")
# multiplot(rev.smov.plot, rev.svodmov.plot, cols=2)
# dev.off()


## cross product analysis
rev.cp.hpb.plot <- sim.cp %>% filter(auction_format == 'hpb') %>% 
  ggplot() +
  geom_tile(aes(x=sDT, y=sVOD, fill=revenue)) +
  scale_fill_gradientn(colours=rev(brewer.pal(10, "Spectral"))) +
  ggtitle(expression(paste("Competition environment for HPB [rev ~ ", s[DT], ", ", s[VOD], "]"))) +
  xlab(expression(s[DT])) +
  ylab(expression(s[VOD])) +
  theme(text = element_text(size=14)) +
  coord_fixed()
rev.cp.hpb.plot
# pdf("img/rev_hpb_comp_environment.pdf", height=5)
png("img/rev_hpb_comp_environment.png")
rev.cp.hpb.plot
dev.off()

rev.cp.smra.plot <- sim.cp %>% filter(auction_format == 'smra') %>% 
  # filter(sDT > 0.5, sVOD > 0.5) %>% 
  ggplot() +
  geom_tile(aes(x=sDT, y=sVOD, fill=revenue)) +
  scale_fill_gradientn(colours=rev(brewer.pal(10, "Spectral"))) +
  # ggtitle(expression(paste("Competition environment for SMRA [rev ~ ", s[DT], ", ", s[VOD], "]"))) +
  xlab(expression(s[DT])) +
  ylab(expression(s[VOD])) +
  coord_fixed()
rev.cp.smra.plot
pdf("img/rev_smra_comp_environment.pdf")
rev.cp.smra.plot
dev.off()

# mutate(diff = ☻)

sim.cp.hpb <- sim.cp %>% 
  filter(auction_format == 'hpb')

sim.cp.smra <- sim.cp %>% 
  filter(auction_format == 'smra')

sim.cp.diff <- inner_join(sim.cp.hpb, sim.cp.smra, by=c("sDT", "sVOD"))
rev.diff <- sim.cp.diff %>% 
  mutate(diff = revenue.x - revenue.y) %>% 
  select(sDT, sVOD, diff)

rev.diff.plot <- rev.diff %>% 
  ggplot() +
  geom_tile(aes(x=sDT, y=sVOD, fill=diff)) +
  scale_fill_gradientn(colours=rev(brewer.pal(10, "Spectral")), guide = guide_legend(title=expression(paste(rev[HPB] - rev[SMRA])), reverse=TRUE)) +
  # ggtitle(expression(paste("Competition environment for HPB vs. SMRA [", rev[HPB] - rev[SMRA]," ~ " , s[DT], ", ", s[VOD], "]"))) +
  ggtitle(expression(paste("Competition environment for HPB vs. SMRA [", rev[HPB] - rev[SMRA],"]"))) +
    xlab(expression(s[DT])) +
  ylab(expression(s[VOD])) +
  theme(text = element_text(size=12)) +
  coord_fixed()
rev.diff.plot
png("img/rev_diff.png")
# pdf("img/rev_diff.pdf", width = 7, height=5)
rev.diff.plot
dev.off()


###### BACKUP
p <- sim.cp %>% filter(auction_format == 'hpb') %>% 
  ggplot(aes(x=sDT, y=sVOD, z=revenue)) +
  geom_contour(aes(weight=3))
  # geom_density_2d()
p
