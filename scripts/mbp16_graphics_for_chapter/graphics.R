#### PACKAGE MANAGEMENT
packages <- c('dplyr', 'ggplot2', 'grid', 'svglite', 
              'RColorBrewer', 'tidyr', 'stringr')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


freq <- read.csv("data/freq_data.csv", sep="\t")
freq$frequency <- as.factor(freq$frequency)

freq.plot <- freq %>% 
  ggplot(aes(x=frequency, y=amount)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits=c("700 MHz", "900 MHz", "1800 MHz", "1500 MHz")) +
  geom_text(aes(label = amount, y = amount + 5)) +
  theme(text = element_text(size=18)) +
  xlab("frequency band") +
  ylab("amount sold in MHz")
freq.plot
pdf("fig/freq_sold.pdf", width=11, height=4)
freq.plot
dev.off()