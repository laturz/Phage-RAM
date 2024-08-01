library("readxl")
library("tidyverse")
library("ggbreak")

theme_set(theme_classic())

my_data <- read_excel("Phagemid_Ratio_06.20.24.xlsx")

my_data$oriR <- factor(my_data$oriR, c("pBBR1", "P1 oriR", "Control"))
my_data$Particle <- factor(my_data$Particle, c('Phage', 'Phagemid'))

my_sum <- my_data %>% 
  group_by(oriR, Particle) %>% 
  summarise(my_mean = mean(Tot_C), my_sd = sd(Tot_C))

plot_t <- ggplot() +
  geom_bar(data = my_sum, position = 'fill', stat = "identity", aes(x = oriR, y=my_mean, fill = Particle), colour = "black", linewidth = 1, width = 0.5) +
  geom_errorbar(data = filter(my_sum, Particle == 'Phagemid'), aes(x = oriR, y = my_mean, ymin = my_mean-my_sd, ymax = my_mean+my_sd), width = 0.25, linewidth = 1) +
  geom_jitter(data = filter(my_data, Particle == 'Phagemid'), aes(x = oriR, y = Tot_C), shape = 21, colour = "black", fill = 'white', size = 2.5, width = 0.1) +
  ylab("Fraction Phagemid Particles") +
  xlab("oriR")

print(plot_t) + theme(axis.line = element_line(color = "white"))

