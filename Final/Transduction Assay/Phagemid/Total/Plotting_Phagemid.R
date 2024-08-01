library("readxl")
library("tidyverse")
library("ggbreak")

theme_set(theme_classic())

my_data <- read_excel("Phagemid_Total_06.20.24.xlsx")

my_data$oriR <- factor(my_data$oriR, c("pBBR1", "P1 oriR", "Control"))

my_sum <- my_data %>% 
  group_by(oriR) %>% 
  summarise(my_mean = mean(Tot_C), my_sd = sd(Tot_C) )

plot_total <- ggplot() +
  geom_bar(data = my_sum, position = position_dodge(), stat = "identity", aes(x = oriR, y=my_mean), colour = "black", linewidth = 1, width = 0.75) +
  geom_errorbar(data = my_sum, position = position_dodge(0.75), aes(x = oriR, y = my_mean, ymin = my_mean-my_sd, ymax = my_mean+my_sd, group = oriR), width = 0.5, linewidth = 1) +
  geom_jitter(data = my_data, position = position_dodge(0.75), aes(x = oriR, y = Tot_C), shape = 21, colour = "black", fill = 'white', size = 2.5) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(1, 1000000, 10000000, 100000000, 1000000000, 10000000000)) +
  scale_y_break(c(2,10000000)) +
  ylab("Colonies/mL Lysate") +
  xlab("Viral Particle") +
  ggtitle('Total Phagemid Particles')

print(plot_total) + theme(axis.line = element_line(color = "white"))

