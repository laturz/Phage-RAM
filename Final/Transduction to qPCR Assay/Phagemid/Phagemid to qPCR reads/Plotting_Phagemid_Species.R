library("readxl")
library("tidyverse")
library("ggbreak")

theme_set(theme_classic())

my_data <- read_excel("Concentration_data_v2.xlsx")

my_data$Concentration <- as.numeric(my_data$Concentration)

my_data$Backbone[my_data$Backbone == 70] <- "P1 oriR"
my_data$Backbone[my_data$Backbone == 64] <- "pBBR1"
my_data$Backbone <- factor(my_data$Backbone, c("P1 oriR", "pBBR1", "Control"))
my_data$Species <- factor(my_data$Species, c("C. freundii", "E. coli", "P. aeruginosa", "S. flexneri", "E. coli - Negative", "E. coli - Positive", "NTC"))

my_sum <- my_data %>% 
  group_by(Backbone, Species) %>% 
  summarise(my_mean = mean(Concentration), my_sd = sd(Concentration) )

plot_t <- ggplot() +
  # geom_bar(data = my_sum, position = position_dodge(), stat = "identity", aes(x = Species, y=my_mean, fill = Condition), colour = "black", linewidth = 1, width = 0.75) +
  geom_errorbar(data = my_sum, position = position_dodge(0.75), aes(x = Species, y = my_mean, ymin = my_mean, ymax = my_mean, group = Backbone), width = 0.5, linewidth = 1) +
  geom_jitter(data = my_data, position = position_dodge(0.75), aes(x = Species, y = Concentration, group = Backbone, shape = Backbone, color = Species), fill = 'white', size = 2) +
  scale_y_continuous(trans="log10", sec.axis = sec_axis( trans=~.*1)) +
  # scale_y_break(c(100000,100001)) +
  ggtitle("Phagemid RAM Reads") +
  ylab("Copies/uL Extract") +
  xlab("Species") +
  theme(axis.title.x = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(shape = "Backbone")

print(plot_t)

