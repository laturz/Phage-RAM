library("readxl")
library("tidyverse")
library("ggbreak")

theme_set(theme_classic())

my_data <- read_excel("Concentration_data.xlsx")

my_data$Species <- as.factor(my_data$Species)

my_sum <- summarise(group_by(my_data, Species), my_mean = mean(Concentration), my_sd = sd(Concentration) )

order_cols <- c("C. freundii", "E. coli", "K. pneumoniae", "S. flexneri", "E. coli + WT P1", "E. coli Positive", "NTC")

plot_t <- ggplot() +
  # geom_bar(data = my_sum, stat = "identity", aes(x = factor(Species, order_cols) , y=my_mean), colour = "black", fill = "#5DA899", linewidth = 1, width = 0.75) +
  geom_errorbar(data = my_sum, aes(x = factor(Species, order_cols), y = my_mean, ymin = my_mean, ymax = my_mean), width = 0.5, linewidth = 1) +
  geom_point(data = my_data, aes(x = factor(Species, order_cols), y = Concentration, color = Species, fill = Species), shape = 21, position = position_jitter(0.1), size = 2) +
  scale_y_continuous(trans="log10", sec.axis = sec_axis( trans=~.*1, breaks = c(1, 10, 100, 1000, 10000, 100000)), breaks = c(1, 10, 100, 1000, 10000, 100000)) +
  theme(legend.position = "none") +
  ylab("Copies/uL Extract") +
  xlab("Species") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggtitle("Phage RAM Reads")

print(plot_t)

