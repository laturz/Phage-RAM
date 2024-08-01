library("readxl")
library("tidyverse")
library("ggbreak")

theme_set(theme_classic())

my_data <- read_excel("Species_06.19.24.xlsx")

total <- cbind(my_data, tot_c = ((1000*my_data[,3]*10^my_data[,2]))/2.5)
total$Species <- as.factor(total$Species)
colnames(total) <- c("Species", "Dilution", "Colonies", "Total_Colonies")

my_sum <- summarise(group_by(total, Species), my_mean = mean(Total_Colonies), my_sd = sd(Total_Colonies) )

order_cols <- c("C. freundii", "E. coli", "K. pneumoniae", "S. flexneri", "E. coli + P1 wt")

plot_t <- ggplot() +
  # geom_bar(data = my_sum, stat = "identity", aes(x = factor(Species, order_cols) , y=my_mean), colour = "black", fill = "#5DA899", linewidth = 1, width = 0.75) +
  geom_errorbar(data = my_sum, aes(x = factor(Species, order_cols), y = my_mean, ymin = my_mean, ymax = my_mean), width = 0.5, linewidth = 1) +
  geom_point(data = total, aes(x = factor(Species, order_cols), y = Total_Colonies, color = Species, fill = Species), shape = 21, position = position_jitter(0.1), size = 2) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0, 1, 100000000, 1000000000, 10000000000, 100000000000)) +
  theme(legend.position = "none") +
  scale_y_break(c(2,50000000)) +
  ylab("Colonies/uL") +
  xlab("Species") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggtitle("Phage Particles")

print(plot_t) + theme(axis.line = element_line(color = "white"))

