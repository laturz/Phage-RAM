library("readxl")
library("tidyverse")
library("ggbreak")

theme_set(theme_classic())

my_data <- as.data.frame(read_excel("Combined_data.xlsx"))

my_data$Strain <- as.factor(my_data$Strain)

my_data$Condition[my_data$Condition == 70] <- "P1 oriR"
my_data$Condition[my_data$Condition == 64] <- "pBBR1"
my_data$Condition <- factor(my_data$Condition, levels = c("pBBR1", "P1 oriR", "P1", "Phagemid Control", "Phage Control"))
my_data$Strain <- factor(my_data$Strain, levels = c("C. freundii", "E. coli", "K. pneumoniae", "P. aeruginosa", "S. flexneri", "Control"))

total <- cbind(my_data, tot_c = ((1000*my_data[,3]*10^my_data[,4]))/2.5)

colnames(total) <- c("Condition", "Species", "Colonies", "Dilution", "Total")

my_sum <- total %>% 
  group_by(Condition, Species) %>% 
  summarise(my_mean = mean(Total), my_sd = sd(Total) )

plot_t <- ggplot() +
  geom_bar(data = my_sum, position = "dodge", stat = "identity", aes(x = factor(Species), y = my_mean, group = Condition, fill = Condition), colour = "black", width = 0.75) +
  geom_errorbar(data = my_sum, position = position_dodge(0.75), aes(x = factor(Species), y = my_mean, ymin = my_mean - my_sd, ymax = my_mean + my_sd, group = Condition), width = 0.5, linewidth = 0.75) +
  geom_jitter(data = total, position = position_dodge(0.75), aes(x = factor(Species), y = Total, group = Condition, fill = Species), color = "black", shape = 21, size = 2) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(1, 100000, 1000000, 10000000, 100000000, 1000000000, 10000000000, 100000000000)) +
  scale_y_break(c(2,1000000)) +
  ggtitle("Phagemid Particles") +
  ylab("Colonies/mL Lysate") +
  xlab("Species") +
  theme(axis.title.x = element_text(hjust = 0.35)) +
  labs(shape = "Backbone")

print(plot_t) + theme(axis.line = element_line(color = "white"))

