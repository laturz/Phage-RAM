library("readxl")
library("tidyverse")
library("ggbreak")

theme_set(theme_classic())

my_data <- read_excel("Phagemid_Species_06.20.24.xlsx")

my_data$Condition <- as.numeric(my_data$Condition)

my_data$Condition[my_data$Condition == 70] <- "P1 oriR"
my_data$Condition[my_data$Condition == 64] <- "pBBR1"
total <- cbind(my_data, tot_c = ((1000*my_data[,3]*10^my_data[,4]))/2.5)
my_data$Condition <- factor(my_data$Condition, c("pBBR1", "P1 oriR"))
my_data$Strain <- factor(my_data$Strain, c("C. freundii", "E. coli", "P. aeruginosa", "S. flexneri"))

colnames(total) <- c("Condition", "Species", "Colonies", "Dilution", "Total")

my_sum <- total %>% 
  group_by(Condition, Species) %>% 
  summarise(my_mean = mean(Total), my_sd = sd(Total) )

plot_t <- ggplot() +
  # geom_bar(data = my_sum, position = position_dodge(), stat = "identity", aes(x = Species, y=my_mean, fill = Condition), colour = "black", linewidth = 1, width = 0.75) +
  geom_errorbar(data = my_sum, position = position_dodge(0.75), aes(x = Species, y = my_mean, ymin = my_mean, ymax = my_mean, group = Condition), width = 0.5, linewidth = 1) +
  geom_jitter(data = total, position = position_dodge(0.75), aes(x = Species, y = Total, group = Condition, shape = Condition, color = Species), fill = 'white', size = 2) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(1, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  scale_y_break(c(2,100000)) +
  ggtitle("Phagemid Particles") +
  ylab("Colonies/mL Lysate") +
  xlab("Species") +
  theme(axis.title.x = element_text(hjust = 0.35)) +
  labs(shape = "Backbone")

print(plot_t) + theme(axis.line = element_line(color = "white"))

