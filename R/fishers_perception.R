# ----------------------------------------
# Author: Alisson Peter Brito Dopona
# PhD Student in Ecology - UFRN
# Artificial dens paper
# ---
# Fishers Perception
# ----------------------------------------

#rm(list = ls())

# ----------------------------------------- Load the packages
library(ggplot2)
library(dplyr)
library(patchwork)

# ----------------------------------------- Load the database
dat_perc <- read.csv("Data/data_fp.csv", 
                     stringsAsFactors = TRUE)
glimpse(dat_perc)

# ----------------------------------------- Organize the variables
dat_perc$qntd_octopus<- factor(dat_perc$qntd_octopus, 
                               levels = c("Increase",
                                          "Decrease",
                                          "No change"))

dat_perc$natural_x_attract_increase<- factor(dat_perc$natural_x_attract_increase, 
                                             levels = c("Natural",
                                                        "Attraction"))

# ----------------------------------------- Calculate the percentages
dat1 <- dat_perc %>% 
  count(qntd_octopus) %>% 
  mutate(porc = round((prop = n / sum(n))*100)) 

dat2 <- dat_perc %>% 
  count(natural_x_attract_increase) %>% 
  mutate(porc = round((prop = n / sum(n))*100)) 


# ----------------------------------------- Visualize plots

ggplot2::ggplot(dat1, aes(x = qntd_octopus, y = porc))+
  geom_bar(stat = "identity", 
           fill = "cornflowerblue",
           width = 0.8) + 
  geom_text(aes(label = paste0(round(porc, 1), "%")), 
            vjust = -0.5) +
  labs(x = "Octopus Abundance",
       y = "Percentage (%)") +
  scale_y_continuous(limits = c(0, 90),
                     expand = expansion(0)) +
  theme_classic(base_size = 12) +
ggplot2::ggplot(dat2, aes(x = natural_x_attract_increase, y = porc))+
  geom_bar(stat = "identity", 
           fill = "cornflowerblue",
           width = 0.6) + 
  geom_text(aes(label = paste0(round(porc, 1), "%")), 
            vjust = -0.5) +
  labs(x = "Octopus Catch",
       y = NULL) +
  scale_y_continuous(limits = c(0, 90),
                     expand = expansion(0)) +
  theme_classic(base_size = 12)


ggsave("Output/fig_fp.png",
       dpi = 300, 
       width = 6, 
       height = 3)
