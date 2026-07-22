# ----------------------------------------
# Author: Alisson Peter Brito Dopona
# PhD Student in Ecology - UFRN
# Attraction x Production script
# Fishers Perception
# ----------------------------------------

# ---------------------------------------------------------------------------------------
#                                  FISHERS PERCEPTION                                   #
# ---------------------------------------------------------------------------------------

dat_perc <- read.csv("Data/data_fp.csv", 
                     stringsAsFactors = TRUE)

glimpse(dat_perc)

dat_perc$qntd_octopus<- factor(dat_perc$qntd_octopus, 
                               levels = c("Increase",
                                          "Decrease",
                                          "No change"))

dat_perc$natural_x_attract_increase<- factor(dat_perc$natural_x_attract_increase, 
                                             levels = c("Natural",
                                                        "Attraction"))
#### 

dat1 <- dat_perc %>% 
  count(qntd_octopus) %>% 
  mutate(porc = round((prop = n / sum(n))*100)) 

dat2 <- dat_perc %>% 
  count(natural_x_attract_increase) %>% 
  mutate(porc = round((prop = n / sum(n))*100)) 


### Graphics

par(mfrow = c(1,2))
ggplot2::ggplot(dat1, aes(x = qntd_octopus, y = porc))+
  geom_bar(stat = "identity", 
           fill = "cornflowerblue",
           width = 0.8) + 
  geom_text(aes(label = paste0(round(porc, 1), "%")), 
            vjust = -0.5) +
  labs(x = "Octopus Abundance",
       y = "Percentage (%)") +
  scale_y_continuous(limits = c(0, 90)) +
  theme_test(base_size = 12) +
  ggplot2::ggplot(dat2, aes(x = natural_x_attract_increase, y = porc))+
  geom_bar(stat = "identity", 
           fill = "cornflowerblue",
           width = 0.6) + 
  geom_text(aes(label = paste0(round(porc, 1), "%")), 
            vjust = -0.5) +
  labs(x = "Octopus Catch",
       y = NULL) +
  scale_y_continuous(limits = c(0, 90)) +
  theme_test(base_size = 12)


ggsave("Output/fig_fp.png",dpi = 300, width = 6, height = 3)
