# ----------------------------------------
# Author: Alisson Peter Brito Dopona
# PhD Student in Ecology - UFRN
# Attraction x Production script
# CPUE changes
# ----------------------------------------

# ------------------------------ Loading Packages
library(tidyverse)
library(mgcv)
library(GGally)
library(gratia)
library(flextable)
#library(performance)

# Open graphics in other window
#options(device = "x11")

# ------------------------------- Loading dataset
dat <- read.csv("Data/data.csv", 
                stringsAsFactors = T)
glimpse(dat)


# ------------------------------- Ordering variables

# Environment
dat$environment <- factor(dat$environment, 
                          levels = c("Cascalho (n=57)",
                                     "Restinga (n=67)",
                                     "Parrachos (n=41)",
                                     "Risca (n=30)"))
# Period
dat$period <- factor(dat$period, levels = c ("Before", "After"))

# Type Boat
dat$type_boat <- factor(dat$type_boat, levels = c ("sailboat", "engine_boat"))


# ------------------------------ Computing CPUE
dat$cpue <- (dat$catch/(dat$n_divers*dat$time_fishing_h))
summary(dat$cpue)

# ------------------------------ Vizualizing

ggplot(dat, aes(x = year, y = cpue)) +
  geom_point(aes(alpha = 0.2,
                 colour = seascape,
                 fill = seascape)) +
  geom_smooth(aes(color = seascape,
                  fill = seascape),
              method = "gam",
              size = 0.5)+
  geom_vline(xintercept = 2013,
             colour = "black",
             linetype = "dashed") +
  labs(x = "Year", y = "CPUE(Kg/nº diver*fishing hours)")+
  facet_wrap(~environment, scales = 'free')+
  theme_test(base_size = 12)+
  theme(legend.position = "bottom")+
  labs(fill = "Seascape",
       color = "Seascape",)+
  scale_alpha(guide = 'none')+
  scale_color_manual(values = c("cornflowerblue", "black"), name = NULL)+
  scale_fill_manual(values = c("cornflowerblue", "black"), name = NULL)
  


ggsave("Output/fig_gam.png",dpi = 300, width = 6, height = 6)
