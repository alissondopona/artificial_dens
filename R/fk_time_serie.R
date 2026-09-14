# ----------------------------------------
# Author: Alisson Peter Brito Dopona
# PhD Student in Ecology - UFRN
# Attraction x Production script
# CPUE changes
# ----------------------------------------

#rm(list = ls())

# ----------------------------------------- Load the packages
library(ggplot2)
library(dplyr)

# ----------------------------------------- Load the database
dat <- read.csv("Data/data.csv", 
                stringsAsFactors = T)
glimpse(dat)

# ----------------------------------------- Organize the variables
# Period
dat$period <- factor(dat$period, 
                     levels = c ("Before", 
                                 "After"))

# Type Boat
dat$type_boat <- factor(dat$type_boat, 
                        levels = c ("sailboat", 
                                    "engine_boat"))

# Environment
dat <- dat %>% 
  mutate(environment = recode(environment,
                              "Flat Biogenic Plateaus (n=67)" = "FBP",
                              "Gravel Beds (n=57)" = "GB",
                              "Outer Reef Slope (n=30)" = "ORS",
                              "Shallow Sedimentary Reefs (n=41)" = "SSR"))

# Order

dat$environment <- factor(dat$environment, 
                          levels = c("SSR",
                                     "ORS",
                                     "GB",
                                     "FBP"))

# ----------------------------------------- Calculate CPUE
dat$cpue <- (dat$catch/(dat$n_divers*dat$time_fishing_h))
summary(dat$cpue)

# ----------------------------------------- Visualize plots
ggplot(dat, aes(x = year, y = cpue)) +
  geom_point(aes(alpha = 0.2,
                 colour = seascape,
                 fill = seascape,
                 shape = environment)) +
  geom_smooth(aes(color = seascape,
                  fill = seascape),
              method = "gam",
              size = 0.5)+
  geom_vline(xintercept = 2013,
             colour = "black",
             linetype = 2,
             alpha = 0.2) +
  labs(x = "Year", y = "CPUE(Kg/nº diver*fishing hours)")+
  facet_wrap(~environment,
             nrow = 2,
             ncol = 2) +
  theme_classic(base_size = 12)+
  theme(legend.position = "right",
        legend.title=element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())+
  scale_alpha(guide = 'none')+
  scale_color_manual(values = c("cornflowerblue", 
                                "black"), 
                     name = NULL)+
  scale_fill_manual(values = c("cornflowerblue", 
                               "black"), 
                    name = NULL)
  
ggsave("Output/fig_gam3.png", 
       dpi = 300, 
       width = 8, 
       height = 6)
