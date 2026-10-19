# -------------------------------------------------------------------------------------------
# Author: Alisson Peter Brito Dopona
# PhD Student in Ecology - UFRN
# Artificial dens paper
# ---
# Construction of models
# ------------------------------------------------------------------------------------------

#rm(list = ls())

# ----------------------------------------- Load the packages
library(tidyverse)
library(mgcv)
library(gratia)


# ----------------------------------------- Load the database
dat <- read.csv("Data/data.csv", 
                stringsAsFactors = T)

# Open graphics in other window
#options(device = "x11", x11 = list(width = 10, height = 7))

# ----------------------------------------- Calculate CPUE
dat$cpue <- (dat$catch/(dat$n_divers*dat$time_fishing_h))
summary(dat$cpue)

# .............................. Change cariables
dat <- dat |> 
  mutate(environment = recode(environment,
                              "Flat Biogenic Plateaus (n=67)" = "Biogenic Plateaus",
                              "Gravel Beds (n=57)" = "Gravel Beds",
                              "Outer Reef Slope (n=30)" = "Deep Reef",
                              "Shallow Sedimentary Reefs (n=41)" = "Shallow Reef"))

# ----------------------------------------- Organize the variables
# Environment
dat$environment <- factor(dat$environment, 
                          levels = c("Shallow Reef",
                                     "Deep Reef",
                                     "Gravel Beds",
                                     "Biogenic Plateaus"))
# Period
dat$period <- factor(dat$period, 
                     levels = c ("Before", "After"))

# Type Boat
dat$type_boat <- factor(dat$type_boat, 
                        levels = c ("sailboat", "engine_boat"))


# ----------------------------------------- Create plots

p_obj <- plot(mod4, residuals = TRUE)
p_sr <- p_obj[[1]]
p_dr <- p_obj[[2]]
p_gb <- p_obj[[3]]
p_bp <- p_obj[[4]]


# Shallow reefs
sm_sr <- as.data.frame(p_sr[c("x", "se", "fit")])
data_sr <- as.data.frame(p_sr[c("raw", "p.resid")])
# Deep reefs
sm_dr <- as.data.frame(p_dr[c("x", "se", "fit")])
data_dr <- as.data.frame(p_dr[c("raw", "p.resid")])
# Gravel Beds
sm_gb <- as.data.frame(p_gb[c("x", "se", "fit")])
data_gb <- as.data.frame(p_gb[c("raw", "p.resid")])
# Biogenic Plateaus
sm_bp <- as.data.frame(p_bp[c("x", "se", "fit")])
data_bp <- as.data.frame(p_bp[c("raw", "p.resid")])


## plot Shallow Reefs
plot_sr <- ggplot(sm_sr, aes(x = x, y = fit)) +
  geom_rug(data = data_sr, mapping = aes(x = raw, y = NULL),
           sides = "b") +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3) +
  geom_line() +
  geom_vline(xintercept = 2013, linetype = "dashed", colour = "gray") +
  labs(x = "", y = " ") +
  theme_classic()+
  ggtitle("Shallow Reefs")


## plot Deep Reefs
plot_dr <- ggplot(sm_dr, aes(x = x, y = fit)) +
  geom_rug(data = data_dr, mapping = aes(x = raw, y = NULL),
           sides = "b") +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3) +
  geom_line() +
  geom_vline(xintercept = 2013, linetype = "dashed", colour = "gray") +
  labs(x = "", y = "") +
  theme_classic()+
  ggtitle("Deep Reefs")

## plot Gravel Beds
plot_gb <- ggplot(sm_gb, aes(x = x, y = fit)) +
  geom_rug(data = data_gb, mapping = aes(x = raw, y = NULL),
           sides = "b") +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3,
              fill = "cornflowerblue") +
  geom_line(colour = "cornflowerblue") +
  geom_vline(xintercept = 2013, linetype = "dashed", colour = "gray") +
  labs(x = "Year", y = "") +
  theme_classic()+
  ggtitle("Gravel Beds")

## plot Biogenic Plateus
plot_bp <- ggplot(sm_bp, aes(x = x, y = fit)) +
  geom_rug(data = data_bp, mapping = aes(x = raw, y = NULL),
           sides = "b") +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3,
              fill = "cornflowerblue") +
  geom_line(colour = "cornflowerblue") +
  geom_vline(xintercept = 2013, linetype = "dashed", colour = "gray") +
  labs(x = "Year", y = "") +
  theme_classic()+
  ggtitle("Biogenic Plateus")



plot <- gridExtra::grid.arrange(plot_sr, plot_dr, plot_gb, plot_bp, ncol=2, nrow=2)

plot
ggsave("Output/gam_effect.png", plot = plot,
       width  = 7, 
       height = 6, 
       dpi = 600, 
       bg = "white")



library(mgcv)            # Fit and interrogate GAMs
library(tidyverse)       # Tidy and flexible data manipulation
library(marginaleffects) # Compute conditional and marginal effects
library(ggplot2)         # Flexible plotting
library(patchwork)       # Combining ggplot objects

coef(mod4)
plot(mod4, select = 2, shade = TRUE)
abline(h = 0, lty = 'dashed')
abline(v = 2013, lty = 'dashed')

marginaleffects::plot_predictions(mod4, condition = c('year', 'environment'),
                                  type = 'response',
                                  rug = TRUE)+
  facet_wrap(~environment,
             scale = 'free')+
  theme_classic()








