# -------------------------------------------------------------------------------------------
# Author: Alisson Peter Brito Dopona
# PhD in Ecology - UFRN
# Attraction x Production script
# Construction of models
# ------------------------------------------------------------------------------------------


# -------- Loading Packages
library(tidyverse)
library(mgcv)
library(GGally)
library(gratia)
library(performance)

# Open graphics in other window
options(device = "x11", x11 = list(width = 10, height = 7))

# Loading dataset
dat <- read.csv("Data/data.csv", 
                stringsAsFactors = T)

# -------------------------------------------------------------------------------------------
#                                 HANDLING VARIABLES                                        #
# -------------------------------------------------------------------------------------------

# ------------------------------- Creating Variables

# Type boat
dat$type_boat <- as.factor(ifelse(dat$year >= 2009,
                                  "engine_boat",
                                  "sailboat"))

# Period
dat$period <- as.factor(ifelse(dat$year <= 2013,
                               "Before",
                               "After"))

# Seascape
dat$seascape <- as.factor(ifelse(dat$environment == "Restinga (n=67)" | 
                                   dat$environment == "Cascalho (n=57)",
                                 "Modified", "Unmodified"))



# ------------------------------- Mutating Variables
#dat <- dat %>% 
#  mutate(environment = recode(environment,
#                              Cascalho = "Cascalho (n=57)",
#                              Parrachos = "Parrachos (n=41)",
#                              Restingas = "Restinga (n=67)",
#                              Risca = "Risca (n=30)"))
#
# ------------------------------- Ordering variables

# Environment
dat$environment <- factor(dat$environment, 
                          levels = c("Parrachos (n=41)",
                                     "Risca (n=30)",
                                     "Cascalho (n=57)",
                                     "Restinga (n=67)"))
# Period
dat$period <- factor(dat$period, 
                     levels = c ("Before", "After"))

# Type Boat
dat$type_boat <- factor(dat$type_boat, 
                        levels = c ("sailboat", "engine_boat"))

# -------------------------------------------------------------------------------------------
#                                     COMPUTING CPUE                                        #
# -------------------------------------------------------------------------------------------

dat$cpue <- (dat$catch/(dat$n_divers*dat$time_fishing_h))
summary(dat$cpue)

# -------------------------------------------------------------------------------------------
#                                     CREATING MODELS                                       #
# -------------------------------------------------------------------------------------------
# Retirando as varáveis pots e tipo de embarcação não muda nada na explicação do modelo.


# --------------------- model1
mod1 <- mgcv::gam(cpue ~ environment*period +
                    pots+
                    type_boat+
                    s(fishing_exp)+
                    s(octopus_exp)+
                    s(year, k = 5) +
                    s(name, bs = "re"),
                  family = Gamma(link = "log"),
                  data = dat,
                  method = "REML")

summary(mod1)

gratia::draw(mod1) &
  theme_bw()



# --------------------- model2
mod2 <- mgcv::gam(cpue ~ environment*period +
                    type_boat+
                    s(fishing_exp)+
                    s(octopus_exp)+
                    s(year, k = 5) +
                    s(name, bs = "re"),
                  family = Gamma(link = "log"),
                  data = dat,
                  method = "REML")
summary(mod2)

gratia::draw(mod2) &
  theme_bw()

# --------------------- model3
mod3 <- mgcv::gam(cpue ~ environment*period +
                    s(fishing_exp)+
                    s(octopus_exp)+
                    s(year, k = 5) +
                    s(name, bs = "re"),
                  family = Gamma(link = "log"),
                  data = dat,
                  method = "REML")

summary(mod3)

gratia::draw(mod3) &
  theme_bw()

# --------------------- model4
mod4 <- mgcv::gam(cpue ~ environment*period +
                    s(fishing_exp)+
                    s(year, k = 5) +
                    s(name, bs = "re"),
                  family = Gamma(link = "log"),
                  data = dat,
                  method = "REML")

summary(mod4)

  gratia::draw(mod4) &
  theme_bw()

# --------------------- model5
mod5 <- mgcv::gam(cpue ~ environment*period +
                    s(year, k = 5) +
                    s(name, bs = "re"),
                  family = Gamma(link = "log"),
                  data = dat,
                  method = "REML")

summary(mod5)

gratia::draw(mod5) &
  theme_bw()


# -------------------- AIC

AIC(mod1, mod2, mod3, mod4, mod5)








