# ----------------------------------------
# Author: Alisson Peter Brito Dopona
# PhD Student in Ecology - UFRN
# Artificial dens paper
# ---
# Standardization CPUE script
# ----------------------------------------

#rm(list = ls())

# ----------------------------------------- Load the packages
library(tidyverse)
library(mgcv)
library(gratia)

# Open graphics in other window
#options(device = "wayland")

# ----------------------------------------- Load the database
dat <- read.csv("Data/data.csv", 
                stringsAsFactors = T)
glimpse(dat)

# ----------------------------------------- Calculate CPUE
dat$cpue <- (dat$catch/(dat$n_divers*dat$time_fishing_h))
summary(dat$cpue)

# ----------------------------------------- Select modified environment
dat2 <- droplevels(dat %>% 
                     filter(
                       seascape == "Modified"
                     ))

# ----------------------------------------- Calculate accumulative vessels
# Primeiro criei um data frame que contenha o ano e o número de acúmulo de embarcaçoes atuando naquele ano.
vessel_acc <- dat2 %>%
  distinct(name, boat_year) %>%
  count(boat_year, name = "acc_vessel")  %>% 
  arrange(boat_year) %>%
  mutate(acc_vessel = cumsum(acc_vessel))

names(vessel_acc) <- c("year", "acc_vessel")

dat3 <- tibble(year = c(1981:1989),
               acc_vessel = 1)

dat4 <- tibble(year = c(1991, 1993, 1998, 1999, 2001, 2003, 2012, 2013),
               acc_vessel = c(2, 4,10,10,15,16,34,34))

vessel <- rbind(vessel_acc, dat3, dat4)

vessel <- vessel[order(vessel$acc_vessel),]

dat2 <- dat2 %>%
  left_join(vessel, by = "year")

# ----------------------------------------- Standardize CPUE
# --- Create models
mod1 <- mgcv::gam(cpue ~ environment + 
                    pots +
                    s(fishing_exp) +
                    s(octopus_exp) +
                    s(year, k = 5) +
                    s(acc_vessel, bs = "tp") +
                    s(name, bs = "re"),
                  family = Gamma(link = "log"),
                  data = dat2,
                  method = "REML")

mod2 <- mgcv::gam(cpue ~ environment + 
                    pots +
                    s(fishing_exp) +
                    s(year, k = 5) +
                    s(acc_vessel, bs = "tp") +
                    s(name, bs = "re"),
                  family = Gamma(link = "log"),
                  data = dat2,
                  method = "REML")

mod3 <- mgcv::gam(cpue ~ environment + 
                    pots +
                    s(year, k = 5) +
                    s(acc_vessel, bs = "tp") +
                    s(name, bs = "re"),
                  family = Gamma(link = "log"),
                  data = dat2,
                  method = "REML")

# O ano nao é significativo, mas mantemos ele porque é o nosso eixo x. E essa nao significância, na verdade, mostra que a CPUE nao tem mudado ao longo do tempo. Ou seja, as mudanças observadaso ano sao devido as mudanças no tempo.
AIC(mod1, mod2, mod3)
stdz_cpue <- mod3

# --- Predic 
dat5 <- dat2
dat5$pots <- as.factor("No")

# ---
dat5$cpue_standardized <- predict(
  stdz_cpue,
  newdata = dat5,
  type = "response",
  exclude = "s(name)")

# ----------------------------------------- Visualize Standardization
x11(width = 15, height = 11)
ggplot2::ggplot(dat5, aes(x = year, 
                 y = cpue_standardized, 
                 colour = seascape,
                 fill = seascape))+
  geom_smooth(method = "gam")+
  geom_vline(xintercept = 2013,
             colour = "black",
             linetype = "dashed")+
  labs(x = "Year", y = "CPUE(Kg/Nº diver*Fishing Hours)") +
  facet_wrap(~environment,
             scale = "free")+
  ylim(0,6.5)+
  theme_test(base_size = 12) +
  theme(legend.position = "none")+
  scale_color_manual(values = c("cornflowerblue", "black"))+
  scale_fill_manual(values = c("cornflowerblue", "black"))

ggsave("Output/cpue_stdz.png", 
       width  = 6, 
       height = 3, 
       dpi = 300, 
       bg = "white")

# ----------------------------------------- Vizualize all environments
dat_unmodified <- droplevels(dat %>% 
                               filter(
                                 seascape == "Unmodified"
                               ))

dat_modified <- droplevels(dat5 %>% 
                             filter(
                               seascape == "Modified"
                             ))

# ----------------------------------------- Create dataframe
dat_unmodified <- dat_unmodified[, c("year", "cpue", "environment", "seascape")]

dat_modified <- dat_modified[, c("year", "cpue_standardized", "environment", "seascape")]

names(dat_modified) <- c("year", "cpue", "environment", "seascape")

dat3 <- rbind(dat_unmodified, dat_modified)

levels(dat5$environment)
dat5 <- dat5 %>% 
  mutate(environment = recode(environment,
                              "Flat Biogenic Plateaus (n=67)" = "Biogenic Plateaus",
                              "Gravel Beds (n=57)" = "Gravel Beds"))

# ----------------------------------------- Compare Smooths Recalled x Standardized CPUE
gam_recalled <- gam(cpue ~ 
                      s(year, 
                        by = environment, 
                        k = 5) +
                      environment,
                    family = Gamma(link = "log"),
                    data = dat5, 
                    method = "REML")
summary(gam_recalled)

gam_std <- gam(cpue_standardized ~ 
                 s(year, 
                   by = environment, 
                   k = 5) + 
                 environment,
               family = Gamma(link = "log"),
               data = dat5, 
               method = "REML")
summary(gam_std)

comp <- compare_smooths(gam_recalled, gam_std)


plot <- draw(comp)

plot[[1]] <- plot[[1]] + ggtitle("Biogenic Plateaus")
plot[[2]] <- plot[[2]] + ggtitle("Gravel Beds")

plot & geom_vline(xintercept = 2013,
             colour = "gray",
             linetype = "dashed") &
  theme_classic(base_size = 12) &
  theme(legend.position = "bottom") &
  labs(x = "Year") &
  scale_color_manual(values = c("cornflowerblue", "black"), 
                     name = NULL,
                     labels = c("GAM Recalled", "GAM Standardized")) &
  scale_fill_manual(values = c("cornflowerblue", "black"), 
                    name = NULL,
                    labels = c("GAM Recalled", "GAM Standardized"))

ggsave("Output/compare_smooths.png", 
       width  = 6, 
       height = 4, 
       dpi = 300, 
       bg = "white")

