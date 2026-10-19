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
                  family = tw(link = "log"),
                  data = dat2,
                  method = "REML")

mod2 <- mgcv::gam(cpue ~ environment + 
                    pots +
                    s(fishing_exp) +
                    s(year, k = 5) +
                    s(acc_vessel, bs = "tp") +
                    s(name, bs = "re"),
                  family = tw(link = "log"),
                  data = dat2,
                  method = "REML")

mod3 <- mgcv::gam(cpue ~ environment + 
                    pots +
                    s(year, k = 5) +
                    s(acc_vessel, bs = "tp") +
                    s(name, bs = "re"),
                  family = tw(link = "log"),
                  data = dat2,
                  method = "REML")

mod4 <- mgcv::gam(cpue ~ environment +
                    s(year, k = 5) +
                    s(acc_vessel, bs = "tp") +
                    s(name, bs = "re"),
                  family = tw(link = "log"),
                  data = dat2,
                  method = "REML")


mod5 <- mgcv::gam(cpue ~ environment + 
                    pots +
                    s(year, k = 5) +
                    s(name, bs = "re"),
                  family = tw(link = "log"),
                  data = dat2,
                  method = "REML")

# AIC
AIC(mod1, mod2, mod3, mod4, mod5)

# Por que eu deveria manter as duas variáveis? Ano e Periodo de fato compartilham muita explicação. Porém, a variável ano busca explicar a variação ao longo de toda a série temporal. Nós estamos preocupados com a variação em relação ao período: antes x depois.

summary(mod3)

gratia::draw(mod3) &
  theme_bw()

gratia::appraise(mod3,
                 method = 'simulate',
                 seed = 123) &
  theme_bw()


# O ano nao é significativo, mas mantemos ele porque é o nosso eixo x. E essa nao significância, na verdade, mostra que a CPUE nao tem mudado ao longo do tempo. Ou seja, as mudanças observadaso ano sao devido as mudanças no tempo. Na verdade, podemos pensar que essa mudança pode ser pq os potes puxaram toda a explicação da variação. 

stdz_cpue <- mod5

# --- Prediction
dat5 <- dat2
dat5$pots <- as.factor("No")

# ---
dat5$cpue_standardized <- predict(
  stdz_cpue,
  newdata = dat5,
  type = "response",
  exclude = "s(name)")

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

