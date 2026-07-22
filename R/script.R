# ----------------------------------------
# Author: Alisson Peter Brito Dopona
# PhD Student in Ecology - UFRN
# Artificial dens paper
# ------
# Data manipulation and trends visualization
# ----------------------------------------

# -------- Loading Packages
library(tidyverse)
library(mgcv)
library(GGally)
library(gratia)
library(flextable)
# Open graphics in other window
options(device = "x11")

# Loading dataset
dat <- read.csv("Data/data.csv", 
                stringsAsFactors = T)

# --------------------------------- Checking data

glimpse(dat)
str(dat)
summary(dat$environment)

# ------------------------------- Creating variables
# Seascape
dat$seascape <- ifelse(dat$environment == "Flat Biogenic Plateaus (n=67)" |
                       dat$environment == "Gravel Beds (n=57)",
                       "Modified", "Unmodified")
dat$seascape <- as.factor(dat$seascape)
#ifelse(all_data$Habitat %in% c("Cascalho", "Restinga"),"Modified", "Unmodified") - Maria code

# Type boat
dat$type_boat <- ifelse(dat$year >= 2009, "engine_boat", "sailboat")
dat$type_boat <- as.factor(dat$type_boat)

# Period
dat$period <- ifelse(dat$year <= 2012, "Before", "After")
dat$period <- as.factor(dat$period)

table(dat$period)

# ------------------------------- Ordering variables

# Environment
dat$environment <- factor(dat$environment, 
                          levels = c("Shallow Sedimentary Reefs (n=41)",
                                     "Outer Reef Slope (n=30)",
                                     "Gravel Beds (n=57)",
                                     "Flat Biogenic Plateaus (n=67)"))
summary(dat$environment)


# Period
dat$period <- factor(dat$period, levels = c ("Before", "After"))

# Type Boat
dat$type_boat <- factor(dat$type_boat, levels = c ("sailboat", "engine_boat"))


# ------------------------------ Computing CPUE
dat$cpue <- (dat$catch/(dat$n_divers*dat$time_fishing_h))

summary(dat$cpue)

#####

str(dat$name)


# n observations
dat %>%
  distinct(name, environment) %>%
  count(environment)

# n fishers
dat %>% 
  distinct(name, local) %>% 
  count(local)

# n fishers
dat %>% 
  distinct(name, boat_year) %>% 
  count(name)


# n boat
dat %>% 
  distinct(name, boat_year) %>% 
  count(boat_year)


# Fishers Age

mean(dat$age)
sd(dat$age)

# Fishers experience
mean(dat$fishing_exp)
sd((dat$fishing_exp))

# Octopus experience
mean(dat$octopus_exp)
sd((dat$octopus_exp))

#-----------------------------------------------------
#                    Data exploration                #
# ----------------------------------------------------

#Boxplots - Cath and CPUE
ggplot(dat, aes(x = environment, y = catch))+
  geom_boxplot()+
  facet_wrap(~pots)

ggplot(dat, aes(x = environment, y = cpue))+
  geom_boxplot()+
  facet_wrap(~pots)

# Effot
ggplot(dat,aes(x = environment, y = time_fishing_h))+
  geom_boxplot()+
  facet_wrap(~pots)

effot <- dat$n_divers*dat$time_fishing_h
ggplot(dat,aes(x = environment, y = effot))+
  geom_boxplot()+
  facet_wrap(~pots)

#Dotplots
dotchart(dat$catch)
dotchart(dat$cpue)

ggplot(dat,aes(x = catch))+
  geom_dotplot()+
  facet_wrap(~environment)

ggplot(dat,aes(x = cpue))+
  geom_dotplot()+
  facet_wrap(~environment)

#Histogram
ggplot(dat,aes(x = catch))+
  geom_histogram()

ggplot(dat,aes(x = cpue))+
  geom_histogram()

ggplot(dat,aes(x = catch))+
  geom_histogram()+
  facet_wrap(~environment)

ggplot(dat,aes(x = cpue))+
  geom_histogram()+
  facet_wrap(~environment)

#QQ-plot
#Kg_day
kg_day_SQ <- sqrt(dat$catch)
kg_day_SQ4 <- dat$catch^(0.25)
kg_day_log <- log(dat$catch+1)

op<-par(mfrow=c(2,2), mar=c(4,3,3,2))
qqnorm(dat$catch,main="None (p=1)")
qqline(dat$catch)
qqnorm(kg_day_SQ,main="Square root (p=0.5)")
qqline(kg_day_SQ)
qqnorm(kg_day_SQ4,main="Fourth root (p=0.25)")
qqline(kg_day_SQ4)
qqnorm(kg_day_log,main="Logarirthmic (p=0)")
qqline(kg_day_log)
par(op)

#cpue
cpue_SQ <- sqrt(dat$cpue)
cpue_SQ4 <- dat$cpue^(0.25)
cpue_log <- log(dat$cpue+1)

op<-par(mfrow=c(2,2), mar=c(4,3,3,2))
qqnorm(dat$cpue,main="None (p=1)")
qqline(dat$cpue)
qqnorm(cpue_SQ,main="Square root (p=0.5)")
qqline(cpue_SQ)
qqnorm(cpue_SQ4,main="Fourth root (p=0.25)")
qqline(cpue_SQ4)
qqnorm(cpue_log,main="Logarirthmic (p=0)")
qqline(cpue_log)
par(op)

#scarplot
op<-par(mfrow=c(2,2), mar=c(4,3,3,2))
plot(dat$cpue ~ dat$time_fishing_h, main = "CPUE/h")
plot(dat$cpue ~ dat$n_divers, main = "CPUE/Fisher")
plot(dat$catch ~ dat$time_fishing_h, main = "Catch/h")
plot(dat$catch ~ dat$n_divers, main = "Catch/Fisher")
par(op)

#Pairsplot
GGally::ggpairs(dat,
                columns = 12:17)

GGally::ggpairs(dat, 
                columns = 13:15)

GGally::ggpairs(dat, 
                columns = 13:15,
                aes(colour = pots,
                    alpha = 0.5))

GGally::ggpairs(dat,
                columns = c(6,8,11,16,19))

pairs(dat[,13:17],
      panel = function(x,y){
        points(x,y)
        abline(lm(x~y),
               col="red")})


# Coplot

coplot(cpue ~ year | environment * period, data = dat)

## Transformation
ggplot(dat, aes(x = year, y = log(cpue)))+
  geom_point()+
  facet_wrap(~environment)


############################################################################################
#                                       Vizualização                                        #
#############################################################################################
ggplot(dat, aes(x = year, y = cpue)) +
  geom_point(aes(alpha = 0.2,
                 colour = seascape,
                 fill = seascape)) +
  geom_smooth(aes(color = seascape,
                  fill = seascape),
              method = "gam",
              size = 0.5)+
  geom_vline(xintercept = 2012,
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


##### Effort

dat %>% 
  distinct(name, boat_year) %>% 
  count(boat_year) %>% 
  ggplot(aes(x = boat_year, 
             y = cumsum(n))) +
  geom_area(fill = "cornflowerblue")+
  geom_vline(xintercept = 2012,
             colour = "black",
             linetype = "dashed")+
  labs(x = "Year",
       y = "Cumulative nº of vessels") +
  ylim(0, 80)+
  theme_test(base_size = 12)

ggsave("Output/fig_effort.png",dpi = 300, width = 3, height = 3)
# ###########################################################################################
#                                        Modelo GAM                                         #
#############################################################################################

# Baci model

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

# Cheking
summary(mod1)

# Evaluiting model assumptions with gratia package
gratia::appraise(mod1)&
  theme_bw()

mgcv::k.check(mod1)


gratia::draw(mod1) &
  theme_bw()

# Mod2

mod2 <- mgcv::gam(cpue ~ environment*period +
                    type_boat+
                    s(fishing_exp)+
                    s(octopus_exp)+
                    s(year, k = 5) +
                    s(name, bs = "re"),
                  family = Gamma(link = "log"),
                  data = dat,
                  method = "REML")

# Cheking
summary(mod2)

# Evaluiting model assumptions with gratia package
gratia::appraise(mod2)&
  theme_bw()

mgcv::k.check(mod2)


gratia::draw(mod2) &
  theme_bw()

# Mod 3

mod3 <- mgcv::gam(cpue ~ environment*period +
                    pots+
                    type_boat+
                    s(fishing_exp)+
                    s(year, k = 5) +
                    s(name, bs = "re"),
                  family = Gamma(link = "log"),
                  data = dat,
                  method = "REML")

# Cheking
summary(mod3)

# Evaluiting model assumptions with gratia package
gratia::appraise(mod3)&
  theme_bw()

mgcv::k.check(mod3)


gratia::draw(mod3) &
  theme_bw()


###########################################################################################
#                           Incorporate accumulative n vessels                            #
###########################################################################################

# Primeiro criei um data frame que contenha o ano, quantidade de embarcaçao que começou atuar naquele ano, e o correspondente número de acúmulo de embarcaçoes atuando naquele ano.

vessel_acc <- dat %>%
  distinct(name, boat_year) %>%
  count(boat_year, name = "acc_vessel")  %>% 
  arrange(boat_year) %>%
  mutate(acc_vessel = cumsum(acc_vessel))

names(vessel_acc) <- c("year", "acc_vessel")

dat1 <- tibble(year = c(1981:1989),
               acc_vessel = 1)

dat2 <- tibble(year = c(1991, 1993, 1998, 1999, 2001, 2003, 2012, 2013),
               acc_vessel = c(2, 4,10,10,15,16,34,34))


vessel <- rbind(vessel_acc, dat1, dat2)

vessel <- vessel[order(vessel$acc_vessel),]

dat <- dat %>%
  left_join(vessel, by = "year")


# ----------------------------------------------------------------------------------------#
#                                     Standardized CPUE                                   #
# -----------------------------------------------------------------------------------------


dat_modified <- droplevels(dat %>% 
                             filter(
                               seascape == "Modified"
                             ))

stdz_cpue <- mgcv::gam(cpue ~ environment + period +
                    pots +
                    s(year, by = environment, k = 5) +
                    s(acc_vessel, bs = "tp") +
                    s(name, bs = "re"),
                  family = Gamma(link = "log"),
                  data = dat_modified,
                  method = "REML")


summary(stdz_cpue)


dat2 <- dat_modified
dat2$pots <- "No"
#dat2$acc_vessel <- 33


dat_modified$cpue_standardized <- predict(
  stdz_cpue, 
  newdata = dat_modified, 
  type = "response",
  exclude = "s(name)"
)


ggplot(dat_modified, aes(x = year)) +
  geom_smooth(aes(y = cpue_standardized, color = "Standardized"), alpha = 0.5) +
  facet_wrap(~environment) +
  theme_bw()


########################             Predicting

dat2 <- dat

dat2$pots <- as.factor("No")


dat2$cpue_standardized <- predict(
  stdz_cpue,
  newdata = dat2,
  type = "response",
  exclude = "s(name)" 
)


dat_unmodified <- droplevels(dat %>% 
                               filter(
                                 seascape == "Unmodified"
                               ))

dat_modified <- droplevels(dat2 %>% 
                               filter(
                                 seascape == "Modified"
                               ))

###

dat_unmodified <- dat_unmodified[, c("year", "cpue", "environment", "seascape")]

dat_modified <- dat_modified[, c("year", "cpue_standardized", "environment", "seascape")]

names(dat_modified) <- c("year", "cpue", "environment", "seascape")

dat3 <- rbind(dat_unmodified, dat_modified)



ggplot(dat3, aes(x = year, 
                 y = cpue, 
                 colour = seascape,
                 fill = seascape))+
  geom_smooth(method = "gam")+
  geom_vline(xintercept = 2012,
             colour = "black",
             linetype = "dashed")+
  facet_wrap(~environment,
             scale = "free")+
  ylim(0,10)+
  theme_test(base_size = 12)+
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("cornflowerblue", "black"))+
  scale_fill_manual(values = c("cornflowerblue", "black"))
  


