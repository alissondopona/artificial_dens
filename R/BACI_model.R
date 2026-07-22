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

# ----------------------------------------- Organize the variables
# Environment
dat$environment <- factor(dat$environment, 
                          levels = c("Shallow Sedimentary Reefs (n=41)",
                                     "Outer Reef Slope (n=30)",
                                     "Gravel Beds (n=57)",
                                     "Flat Biogenic Plateaus (n=67)"))
# Period
dat$period <- factor(dat$period, 
                     levels = c ("Before", "After"))

# Type Boat
dat$type_boat <- factor(dat$type_boat, 
                        levels = c ("sailboat", "engine_boat"))


# ----------------------------------------- Create models
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

# --------------------- model3
mod3 <- mgcv::gam(cpue ~ environment*period +
                    s(fishing_exp)+
                    s(octopus_exp)+
                    s(year, k = 5) +
                    s(name, bs = "re"),
                  family = Gamma(link = "log"),
                  data = dat,
                  method = "REML")

# --------------------- model4
mod4 <- mgcv::gam(cpue ~ environment*period +
                    s(fishing_exp)+
                    s(year, k = 5) +
                    s(name, bs = "re"),
                  family = Gamma(link = "log"),
                  data = dat,
                  method = "REML")

# --------------------- model5
mod5 <- mgcv::gam(cpue ~ environment*period +
                    s(year, k = 5) +
                    s(name, bs = "re"),
                  family = Gamma(link = "log"),
                  data = dat,
                  method = "REML")

# ----------------------------------- AIC and BIC

AIC(mod1, mod2, mod3, mod4, mod5)
BIC(mod1, mod2, mod3, mod4, mod5)

# ----------------------------------------- Check and visualize model diagnostics
summary(mod5)

gratia::draw(mod5) &
  theme_bw()

gratia::appraise(mod5,
                 method = 'simulate',
                 seed = 123) &
  theme_bw()

mgcv::k.check(mod5)

# ----------------------------------------- Calculate changes in CPUE

# Since your model uses a log link function, the coefficients are on the log scale. To get proportional changes in CPUE, we exponentiate them:

# O que esse calculo quer dizer:
# Ao calcular o exponencial do(s) interceptos, mostramos o que a CPUE se tornou em relaçao ao que foi. Porém, queremos saber a diferença, entao subtraímos por 1. Para transformar em porcentagem, multiplicamos por 100 e arrendondamos para uma casa decimal.

# ---- Parrachos
parrachos <- as.numeric(100*(round(exp(mod5$coefficients[5])-1,3)))

# ---- Risca
risca <- as.numeric(100*(round(exp(mod5$coefficients[6]+mod5$coefficients[5])-1,3)))

# ---- Cascalho
cascalho <- as.numeric(100*(round(exp(mod5$coefficients[7]+mod5$coefficients[5])-1,3)))

# ---- Restinga
restinga <- as.numeric(100*(round(exp(mod5$coefficients[8]+mod5$coefficients[5])-1,3)))

# ------------------ Risca com parrachos
as.numeric(100*(round(exp(mod5$coefficients[2])-1,2)))


# ----------------------------------------- Visualizing changes in CPUE
# --- Organize database
dat2 <- data.frame(
  environment = c("Gravel Beds\n(n=57)",
                  "Flat Biogenic\nPlateaus (n=67)",
                  "Shallow Sedimentary\nReefs (n=41)",
                  "Outer Reef\nSlope (n=30)"),
  change      = c(cascalho, restinga, parrachos, risca),
  sig         = c("***", "***", "ns", "***"),
  seascape   = c("Modified", "Modified", "Unmodified", "Unmodified"))

dat2 <- dat2 |>
  mutate(environment = factor(environment,
                              levels = environment[order(change)]))

# --- Create plot
ggplot(dat2, aes(x = change, y = environment, fill = seascape)) +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey20") +
  geom_col(aes(alpha = sig), width = 0.55, color = NA) +
  geom_text(aes(label = ifelse(change > 0,
                               paste0("+", change, "%"),
                               paste0(change, "%")),
                hjust = ifelse(change > 0, 1.15, -0.15)),
                color = "white", fontface = "bold", size = 3.4) +
  geom_text(aes(label = sig, hjust = ifelse(change > 0, -0.3, 1.3), color = sig),
            fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Modified" = "cornflowerblue",
                               "Unmodified" = "black"), name = NULL) +
  scale_color_manual(values = c("***" = "grey20", "ns" = "grey60"), guide = "none") +
  scale_alpha_manual(values = c("***" = 1, "ns" = 0.6), guide = "none") +
  scale_x_continuous(limits = c(-105, 75),
                     breaks = seq(-100, 75, by = 25),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0)) +
  labs(x = "Proportional Change in CPUE (%)", y = NULL) +
  theme_test(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13, hjust = 0,
                                  margin = margin(b = 4)),
        plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0,
                                  margin = margin(b = 16)),
        plot.caption = element_text(size = 9, color = "grey50", hjust = 0,
                                  margin = margin(t = 12)),
        axis.text.y = element_text(size = 10.5, color = "grey15", lineheight = 1.2),
        axis.text.x = element_text(size = 9.5, color = "grey30"),
        axis.title.x = element_text(size = 10, color = "grey20",
                                   margin = margin(t = 8)),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey88", linewidth = 0.4),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        plot.margin = margin(20, 30, 16, 16))


ggsave("Output/cpue_change_plot.png", 
       width  = 7, 
       height = 4, 
       dpi = 300, 
       bg = "white")













