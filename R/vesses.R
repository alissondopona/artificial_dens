# ----------------------------------------- Load the database
dat <- read.csv("Data/data.csv", 
                stringsAsFactors = T)
glimpse(dat)

dat |> filter(environment == "Outer Reef Slope (n=30)") |> 
  summary(year)

# ----------------------------------------- Calculate CPUE
dat$cpue <- (dat$catch/(dat$n_divers*dat$time_fishing_h))
summary(dat$cpue)
)

# ----------------------------------------- Select modified environment
dat2 <- droplevels(dat %>% 
                     filter(
                       seascape == "Modified"
                     ))

# ----------------------------------------- Calculate accumulative vessels
# Primeiro criei um data frame que contenha o ano e o número de acúmulo de embarcaçoes atuando naquele ano.
vessel_acc <- dat %>%
  group_by(environment) |> 
  distinct(name, boat_year) %>%
  count(boat_year, name = "acc_vessel")  %>% 
  arrange(boat_year) %>%
  mutate(acc_vessel = cumsum(acc_vessel))

names(vessel_acc) <- c("environment", "year", "acc_vessel")

glimpse(vessel_acc)

ggplot(vessel_acc, aes(x = year, y = acc_vessel)) +
  geom_line() + 
  facet_wrap(~ environment)


dat3 <- tibble(year = c(1981:1989),
               acc_vessel = 1)

dat4 <- tibble(year = c(1991, 1993, 1998, 1999, 2001, 2003, 2012, 2013),
               acc_vessel = c(2, 4,10,10,15,16,34,34))

vessel <- rbind(vessel_acc, dat3, dat4)

vessel <- vessel[order(vessel$acc_vessel),]

dat2 <- dat2 %>%
  left_join(vessel, by = "year")