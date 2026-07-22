# -----------------------------------------------------------------------------------------
# Author: Alisson Peter Brito Dopona
# PhD Student in Ecology - UFRN
# Attraction x Production script
# Organizing Data Base
# ------------------------------------------------------------------------------------------


# -------- Loading Packages
library(tidyverse)

# Loading dataset
dat <- read.csv("Data/Raw_Data/data_base.csv", 
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
dat$period <- as.factor(ifelse(dat$year < 2013,
                               "Before",
                               "After"))

# Seascape
dat$seascape <- as.factor(ifelse(dat$environment == "Restingas" | 
                                   dat$environment == "Cascalho",
                                 "Modified", "Unmodified"))


# ------------------------------- Mutating Variables
dat <- dat %>% 
  mutate(environment = recode(environment,
                              Cascalho = "Gravel Beds (n=57)",
                              Parrachos = "Shallow Sedimentary Reefs (n=41)",
                              Restingas = "Flat Biogenic Plateaus (n=67)",
                              Risca = "Outer Reef Slope (n=30)"))

# ------------------------------- Ordering variables

# Environment
#dat$environment <- factor(dat$environment, 
#                          levels = c("Shallow Reef (n=41)",
#                                     "Deep Reef (n=30)",
#                                     "Shallow Rhodolith Bed (n=57)",
#                                     "Deep Rhodolith Bed (n=67)"))
# Period
dat$period <- factor(dat$period, 
                     levels = c ("Before", "After"))

# Type Boat
dat$type_boat <- factor(dat$type_boat, 
                        levels = c ("sailboat", "engine_boat"))


dat2 <- dat[,c(1,2,4,5,6,7,8,10,11,12,14,15,16,17,18)]

write.csv(dat2, "Data/data.csv",
          row.names = FALSE)
