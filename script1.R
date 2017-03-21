# load data from NHANES

library(NHANES)
library(dplyr)

diab <- NHANES %>% 
  dplyr::select(Age, Gender, Diabetes, BMI, HHIncome, PhysActive) %>% 
  na.omit()

# change back to a regular data.frame
diab <- as.data.frame(diab)

# let's do some stuff via Rcmdr

library(Rcmdr)

# let's also look at rattle

library(rattle)
rattle()

