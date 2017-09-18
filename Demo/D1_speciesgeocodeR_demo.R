library(tidyverse)
library(speciesgeocodeR)


#Use the bombacoids data here
###############################################
#load data and remove NAs
dat <- read_delim("input/lions_gbif.csv", delim = "\t") %>%
  filter(!is.na(decimallongitude))

#####################################################
#Automated cleaning 
flags <- CleanCoordinates(dat[,c("decimallongitude", "decimallatitude")],
                          countries = unlist(dat[,"countrycode"]), 
                          institutions = T, inst.rad = 0.01, duplicates = T,
                          outliers = T)

#plot cleaning results
plot(flags, clean = F, pts.size = 2)

#exclude flagged records
clean <- filter(dat, flags$summary == TRUE)
clean

?CleanCoordinates


#####################################################
# Species ranges




#####################################################
# Species richness


####################################################
# point to area classification






