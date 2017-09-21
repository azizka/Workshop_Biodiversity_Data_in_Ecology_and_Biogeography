##################################################################################
# Speciesgeocoder - data cleaning, visualization and classification
##################################################################################

#setup
require(speciesgeocodeR)
require(tidyverse)
require(countrycode)
require(raster)


#Load example data
dat <- read_csv("Example_data/bombacoideae_occurrences_gbif.csv")
names(dat) #a lot of columns

#Visualize
wm <- borders("world", colour="gray50", fill="gray50")
ggplot() +
  coord_fixed()+
  wm +
  geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)



#Data cleaning using speciesgeocoder
##convert country code
dat$countryCode <-  countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')

##flag problems
flags <- CleanCoordinates(x = dat[,c("decimalLongitude", "decimalLatitude")], countries = dat$countryCode, 
                          species = dat$species, countrycheck = T, outliers = T, seas = F)

##Exclude problematic records
dat.cl <- dat[flags$summary,]


#Automated testing for coordinate imprecisions
##Create dummy variable
dat.cl$datasettotal <- "TOTAL"

t.all <- dat.cl%>%
  dplyr::select(dataset = datasettotal, 
         decimallongitude = decimalLongitude, 
         decimallatitude = decimalLatitude)

##Run dataset level test
outp <- CleanCoordinatesDS(t.all)
outp


#Species richness patterns
dat.cl <- read_csv("Example_data/bombacoideae_occurrences_gbif_clean.csv")
#create the rasters
spnum <- RichnessGrid(dat.cl, res = 1, type = 'spnum')%>% #change type to "abu" for the number of occurrences
  rasterToPoints()%>%
  as_data_frame()

#plot
wm <- borders("world", colour="gray90", fill="gray90")
ggplot() +
  coord_fixed()+
  wm +
  geom_raster(data = spnum, aes(x = x, y = y, fill = layer))+
  scale_fill_gradient2(low= "grey", mid = "green", high = "red")+
  theme_bw()


#Species ranges
#identify widespread species

inp <- split(dat.cl, f = dat.cl$species)
test <- lapply(inp, function(k){SpatialPoints(k[,2:3])})
test <- lapply(test, "extent")
test <- lapply(test, function(k){(k@xmax + 180) - (k@xmin +180)})
test <- unlist(lapply(test, function(k){k >= 180}))
test <- test[test]

#exclude widespread cultivated species
dat.cl <- filter(dat.cl, !species %in% names(test))

#calculate ranges
rang.pol <- CalcRange(dat.cl, terrestrial = T)
plot(rang.pol)


#Range based richnes pattern
##Create richness map
rang.r <- RangeRichness(rang.pol, res = 1)%>%
  rasterToPoints()%>%
  as_data_frame()%>%
  filter(layer != 0)

##plot
wm <- borders("world", colour="gray90", fill="gray90")
ggplot() +
  coord_fixed()+
  wm +
  geom_raster(data = rang.r, aes(x = x, y = y, fill = layer))+
  scale_fill_gradient2(low= "green", mid = "yellow", high = "red", midpoint = 7)+
  theme_bw()


##################################################################################
# Infomap Bioregions - taxon-specific bioregions
##################################################################################
#Taxon-specific bioregionalization using the webpage
##Using occurrences
##Using ranges

##################################################################################
# SampBias - biasing effect of anthorpogenic structures
##################################################################################
#setup the package
# install_github(repo = "azizka/sampbias")
library(sampbias)

#Load example data
occ <- read.csv("Example_data/borneo_mammals.csv", sep = "\t")

#run sampbias with default options
bias.out <- SamplingBias(x = occ, res = 0.1, biasdist = c(0,30000, 100000))

#Visualize results
par (ask = T)
plot(bias.out)
