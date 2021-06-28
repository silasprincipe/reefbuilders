#####Download of occurrence points from OBIS
###Silas C. Principe, september 2019
###silasprincipe@yahoo.com.br

#####ATENTION: rememer to change the name of the species
##### in all the code!

#Load packages
library(dplyr)
library(ggplot2)
library(rgbif)
library(sp)
library(countrycode)
library(CoordinateCleaner)
library(robis)
library(obistools)

#Name of species to be downloaded
species <- "Mussismilia hispida"
#Dowload species data (can take some time)
#obtain data from GBIF via rgbif
dat <- occ_search(scientificName = species,
                  return = "data",
                  hasCoordinate = T)
#Look if data was corrected downloaded
head(dat[, 1:5])


#select columns of interest
sp.sel <- dat %>%
  dplyr::select(
    species,
    decimalLongitude,
    decimalLatitude,
    countryCode,
    individualCount,
    gbifID,
    family,
    taxonRank,
    coordinateUncertaintyInMeters,
    year,
    basisOfRecord,
    institutionCode,
    datasetName
  )
# remove records without coordinates
sp.sel <- sp.sel %>%
  filter(!is.na(decimalLongitude)) %>%
  filter(!is.na(decimalLatitude))

#Crop data for the selected area
lat1 <- -42.5
lat2 <- 42.5
long1 <- -99
long2 <- 22
sp.sel <- sp.sel %>%
  filter(decimalLatitude >= lat1 & decimalLatitude <= lat2) %>%
  filter(decimalLongitude >= long1 & decimalLongitude <= long2)

#plot data to get an overview
wm <- borders("world", colour = "gray50", fill = "gray50")
sp.points <- sp.sel %>% select(decimalLongitude, decimalLatitude)
map.points <- data.frame(
  lo1 = min(sp.points$decimalLongitude),
  lo2 = max(sp.points$decimalLongitude),
  la1 = min(sp.points$decimalLatitude),
  la2 = max(sp.points$decimalLatitude)
)
wm <- borders("world", colour = "gray50", fill = "gray50")
sp.plot <- ggplot() + wm +
  geom_point(
    data = sp.points,
    aes(x = decimalLongitude, y = decimalLatitude),
    colour = "darkred",
    size = 0.5
  ) +
  #Delimits map limits
  coord_sf(
    xlim = c(map.points$lo1, map.points$lo2),
    ylim = c(map.points$la1, map.points$la2)
  ) +
  theme_bw()
sp.plot


#Verify data on land
sp.land <-
  check_onland(sp.sel, buffer = 0) #It's possible to add a buffer (in m)
#plot all points to see
sp.plot +  geom_point(
  data = sp.land,
  aes(x = decimalLongitude, y = decimalLatitude),
  colour = "blue",
  size = 0.8
)


#Remove data on land
sp.sea <- sp.sel %>% anti_join(sp.land)
plot_map_leaflet(sp.sea)



#convert country code from ISO2c to ISO3c
sp.sea$countryCode <-
  countrycode(sp.sea$countryCode,
              origin =  'iso2c',
              destination = 'iso3c')
####Flag problems with the data
#You can select different tests.
#Look for the help of clean_coordinates
sp.sea <- data.frame(sp.sea)
flags <-
  clean_coordinates(
    x = sp.sea,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    countries = "countryCode",
    species = "species",
    tests = c("equal", "gbif", "institutions",
              "zeros", "countries", "seas")
  )
#show summary
summary(flags)
head(flags)
### You have to decide if any of those errors are problematic
### Then you can remove those data. For now, we will just proceed.
### But, we will use other 'quality control' tools

#####Improving data
#hist(sp.sea$coordinateUncertaintyInMeters / 1000, breaks = 20)
sp.cl <- sp.sea #%>%
 # filter(coordinateUncertaintyInMeters / 1000 <= 100 |
  #         is.na(coordinateUncertaintyInMeters))
#Remove unsuitable data sources, especially fossils
#which are responsible for the majority of problems in this case
table(sp.cl$basisOfRecord)
sp.cl <- filter(
  sp.cl,
  basisOfRecord == "HUMAN_OBSERVATION" |
    basisOfRecord == "OBSERVATION" |
    basisOfRecord == "MACHINE_OBSERVATION" |
    basisOfRecord == "PRESERVED_SPECIMEN" 
)
#Individual count
table(sp.cl$individualCount)
sp.cl <- sp.cl %>%
  filter(individualCount > 0 | is.na(individualCount)) %>%
  filter(individualCount < 99 |
           is.na(individualCount)) # high counts are not a problem
#Age of records
table(sp.cl$year)
#Remove old
sp.cl <- sp.cl %>%
  filter(year >= 1950) # remove records from before second world war


### To remove data from institutions that are not reliable
#Using institution code
inst.code <- as.factor(sp.cl$institutionCode)
summary(inst.code)
#Remove data that don't have institutions information
sp.cl <- sp.cl %>%
  filter(!is.na(institutionCode))
#Verify again
inst.code <- as.factor(sp.cl$institutionCode)
summary(inst.code)


#### Optional
#If you need to remove data from a certain institution
inst.code <- unique(sp.cl$institutionCode)
inst.code
sp.cl <- sp.cl %>%
  filter(institutionCode != "Diveboard") %>%
  filter(institutionCode != "MW")

unique(sp.cl$institutionCode)
#In this case we removed all the data from Citizen Science
######

#Plot final dataset
plot_map_leaflet(sp.cl)

#### Generate final data sets
#Full data
write.csv(sp.cl, file = "data_cleaning/muhi_gbif_complete.csv",
          row.names = FALSE)


#Generates the final list of occurence points ready for BIOMOD2
sp.cl$pa <- rep(1, length.out = length(sp.cl$species))
sp.points <- sp.cl %>% select(decimalLongitude,
                              decimalLatitude,
                              pa)
colnames(sp.points)[3] <- "Mussismilia_hispida"
write.csv(sp.points, file = "data_cleaning/muhi_gbif.csv",
          row.names = FALSE)

###At the end you have two files:
#species_name_complete.csv >>>>> complete dataset with all information
#species_name.csv >>>> dataset with long/lat and presence/abs for BIOMOD2
