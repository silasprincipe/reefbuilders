#####Download of occurrence points from OBIS
###Silas C. Principe, september 2019
###silasprincipe@yahoo.com.br
###Some code adapted from a code from
###Muhammed A Oyinlola / m.oyinlola@oceans.ubc.ca

#####ATENTION: rememer to change the name of the species
##### in all the code!

#Load packages
library(robis)
library(obistools)
library(dplyr)
library(ggplot2)

#Name of species to be downloaded
###If you want to download more species, just add them
###concatenated >> c("species1", "species2"),
###However, you will need to change other parts of the code
species <- "Siderastrea"
#Dowload species data (can take some time)
sp <- occurrence(species)
#Verify if data was corrected dowloaded
head(sp[5:13])

#Choose period of data
year.s <- c(1950:2020)
year.s <- as.character(as.integer(year.s))

#Choose kind of records
record <- c("HumanObservation", "Occurrence", "PreservedSpecimen")

#Filter data
sp$year[is.na(sp$year)] <- 0
sp.sel <- sp %>%
  filter(year %in% year.s) %>%
  filter(basisOfRecord %in% record)
#Check fields
errors <- check_fields(sp.sel)
#Show existing errors
ex.errors <- unique(errors$message)
ex.errors

####This is optional
#You have to decide if any of those errors are problematic or not
#e.date <- errors$row[errors$message == ex.errors[1]]
#e.date.data <- sp[e.date, ]
#e.date.data
#if you need to remove, just use the anti_join of dplyr
####

#Crop data for the selected area
lat1 <- -42.5
lat2 <- 42.5
long1 <- -99
long2 <- 22
sp.sel <- sp.sel %>%
  filter(decimalLatitude >= lat1 & decimalLatitude <= lat2) %>%
  filter(decimalLongitude >= long1 & decimalLongitude <= long2)

#Plot data points to verify
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

sp.sel$flags <- NA
sp.land$flags <- NA

#Remove data on land
sp.sea <- sp.sel %>% anti_join(sp.land)
plot_map_leaflet(sp.sea)

###Plots for each species to verify
plot_map_leaflet(sp.sea[sp.sea$scientificName == "Siderastrea stellata" |
                          sp.sea$scientificName == "Siderastrea radians",])

#We remove S. siderea in the final data cleaning.
plot_map_leaflet(sp.sea[sp.sea$scientificName == "Siderastrea siderea",])


#### To remove data from institutions that are not reliable
#Using bibliographic citation
sources <- as.factor(sp.sea$bibliographicCitation)
summary(sources)
#Using institution code
inst.code <- as.factor(sp.sea$institutionCode)
summary(inst.code)
#Remove data that don't have institutions information
sp.clean <- sp.sea %>%
  #filter(!is.na(bibliographicCitation)) %>%
  filter(!is.na(institutionCode))
#Verify again
sources <- as.factor(sp.clean$bibliographicCitation)
summary(sources)
inst.code <- as.factor(sp.clean$institutionCode)
summary(inst.code)


#### Optional
#If you need to remove data from a certain institution
inst.code <- unique(sp.clean$institutionCode)
inst.code
sp.clean <- sp.clean %>%
  filter(institutionCode != "Diveboard")
unique(sp.clean$institutionCode)
#the same can be done with bibliographic citation.
######

#### Generate final data sets
#Full data
write.csv(sp.clean, file = "data_cleaning/side_obis_complete.csv", 
          row.names = FALSE)

#List of occurence points with presence/absence info and species name
sp.points.pa <- sp.clean %>% select(scientificName, 
                                    absence,
                                    decimalLongitude, 
                                    decimalLatitude)
sp.points.pa$absence <- as.character(sp.points.pa$absence)
sp.points.pa[sp.points.pa$absence == "FALSE", 2] <- 1
sp.points.pa[sp.points.pa$absence == "TRUE", 2] <- 0
colnames(sp.points.pa)[2] <- "pa"
write.csv(sp.points.pa, file = "data_cleaning/side_obis_pa.csv",
          row.names = FALSE)


#Generates the final list of occurence points ready for BIOMOD2
sp.points <- sp.points.pa %>% select(decimalLongitude,
                                     decimalLatitude,
                                     pa)
colnames(sp.points)[3] <- "Siderastrea"
write.csv(sp.points, file = "data_cleaning/side_obis.csv", 
          row.names = FALSE)

###At the end you have three files:
#species_name_complete.csv >>>>> complete dataset with all information
#species_name_pa.csv >>>>> dataset with name, presence/absence and location
#species_name.csv >>>> dataset with long/lat and presence/abs for BIOMOD2
