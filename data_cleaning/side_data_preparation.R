library(dplyr)
library(ggplot2)
library(obistools)


# Prepare data for side (Siderastrea stellata and Siderastrea radians - Siderastrea complex)

obis <- read.csv("data_cleaning/side_obis_complete.csv")

obis <- obis[obis$scientificName == "Siderastrea stellata" |
                    obis$scientificName == "Siderastrea radians" ,c("decimalLongitude","decimalLatitude")]
obis$side <- 1
obis$code <- paste0("OBIS",
                         seq(1:length(obis$side)))

###We desconsidered GBIF data as it was not available for any of the complex species.

gbif <- read.csv("data_cleaning/side_gbif_complete.csv")

gbif <- gbif[gbif$species == "Siderastrea stellata" |
                    gbif$species == "Siderastrea radians" ,c("decimalLongitude","decimalLatitude")]

gbif$side <- 1
gbif$code <- paste0("GBIF",
                         seq(1:length(gbif$decimalLongitude)))

aued <- read.csv("data_cleaning/side_aued.csv", sep = ";")
colnames(aued) <- c(colnames(aued[,1:2]),"side")
aued$code <- paste("AUED",
                   seq(1:length(aued$side)), sep = "")


biblio <- read.csv("data_cleaning/side_bibliografia.csv", sep = ";")

biblio <- biblio[biblio$species == "Siderastrea stellata" |
                   biblio$species == "Siderastrea radians",]

biblio <- biblio[,3:4]
biblio$side <- 1
biblio$code <- paste("BIBL",seq(1:length(biblio$Siderastrea))
                     , sep = "")


#Data gathered from Maia et al. 2018 (Marine Ecology, doi:10.1111/maec.12520)
#to complement data from Africa coast.
maiaetal <- read.csv("data_cleaning/maiaetal_moca_side.csv")
maiaetal <- maiaetal[maiaetal$species == "side",1:2]
maiaetal$side <- 1
maiaetal$code <- paste("MAIA",seq(1:nrow(maiaetal))
                       , sep = "")



sp <- bind_rows(obis, gbif, aued, biblio, maiaetal)


sp.p <- sp[sp$side == 1,]

sp.a <- sp[sp$side == 0,]

dups <- duplicated(sp.p[, c('decimalLongitude', 'decimalLatitude')])
# number of duplicates
sum(dups)
# keep the records that are _not_ duplicated
sp.p <- sp.p[!dups, ]

dups <- duplicated(sp.a[, c('decimalLongitude', 'decimalLatitude')])
# number of duplicates
sum(dups)
# keep the records that are _not_ duplicated
sp.a <- sp.a[!dups, ]


sp.total <- bind_rows(sp.p, sp.a)

dups <- duplicated(sp.total[, c('decimalLongitude', 'decimalLatitude')])
sum(dups)

sp.total <- sp.total[!dups,]

dups <- duplicated(sp.total[, c('decimalLongitude', 'decimalLatitude')])
sum(dups)

sp.sel <- sp.total

#Plot data points to verify
sp.points <- sp.sel %>% dplyr::select(decimalLongitude, decimalLatitude)
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


#We check again for data on land because of bibliographic data
sp.land <-
  check_onland(sp.sel, buffer = 0) #It's possible to add a buffer (in m)


#Remove data on land
sp.sea <- sp.sel %>% anti_join(sp.land)
plot_map_leaflet(sp.sea)



#Crop data for the selected area
lat1 <- -42.5
lat2 <- 42.5
long1 <- -99
long2 <- 22
sp.final <- sp.sea %>%
  filter(decimalLatitude >= lat1 & decimalLatitude <= lat2) %>%
  filter(decimalLongitude >= long1 & decimalLongitude <= long2)



#Plot data points to verify
sp.points <- sp.final %>% dplyr::select(decimalLongitude, decimalLatitude)
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



write.csv(sp.sel, "data_cleaning/side_total.csv", row.names = F)
write.csv(sp.final, "data/side/side_final.csv", row.names = F)

###END