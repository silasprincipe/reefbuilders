library(dplyr)
library(ggplot2)
library(obistools)

obis <- read.csv("data_cleaning/muhi_obis.csv")
obis$code <- paste("OBIS",
                   seq(1:length(obis$Mussismilia_hispida)), sep = "")

gbif <- read.csv("data_cleaning/muhi_gbif.csv")
gbif$code <- paste("GBIF",
                   seq(1:length(gbif$Mussismilia_hispida)), sep = "")

aued <- read.csv("data_cleaning/muhi_aued.csv", sep = ";")
aued$code <- paste("AUED",
                   seq(1:length(aued$Mussismilia_hispida)), sep = "")
aued <- aued[,2:5]

aquamaps <- read.csv("data_cleaning/muhi_aquamaps_peluso.csv")
aquamaps$code <- 1
aquamaps$code[1:15] <- paste("PELU",seq(1:15)
                             , sep = "")
aquamaps$code[16:28] <- paste("AQUA",seq(1:13)
                              , sep = "")
aquamaps$species <- 1
colnames(aquamaps) <- c("Mussismilia_hispida",
                        "decimalLongitude",
                        "decimalLatitude",
                        "code")

aquamaps <- aquamaps[1:15,] #we remove data from aquamaps because we decided to not use it anymore


biblio <- read.csv("data_cleaning/muhi_bibliografia.csv", sep = ";")
biblio <- biblio[1:155,]
biblio$Mussismilia_hispida <- 1
biblio$code <- paste("BIBL",seq(1:155)
                     , sep = "")
biblio <- biblio[,4:7]



sp <- bind_rows(obis, gbif, aued, aquamaps, biblio)
rm(obis, gbif, aued, aquamaps, biblio)



dups <- duplicated(sp[, c('decimalLongitude', 'decimalLatitude')])
# number of duplicates
sum(dups)
# keep the records that are _not_ duplicated
sp.sel <- sp[!dups, ]





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
#ggsave("muhi_total.png",sp.plot)


sp.land <-
  check_onland(sp.sel, buffer = 0) #It's possible to add a buffer (in m)





### Correct data on land using GIS (QGIS - external)
# nearest on-water point (only for bibliographic points)

sp.cor <- read.csv("data_cleaning/muhi_land_corrected.csv")
colnames(sp.cor) <- c("code", "decimalLongitude", "decimalLatitude")

sp.sel.cor <- left_join(sp.sel, sp.cor, by = "code") %>% 
  mutate(decimalLongitude = ifelse(is.na(decimalLongitude.y), 
                                   decimalLongitude.x,
                                   decimalLongitude.y)) %>% 
  mutate(decimalLatitude = ifelse(is.na(decimalLatitude.y), 
                                  decimalLatitude.x,
                                  decimalLatitude.y))

sp.sel <- sp.sel.cor %>% dplyr::select(decimalLongitude, decimalLatitude, Mussismilia_hispida, code)

### check on land again

sp.land.cor <- check_onland(sp.sel, buffer = 0)


#plot all points to see
sp.plot +  geom_point(
  data = sp.land.cor,
  aes(x = decimalLongitude, y = decimalLatitude),
  colour = "blue",
  size = 0.8
)
#ggsave("muhi_onland.png",last_plot())


#Remove data on land
sp.sea <- sp.sel %>% anti_join(sp.land)
plot_map_leaflet(sp.sea)

#Remove registro do Panamá
sp.final <- sp.sea[-c(86),]
plot_map_leaflet(sp.final)



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
#ggsave("muhi_final.png",sp.plot)


write.csv(sp.sel, "data_cleaning/muhi_total.csv", row.names = F)
write.csv(sp.final, "data/muhi/muhi_final.csv", row.names = F)
