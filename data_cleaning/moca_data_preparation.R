library(dplyr)
library(ggplot2)
library(obistools)

obis <- read.csv("data_cleaning/moca_obis.csv")
colnames(obis) <- c("decimalLongitude", "decimalLatitude", "moca")
obis$code <- paste("OBIS",
                   seq(1:length(obis$moca)), sep = "")

gbif <- read.csv("data_cleaning/moca_gbif.csv")
colnames(gbif) <- c("decimalLongitude", "decimalLatitude", "moca")
gbif$code <- paste("GBIF",
                   seq(1:length(gbif$moca)), sep = "")

aued <- read.csv("data_cleaning/moca_aued.csv", sep = ";")
aued$code <- paste("AUED",
                   seq(1:length(aued$moca)), sep = "")


biblio <- read.csv("data_cleaning/moca_bibliografia.csv", sep = ";")
biblio$moca <- 1
biblio$code <- paste("BIBL",seq(1:length(biblio$moca))
                     , sep = "")

#Data gathered from Maia et al. 2018 (Marine Ecology, doi:10.1111/maec.12520)
#to complement data from Africa coast.
maiaetal <- read.csv("data_cleaning/maiaetal_moca_side.csv")
maiaetal <- maiaetal[maiaetal$species == "moca",1:2]
maiaetal$moca <- 1
maiaetal$code <- paste("MAIA",seq(1:nrow(maiaetal))
                       , sep = "")


sp <- bind_rows(obis, gbif, aued, biblio, maiaetal)


sp.p <- sp[sp$moca == 1,]

sp.a <- sp[sp$moca == 0,]

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


sp.total <- bind_rows(sp.p,sp.a)

dups <- duplicated(sp.total[, c('decimalLongitude', 'decimalLatitude')])
sum(dups)

sp.sel <- sp.total[!dups,]

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



sp.land <-
  check_onland(sp.sel, buffer = 0) #It's possible to add a buffer (in m)


#plot all points to see
sp.plot +  geom_point(
  data = sp.land,
  aes(x = decimalLongitude, y = decimalLatitude),
  colour = "blue",
  size = 0.8
)
#ggsave("muhi_onland.png",last_plot())


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


sp.final <- rbind(sp.final, sp.complement)

write.csv(sp.sel, "data_cleaning/moca_total.csv", row.names = F)
write.csv(sp.final, "data/moca/moca_final.csv", row.names = F)
