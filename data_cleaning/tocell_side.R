#### Convert to 1 point per cell

library(raster)
library(dplyr)
library(maptools)
data("wrld_simpl")

#### Convert to 1 point per cell

#Load environmental layers
source("functions/Varload.r")
env <- Varload(folder = "crop_layers/", layers = "env_layers.txt",
               bath = "2_100")

bath <- env$bath_2_100

env <- mask(env, bath)

#Read species data
sp <- read.csv("data/side/side_final.csv")

env.f <- mean(env)

# #Remove points out of the area/bathymetry
# sp2 <- sp
# coordinates(sp2) <- ~ decimalLongitude + decimalLatitude
# crs(sp2) <- crs(env.f)
# out.p <- raster::extract(env.f, sp2)
# sp <- sp[!is.na(out.p),]
# rm(sp2, out.p)


out.p <- raster::extract(env.f, sp[,1:2])
sp <- sp[!is.na(out.p),]


# 1 point per cell
#Create a clean raster
r <- raster(nrow=1020, ncol=1452, xmn = -99, xmx= 22, ymn= -42.5, ymx = 42.5)
values(r) <- NA

#Put 0 in the cell that is 0 in data frame and 1 if 1 in data frame
#The priority is the presence, even if there is a 0 with a 1
r[cellFromXY(r, sp[sp$side==0,1:2])] <- 0
r[cellFromXY(r, sp[sp$side==1,1:2])] <- 1

#Final frequency of points
freq(r)

#Convert raster to dataframe
data <- as.data.frame(coordinates(r))
data$sp <- as.vector(values(r))

#Change column names
colnames(data) <- c("decimalLongitude", "decimalLatitude", "side")

#Remove NAs
sp <- filter(data, side == 1 | side == 0)

#Plot just to verify
plot(wrld_simpl, xlim = c(-99,22), ylim=c(-42.5,42.5))
points(sp[sp$side == 1,1:2], col = "blue", pch = 20, cex = 0.75)
points(sp[sp$side == 0,1:2], col = "red", pch = 20, cex = 0.75)

#Create a 'temp' object with the final results
# sp.total <- sp
# colnames(sp.total) <- c("decimalLongitude", "decimalLatitude", "Siderastrea")

write.csv(sp, "data/side/side_cell.csv", row.names = F)

######## END of conversion to 1 per cell
