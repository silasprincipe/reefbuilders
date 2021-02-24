###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br
#Collaboration: André L. Acosta

### Wind Layer preparation - Copernicus / Mean over 1 year ###
# Wind layer was obtained from the Copernicus
# website (http://marine.copernicus.eu/)

# Open required libraries ----
library(sp)
library(rgdal)
library(ncdf4)
library(raster)
library(sdmpredictors)
library(gstat)
library(automap)

# Set path and filename ----
ncpath <- "data/env/env_layers/"
ncname <- "CERSAT-GLO-REP_WIND_L4-OBS_FULL_TIME_SERIE_1608661988812"
ncfname <- paste(ncpath, ncname, ".nc", sep = "")
dname <- "wind_speed"  #name of the variable that we will extract

pb <- txtProgressBar(
        min = 0,
        max = 12,
        initial = 0,
        style = 3
)

# Convert each band (month) into a raster and then stack ----
for (i in 1:12) {
        
        r <- raster(ncfname, varname = dname, band = i)
        names(r) <- paste("month_", i, sep = "")
        
        if (i == 1) {
                wind.st <- stack(r)
        }
        if (i != 1) {
                wind.st <- stack(wind.st, r)
        }
        setTxtProgressBar(pb, i)
}

# Verify produced rasters
wind.st

plot(wind.st$month_1)

# Extract the mean, sd, and meadian of values
wind.mean <- calc(wind.st, mean)
wind.sd <- calc(wind.st, sd)
wind.median <- calc(wind.st, median)

# We will use just the mean
plot(wind.mean)

# Save a copy
writeRaster(wind.mean,
            "data/env/env_layers/windspeed_mean_copernicus.tif",
            overwrite = T)

# Shapefile of the selected area
sel.area <- shapefile("data/env/crop_shape.shp")

# Load bathymetry layer (prepared with bath_layer_prep.R)
bath <- raster("data/env/bath_layers/bath_2_200.tif")

#Extent of study
ext <- extent(-99,22,-42.5, 42.5)

#Crop to the extent
wind.c <- crop(wind.mean, ext)

bath.ag <- aggregate(bath, fact = 3)

#Crop to the extent
wind.c <- mask(wind.c, bath.ag)
wind.c <- mask(wind.c, sel.area)


# Interpolation to fit BIO-Oracle resolution ----
tmp <-
        data.frame(cbind(xyFromCell(wind.c, 1:length(wind.c[[1]])),
                         values(wind.c[[1]])))

#change colnames
colnames(tmp) <- c('lon', 'lat', 'var')

#Remove NAs
tmp <- tmp[!is.na(tmp$var), ]

#Convert to SpatialGrid
coordinates(tmp) <- ~ lon + lat

#Correct CRS
crs(tmp) <- crs(bath)


#Create variograms
#Use automap to fit the variogram automatically
vario <-
        automap::autofitVariogram(formula = var ~ 1, input_data = tmp)
m <- vario$var_model

#If wanted, variogram can be fitted mannualy
#m <- fit.variogram(v, vgm(1, "Sph", 500, 1))

#Create the kriging object, using 12 nearest cells
gform <- gstat(NULL, "var", var ~ 1, tmp, model = m, nmax = 12)

#Interpolate using the basemap as model
imap <- interpolate(bath, gform)

#Mask with bath layer
wind.f <- mask(imap, bath)
wind.f <- mask(wind.f, sel.area)

#Plot to verify
plot(wind.f)

#Correct name to fit the same standard of Bio-ORACLE
names(wind.f) <- "CO_windspeed"

#Write the final raster layer
writeRaster(wind.f,
            "data/env/env_layers/windspeed_cop_ed.tif",
            overwrite = T)
##END
