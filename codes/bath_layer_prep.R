###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br
#Collaboration: André L. Acosta

#### Bathymetry Layer preparation #####

### Important Note: to run this code you first need to download the
# GEBCO (General Bathymetric Chart of the Oceans) from www.gebco.net
# and place it in the data/env/bath_layers/ folder.
# Also note that you don't need to run it, as we already provide
# the croped layer.

# Load libraries
library(raster)

# Define study extent
ext <- extent(-99, 22, -42.5, 42.5)

# Load GEBCO file
# When you download the GEBCO file, remember to rename it
bath <- raster("data/env/bath_layers/gebco_america_africa.tif")

# Load shapefile
sel.area <- shapefile("data/env/crop_shape.shp")

# Crop for the extension
bath.crop <- crop(bath, ext)

# Plot to verify
plot(bath.crop)

# Generate files for different depths ----
bath.2_100 <-
  calc(
    bath.crop,
    fun = function(x) {
      x[x > 2 | x < -100] <- NA
      return(x)
    }
  )

bath.2_150 <-
  calc(
    bath.crop,
    fun = function(x) {
      x[x > 2 | x < -150] <- NA
      return(x)
    }
  )

bath.2_200 <-
  calc(
    bath.crop,
    fun = function(x) {
      x[x > 2 | x < -200] <- NA
      return(x)
    }
  )



#Adjust resolution
bath.2_100 <- aggregate(bath.2_100, fact = 20, fun = mean)
bath.2_150 <- aggregate(bath.2_150, fact = 20, fun = mean)
bath.2_200 <- aggregate(bath.2_200, fact = 20, fun = mean)

#Mask in the selected area
m2_100 <- mask(bath.2_100, sel.area)
m2_150 <- mask(bath.2_150, sel.area)
m2_200 <- mask(bath.2_200, sel.area)

#Plot to verify
plot(m2_100)
plot(m2_150)
plot(m2_200)


#Write final rasters ----

writeRaster(m2_100, filename = "data/env/bath_layers/bath_2_100.tif", overwrite = T)

writeRaster(m2_150, filename = "data/env/bath_layers/bath_2_150.tif", overwrite = T)

writeRaster(m2_200, filename = "data/env/bath_layers/bath_2_200.tif", overwrite = T)


###END
