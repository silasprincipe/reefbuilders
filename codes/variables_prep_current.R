###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br
#Collaboration: André L. Acosta

#### Environmental layers #####

### Note: Wind Layer have to be prepared previously using wind_layer_prep.R
### Here in this data package, the edited wind layer is already provided.

# Layers preparing ----

# Load libraries
library(sdmpredictors)
library(dplyr)
library(raster)
library(sp)
library(usdm)

# Extent of study

ext <- extent(-99, 22, -42.5, 42.5)

# Shapefile of the selected area

sel.area <- shapefile("data/env/crop_shape.shp")

# Load bathymetry layer

bath <- raster("data/env/bath_layers/bath_2_200.tif")

# Load layer codes

codes <- read.table("data/env/layercodes.txt")

# Load environmental layers

layer.codes <- as.vector(codes$V1)

env <- load_layers(
  layercodes = layer.codes ,
  equalarea = FALSE,
  rasterstack = FALSE,
  datadir = "data/env/env_layers"
)

# Crop environmental layers to study extent

for (i in 1:length(env)) {
  env[[i]] <- crop(env[[i]], ext)
}

env <- stack(env)

# Mask layers to bathymetry and to selected area
env.f <- mask(env, bath)
env.f <- mask(env.f, sel.area)

plot(env.f$BO21_chlomax_ss)
plot(bath)

#Load wind speed file (copernicus)
### This file was prepared using another code (wind_layer_prep.r)
### and was based on Copernicus file edited from a netcdf
wind <- raster("data/env/env_layers/windspeed_cop_ed.tif")
names(wind) <- "CO_windspeed"

wind <- mask(wind, env.f$BO21_tempmax_ss)

# Stack layers
env <- stack(env.f, wind)
names(env)

# Colinearity verification ----
vifstep.env <- vifstep(env, th = 10)

vifstep.env

#Exclude based on vifstep
env.2 <- exclude(env, vifstep.env)

names(env.2)

### Now exclude variations of the same variable based on the ones
### that have stronger biological connection
#we also exclude damax and parmax/parmean - see explanation in the article

env.3 <- dropLayer(env.2,
                   c(
                     "BO21_chlomin_ss",
                     "BO_cloudmean",
                     "BO_damax",
                     "BO_parmax",
                     "BO_parmean",
                     "BO21_phosphatemin_ss"
                   ))

names(env.3)


#We can make another vifstep verification
vifstep.env3 <- vifstep(env.3, th = 10)
vifstep.env3

#Write final list of layers
write.table(names(env.3), "data/env/env_layers.txt", col.names = F)

#Save Vifstep outputs
capture.output(vifstep.env, file = "data/env/vifstep_result.txt")
capture.output(vifstep.env@corMatrix, file = "data/env/vifstep_matrix.txt")
capture.output(vifstep.env3, file = "data/env/vif_result_afterexcluding.txt")


##### Separate rasters
names(env)

if (dir.exists("data/env/crop_layers") == F) {
  dir.create("data/env/crop_layers", recursive = T)
}

writeRaster(
  env,
  filename = paste("data/env/crop_layers/", names(env), sep = ""),
  format = "GTiff",
  bylayer = TRUE,
  overwrite = T
)

write.table(names(env), "data/env/layers_names_full.txt", col.names = F)

#### END
