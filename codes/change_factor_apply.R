###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

#### Applying change factor to future layers #####

####Read first:
#To create the future layers we used the change-factor approach,
#the same used by the Bio-ORACLE team. In this approach, coarse layers from
#CMIP are used to obtain min,max or mean from a reference condition
#(in our case 2000-2014) and a future condition (2091-2100)
#for each scenario of RCP. Then we get the absolute difference between the reference
#condition and the new one. All this procedure was done using CDO (Climate Data Operators).
#More details of this software (that is opensource) can be found on
#https://code.mpimet.mpg.de/projects/cdo/
#Then the change factor layer was interpolated to the same resolution of our climate layers
#using kriging interpolation in R (code is on the file interpolation.r)
#Now we will apply this change factor to our climate layers from Bio-ORACLE.
####

library(raster)
library(sdmpredictors)

scenarios <- c("rcp26","rcp45","rcp85")
period <- c("2100") #each period was done separately
#for the historical one use 'hist', in  that case the scenario is ignored

for (i in 1:length(scenarios)) {

  # Establish the scenario
  set <- scenarios[i]

  #Open files for each period
  if (period != "hist") {

    files <- list.files(paste0("data/env/change_factor/",period,"/",set))

    cfact <- stack(paste0("data/env/change_factor/",period,"/",set,"/",files))
    
    bath.new <- raster("data/env/bath_layers/bath_2_200.tif")
    
    cfact <- mask(cfact, bath.new)

  }

  if (period == "hist") {

    files <- list.files(paste0("data/env/change_factor/",period))

    cfact <- stack(paste0("data/env/change_factor/",period,"/",files))

  }

  #Stack current files on the same order
  env <- stack(paste0("data/env/crop_layers/",
                      c("BO_calcite.tif",
                        "BO21_chlomax_ss.tif",
                        "BO_cloudmax.tif",
                        "BO21_nitratemean_ss.tif",
                        "BO_ph.tif",
                        "BO21_phosphatemax_ss.tif",
                        "CO_windspeed.tif",
                        "BO21_silicatemean_ss.tif",
                        "BO21_salinitymean_ss.tif",
                        "BO21_tempmax_ss.tif")))

# Combine files. Verify during the process if names are being binded correctly
  for (z in 1:length(names(cfact))) {
    cat("Combining", names(cfact[[z]]), "and", names(env[[z]]), "\n")
          
          #For this strategy, used for phosphate, chl-a, salinity and nitrate
          #we apply the relative change (future-current/current) to avoid
          #abnormal values
          
          if (names(env[[z]]) == "BO21_phosphatemax_ss" |
              names(env[[z]]) == "BO21_chlomax_ss" |
              names(env[[z]]) == "BO21_salinitymean_ss" |
              names(env[[z]]) == "BO21_nitratemean_ss") {
                  
                  cat("Combination using relative change", "\n")
                  
                  rc <- cfact[[z]]
                  ro <- env[[z]]
                  
                  relc <- rc * ro
                  
                  rf <- ro + relc
                  
                  if (cellStats(rf, min) < 0) {
                    
                    rf <-
                      calc(
                        rf,
                        fun = function(x) {
                          x[x < 0] <- 0
                          return(x)
                        }
                      )
                    
                  }
                  
                  names(rf) <- names(ro)
                  
          }
          
          #For the other layers is preferred to apply the absolute difference
          #(future-current) and this value summed to the current after
          #interpolation
          
          else {
                  
                  cat("Combination using absolute difference", "\n")
                  
                  rc <- cfact[[z]]
                  ro <- env[[z]]
                  rf <- ro+rc
                  names(rf) <- names(ro)   
                  
          }
          
    

# Save final rasters
    if (period == "2100" | period == "2050") {

      if (dir.exists(
              paste0("data/env/proj_layers/",set,"/",period,"/")) == F) {
        dir.create(
                paste0("data/env/proj_layers/",set,"/",period,"/"),
                recursive = T)
      }

      writeRaster(rf,
              paste0("data/env/proj_layers/",set,"/",period,"/",names(rf),".tif"),
              overwrite = T)



    }

    if (period == "hist") {

      if (dir.exists(paste0("data/env/proj_layers/",period,"/")) == F) {
        dir.create(paste0("data/env/proj_layers/",period,"/"), recursive = T)
      }

      writeRaster(rf,
              paste0("data/env/proj_layers/",period,"/",names(rf),".tif"))

    }

  }


  if (period == "2100" | period == "2050") {

    #Crop to extent/bathymetry
    curve <- load_layers(
      layercodes = c(paste0("BO21_",toupper(set),"_",period,"_curvelmean_ss")),
      equalarea = FALSE,
      rasterstack = TRUE,
      datadir = "data/env/env_layers"
    )

    names(curve) <- "BO21_curvelmean_ss"

    curve <- crop(curve, extent(-99, 22, -42.5, 42.5))

    curve <- mask(curve, rf)

    writeRaster(curve,
            paste0("data/env/proj_layers/",set,"/",period,"/",names(curve),".tif"),
            overwrite = T)



  }

  if (period == "hist") {


    #Crop to extent/bathymetry
    curve <- load_layers(
      layercodes = "BO2_curvelmean_ss",
      equalarea = FALSE,
      rasterstack = TRUE,
      datadir = "data/env/env_layers"
    )

    names(curve) <- "BO2_curvelmean_ss"

    curve <- crop(curve, extent(-99, -29, -42.5, 42.5))

    curve <- mask(curve, rf)

    writeRaster(curve,
            paste0("data/env/proj_layers/",period,"/",names(curve),".tif"))

  }

}



##END##
