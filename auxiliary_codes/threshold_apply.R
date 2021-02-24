###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

# Apply the threshold to ensembles to save a tif file or use

species <- c("muhi", "moca", "side")

round.code <- "rbf1"

for (i in 1:3) {
        
        sp <- species[i]
        
        #Load libraries
        library(raster)
        library(dplyr)
        
        #Load current raster
        curr <- raster(
                paste0(
                        sp,
                        "/proj_current_",
                        sp,
                        "_",
                        round.code,
                        "/individual_projections/",
                        sp,
                        "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"
                )
        )
        
        #Load future raster (each scenario)
        rcp26 <-
                raster(
                        paste0(
                                sp,
                                "/proj_",
                                "rcp26",
                                "_",
                                sp,
                                "_",
                                round.code,
                                "/individual_projections/",
                                sp,
                                "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"
                        )
                )
        
        rcp45 <-
                raster(
                        paste0(
                                sp,
                                "/proj_",
                                "rcp45",
                                "_",
                                sp,
                                "_",
                                round.code,
                                "/individual_projections/",
                                sp,
                                "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"
                        )
                )
        
        rcp85 <-
                raster(
                        paste0(
                                sp,
                                "/proj_",
                                "rcp85",
                                "_",
                                sp,
                                "_",
                                round.code,
                                "/individual_projections/",
                                sp,
                                "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"
                        )
                )
        
        
        #Select the threshold
        thres <- 750
        
        #Apply threshold for ech raster
        curr[curr < thres] <- 0
        curr[curr >= thres] <- 1
        
        rcp26[rcp26 < thres] <- 0
        rcp26[rcp26 >= thres] <- 2
        
        rcp45[rcp45 < thres] <- 0
        rcp45[rcp45 >= thres] <- 2
        
        rcp85[rcp85 < thres] <- 0
        rcp85[rcp85 >= thres] <- 2
        
        rcp26 <- rcp26+curr
        rcp45 <- rcp45+curr
        rcp85 <- rcp85+curr
        
        writeRaster(curr, paste0(sp, "_current.tif"))
        writeRaster(rcp26, paste0(sp, "_rcp26.tif"))
        writeRaster(rcp45, paste0(sp, "_rcp45.tif"))
        writeRaster(rcp85, paste0(sp, "_rcp85.tif"))
}
