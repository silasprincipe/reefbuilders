###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

#### Function to calculate change in area from current to future #####
areaChange <- function(sp, round.code) {
  
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
  rcp26[rcp26 >= thres] <- 1
  
  rcp45[rcp45 < thres] <- 0
  rcp45[rcp45 >= thres] <- 1
  
  rcp85[rcp85 < thres] <- 0
  rcp85[rcp85 >= thres] <- 1
  
  #Calculate number of cells with presence
  pcurr <- length(curr[curr == 1])
  prcp26 <- length(rcp26[rcp26 == 1])
  prcp45 <- length(rcp45[rcp45 == 1])
  prcp85 <- length(rcp85[rcp85 == 1])
  
  #Create a raster with cell area
  cell.area <- area(curr, na.rm = T)
  
  #Calculate the area of presence
  acurr <- sum(cell.area[curr == 1])
  arcp26 <- sum(cell.area[rcp26 == 1])
  arcp45 <- sum(cell.area[rcp45 == 1])
  arcp85 <- sum(cell.area[rcp85 == 1])
  
  #Create a data-frame with the data
  sp.area <- data.frame(
    "current" = c(pcurr, acurr),
    "rcp26" = c(prcp26, arcp26),
    "rcp45" = c(prcp45, arcp45),
    "rcp85" = c(prcp85, arcp85)
  )
  
  # Now to get for each region defined in the article
  for (i in 1:3) {
    
    # Load Marine Ecoregions of the World dataset
    meow <- shapefile("gis/meow_edited.shp")
    
    if (i == 1) {
      
      # Apply the filter
      pres.meow <- c("Magellanic", 
                     "Warm Temperate Southwestern Atlantic",
                     "Tropical Southwestern Atlantic", 
                     "North Brazil Shelf")
    }
    
    
    if (i == 2) {
      
      # Apply the filter
      pres.meow <- c("Tropical Northwestern Atlantic",
                     "Warm Temperate Northwest Atlantic",
                     "Cold Temperate Northwest Atlantic")
    }
    
    if (i == 3) {
      
      # Apply the filter
      pres.meow <- c("Tropical Northwestern Atlantic",
                     "Warm Temperate Northwest Atlantic",
                     "Cold Temperate Northwest Atlantic",
                     "Magellanic", 
                     "Warm Temperate Southwestern Atlantic",
                     "Tropical Southwestern Atlantic", 
                     "North Brazil Shelf")
      
      pres.meow <- meow$PROVINCE[!meow$PROVINCE %in% pres.meow]
      
    }
    
    shape <- meow[meow$PROVINCE %in% pres.meow,]
    
    crs(shape) <- crs(curr)
    
    curr.b <- mask(curr, shape)
    rcp26.b <- mask(rcp26, shape)
    rcp45.b <- mask(rcp45, shape)
    rcp85.b <- mask(rcp85, shape)
    
    
    pcurr <- length(curr.b[curr.b == 1])
    prcp26 <- length(rcp26.b[rcp26.b == 1])
    prcp45 <- length(rcp45.b[rcp45.b == 1])
    prcp85 <- length(rcp85.b[rcp85.b == 1])
    
    
    cell.area <- area(curr, na.rm = T)
    
    acurr <- sum(cell.area[curr.b == 1])
    arcp26 <- sum(cell.area[rcp26.b == 1])
    arcp45 <- sum(cell.area[rcp45.b == 1])
    arcp85 <- sum(cell.area[rcp85.b == 1])
    
    sp.area.cut <- data.frame(
      "current" = c(pcurr, acurr),
      "rcp26" = c(prcp26, arcp26),
      "rcp45" = c(prcp45, arcp45),
      "rcp85" = c(prcp85, arcp85)
    )
    
    sp.area <- rbind(sp.area, sp.area.cut)
    
    
  }
  
  #Return results
  return(sp.area)
  
}
