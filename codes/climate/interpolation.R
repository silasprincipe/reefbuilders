###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

#### NETCDF files (CMIP5) interpolation #####
#Fut layers preparing

library(raster)
library(automap)
library(gstat)
library(snowfall)
library(ncdf4)
library(dplyr)

#These have to be in the same order!
groups <- c('rcp26','rcp45','rcp85')
codes <- c(26, 45, 85)
###
period <- c('50', '100') #This code was prepared in a way that it's possible to produce
                         #maps for distinct periods.

period.s <- period[2] #For each period we run separately
#Before running another period, remember to move files from the folder
#to a new one as it will be saved in the same folder with the same name!

for (j in 1:length(groups)) {
  #Define the set that will be produced
  set <- groups[j]
  
  #Layer file name and dname may vary
  layers <- c("calc",
              "chl",
              "clt",
              "no3",
              "ph",
              "po4",
              "sfc",
              "si",
              "so",
              "tos")
  
  names <- c("calc",
             "chl",
             "clt",
             "no3",
             "ph",
             "po4",
             "sfcWind",
             "si",
             "so",
             "tos")
  
  cat("Converting ncdf files to raster. Group: ", set, " Code: ", codes[j], " Period: ",period.s, "\n")
  
  #Set a progress bar for the ncdf open
  pb <- txtProgressBar(
    min = 0,
    max = length(names),
    initial = 0,
    style = 3
  )
  
  #Create a list to hold converted files
  r.lay <- list()
  
  for (i in 1:length(names)) {
    # set path and filename
    ncpath <- paste("cfactor/", sep = "")  #if needed to change the path
    ncname <- layers[i]
    
    if (layers[i] == "po4" |
        layers[i] == "no3" |
        layers[i] == "so" |
        layers[i] == "chl") {
      
      ncfname <-
        paste(ncpath, ncname, "_", codes[j], "_rf", period.s, ".nc", sep = "")
      
    }
    
    else{
      ncfname <-
        paste(ncpath, ncname, "_", codes[j], "_f", period.s, ".nc", sep = "")
    }
    
    dname <- names[i]  #name of the variable that we will extract
    
    # open a netCDF file
    ncin <- nc_open(ncfname)
    
    # get longitude and latitude
    lon <- ncvar_get(ncin, "lon")
    nlon <- dim(lon)
    
    lat <- ncvar_get(ncin, "lat")
    nlat <- dim(lat)
    
    # get the array of the variable we want to use
    tmp_array <- ncvar_get(ncin, dname)
    
    #close netcdf file
    nc_close(ncin)
    
    #### Convert to raster file
    r <- raster(ncol = 360, nrow = 216)
    
    # create dataframe and add names
    lonlat <- as.matrix(expand.grid(lon, lat))
    
    tmp_vec <- as.vector(tmp_array)
    
    tmp_df01 <- data.frame(cbind(lonlat, tmp_vec))
    names(tmp_df01) <- c("lon", "lat", paste(dname, sep = ""))
    
    c.r <- as.vector(cellFromXY(r, tmp_df01))
    
    r[c.r] <- tmp_vec
    
    #Conversions needed to fit the Bio-ORACLE units
    if (#dname == "no3" |
        #dname == "po4" |
        dname == "si") {
      #Conversion molar/m-3 to micromolar/l
      r <- r * 1000
      plot(r)
    }
    
    # if (dname == "chl") {
    #   #Conversion kg m-3 to mg m-3
    #   r <- r * 1000000
    #   plot(r)
    # }
    
    if (dname == "clt") {
      #Conversion percentage to dec.
      r <- r / 100
      plot(r)
    }
    
    #If you don't want the plots fot each converted file just comment this line
    plot(r)
    
    #Change raster name
    names(r) <- dname
    
    #Paste to the list
    r.lay[[i]] <- r
    
    setTxtProgressBar(pb, i)
    
    gc()
  }
  
  #Set a base file for the interpolation
  #Here using a file already prepared of temperature from B.Oracle
  base <- raster("BO21_tempmax_ss.tif")
  
  
  #Creates a fuction for interpolation
  interpol <- function(x) {
    #Get the name of the layer
    lname <- names(r.lay[[x]])
    
    #Get the layer from the list
    r <- r.lay[[x]]
    
    #Create a dataframe with data from raster
    tmp <-
      data.frame(cbind(xyFromCell(r, 1:length(r[[1]])), values(r[[1]])))
    
    #change colnames
    colnames(tmp) <- c('lon', 'lat', 'var')
    
    #Remove NAs
    tmp <- tmp[!is.na(tmp$var),]
    
    #Crop to the area of study
    tmp <-
      tmp %>% filter(lon >= -90 &
                       lon <= 22) %>% filter(lat >= -42.5 &
                                                lat <= 42.5)
    
    #Convert to SpatialGrid
    coordinates(tmp) <- ~ lon + lat
    
    #Correct CRS
    crs(tmp) <- crs(base)
    crs(r) <- crs(base)
    
    #Create variograms
    #v <- variogram(var ~ 1, tmp)
    #Use automap to fit the variogram automatically
    vario <-
      automap::autofitVariogram(formula = var ~ 1, input_data = tmp)
    m <- vario$var_model
    
    #If wanted, variogram can be fitted mannualy
    #m <- fit.variogram(v, vgm(1, "Sph", 500, 1))
    
    #Create the kriging object, using 12 nearest cells
    gform <- gstat(NULL, "var", var ~ 1, tmp, model = m, nmax = 12)
    
    #Interpolate using the basemap as model
    imap <- interpolate(base, gform)
    
    #Correct raster name
    names(imap) <- lname
    
    #Mask with the base map
    imap <- mask(imap, base)
    
    #Return the object
    return(imap)
  }
  
  #initiate parallel computing
  sfInit(parallel = TRUE, cpus = 3) ##I have 4 cores, using 3 here
  
  ## Export packages
  sfLibrary('raster', character.only = TRUE)
  sfLibrary('automap', character.only = TRUE)
  sfLibrary('gstat', character.only = TRUE)
  sfLibrary('dplyr', character.only = TRUE)
  
  ## Export variables
  sfExport('r.lay')
  sfExport('base')
  
  cat("Starting interpolation. Running in parallel using 3 cores.",
      "\n")
  
  ## Interpolate
  interpol.rasters <- sfLapply(1:length(r.lay), interpol)
  
  ## stop snowfall
  sfStop(nostop = FALSE)
  
  cat("Interpolation finished.", "\n")
  
  cat("Writing rasters.", "\n")
  for (z in 1:length(interpol.rasters)) {
    f.rast <- interpol.rasters[[z]]
    writeRaster(f.rast,
                paste0("final/", set, "/", names(f.rast), ".tif"),
                overwrite = T)
  }
  
  cat(set, " done!", "\n")
  
  gc()
  
}

##END##
