###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br
#Collaboration: André L. Acosta

#### Bootstrap for projection coefficient of variation#####
##This code may take several hours to run

#Load libraries
library(raster)
library(dplyr)
library(snowfall)
library(biomod2)

#Set seed for replicability
set.seed(2932)

start.time <- Sys.time()

#Create bootstraping function
bootModel <- function(x){
  
  #Set seed for replicability
  set.seed(2932)
  
  #Names of species
  namesl <- c("muhi", "moca", "side")
  
  #Selec species
  species <- namesl[x]
  
  #Load species data
  pts <- read.csv(paste("data/",species,"/",species,"_pts.csv",sep=""))
  dsp <- read.csv(paste("data/",species,"/",species,"_dsp.csv",sep=""))
  
  # Load environmental layers (present and future) ----
  source("functions/Varload.r")
  env <- Varload(folder = "crop_layers/", layers = "env_layers.txt",
                 bath = "2_200")
  
  r26 <-
    Varload(folder = "proj_layers/rcp26/2100/",
            layers = "env_layers.txt",
            bath = "2_200")
  
  r45 <-
    Varload(folder = "proj_layers/rcp45/2100/",
            layers = "env_layers.txt",
            bath = "2_200")
  
  r85 <-
    Varload(folder = "proj_layers/rcp85/2100/",
            layers = "env_layers.txt",
            bath = "2_200")
  
  #Convert NA to 0 (this will not affect results)
  dsp[is.na(dsp)] <- 0
  
  #Prepare data
  sp.pts <- cbind(pts, dsp)
  
  sp.pres <- sp.pts[sp.pts$dsp == 1,]
  sp.abs <- sp.pts[sp.pts$dsp == 0,]
  
  p.l <- length(sp.pres$decimalLongitude)
  a.l <- length(sp.abs$decimalLongitude)
  
  #Models to bootstrap
  models <- c("RF", "GBM", "GAM", "GLM")
  
  #Create dir to save bootstrap temp maps
  if (dir.exists(paste("boot/",species,"/maps/", sep = "")) == F) {
    dir.create(paste("boot/",species,"/maps/", sep = ""), recursive = T)
  }
  
  #Set working dir as the boot dir
  setwd("boot/")
  
  #Run Bootstrap for 100 times
  for (i in 1:100) {
    
    #Sample presence and absence with replacement (manual bootstraping)
    pres <- sample_n(sp.pres, size = p.l, replace = T)
    abs <- sample_n(sp.abs, size = a.l, replace = T)
    
    #Create dataset
    pa <- rbind(pres, abs)
    
    df <- data.frame(matrix(nrow = (p.l+a.l), ncol = 4))
    
    colnames(df) <- c("decimalLongitude",
                      "decimalLatitude",
                      "dsp",
                      "PA1")
    
    df$decimalLongitude <- pa[,1]
    
    df$decimalLatitude <- pa[,2]
    
    df$dsp <- pa[,3]
    
    df[,4] <- T
    
    # Create the data object to BIOMOD
    biomod.data <- BIOMOD_FormatingData(
      resp.var = df[,3],
      expl.var = env,
      resp.xy = df[,1:2],
      resp.name = species,
      PA.strategy = 'user.defined',
      PA.table = data.frame("PA1" = df[,4])
    )
    
    #Set the options
    biomod.options <- BIOMOD_ModelingOptions()
    
    
    biomod.model.out <- BIOMOD_Modeling(
      biomod.data,
      models = models,
      DataSplit = 100,
      NbRunEval = 1,
      models.options = biomod.options,
      Prevalence = 0.5,
      SaveObj = TRUE,
      modeling.id = species
    )
    
      
    ###### Projection in current conditions #####
    biomod.proj <- BIOMOD_Projection(
      modeling.output = biomod.model.out,
      new.env = env,
      proj.name = 'cur',
      selected.models = 'all',
      compress = F,
      build.clamping.mask = F,
      do.stack = F,
      output.format = '.img',
      on_0_1000 = T
    )
    
    

    # Ensemble modelling and projection in current conditions ----
    # Ensemble of models
    biomod.ensemble <- BIOMOD_EnsembleModeling(
      modeling.output = biomod.model.out,
      chosen.models = 'all',
      em.by = 'all',
      eval.metric = c('TSS'),
      eval.metric.quality.threshold = 0.7,
      prob.mean = F,
      prob.cv = F,
      prob.ci = F,
      prob.ci.alpha = 0.05,
      prob.median = F,
      committee.averaging = T,
      prob.mean.weight = F,
      prob.mean.weight.decay = 'proportional'
    )
    
    
    # Ensemble Forecasting
    biomod.ens.proj <-
      BIOMOD_EnsembleForecasting(
        projection.output = biomod.proj,
        EM.output = biomod.ensemble,
        output.format = '.img',
        on_0_1000 = T
      )
    
    # Projection in future conditions ----
    # Create a function that make ensemble and projection in the future
    futureProj <- function(scenario){
      
      if(scenario == "rcp26"){
        fut.env <- r26
      }
      if(scenario == "rcp45"){
        fut.env <- r45
      }
      if(scenario == "rcp85"){
        fut.env <- r85
      }
      
      
      # Ensemble projection in future condition
      biomod.proj.fut <- BIOMOD_Projection(
        modeling.output = biomod.model.out,
        new.env = fut.env,
        proj.name = scenario,
        selected.models = 'all',
        binary.meth = 'TSS',
        compress = F,
        build.clamping.mask = F,
        do.stack = F,
        output.format = '.img',
        on_0_1000 = T
      )
      
      # Ensemble Forecasting
      biomod.ens.proj <-
        BIOMOD_EnsembleForecasting(
          projection.output = biomod.proj.fut,
          EM.output = biomod.ensemble,
          output.format = '.img',
          on_0_1000 = T
        )
    }
    
    # Apply for all scenarios
    lapply(c("rcp26", "rcp45", "rcp85"), futureProj)
    
    #Save files
    for (j in 1:length(models)) {
      
      cur <- raster(paste0(species, "/proj_cur/individual_projections/","proj_cur_",species,"_PA1_Full_",models[j],".img"))
      
      writeRaster(cur, paste0(species, "/maps/current_",i,"_",models[j],".tif"))
      
      rcp26 <- raster(paste0(species, "/proj_rcp26/individual_projections/","proj_rcp26_",species,"_PA1_Full_",models[j],".img"))
      
      writeRaster(rcp26, paste0(species, "/maps/rcp26_",i,"_",models[j],".tif"))
      
      rcp45 <- raster(paste0(species, "/proj_rcp45/individual_projections/","proj_rcp45_",species,"_PA1_Full_",models[j],".img"))
      
      writeRaster(rcp45, paste0(species, "/maps/rcp45_",i,"_",models[j],".tif"))
      
      rcp85 <- raster(paste0(species, "/proj_rcp85/individual_projections/","proj_rcp85_",species,"_PA1_Full_",models[j],".img"))
      
      writeRaster(rcp85, paste0(species, "/maps/rcp85_",i,"_",models[j],".tif"))
       
    }
    
    
    ###ensembles
    cur <- raster(paste0(species, "/proj_cur/individual_projections/",species,"_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"))
    
    cur <- calc(cur, function(x){
      x[x < 750] <- 0
      x[x >= 750] <- 1
      x
      })
    
    writeRaster(cur, paste0(species, "/maps/curens_",i,".tif"))
    
    rcp26 <- raster(paste0(species, "/proj_rcp26/individual_projections/",species,"_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"))
    
    rcp26 <- calc(rcp26, function(x){
      x[x < 750] <- 0
      x[x >= 750] <- 1
      x
    })
    
    writeRaster(rcp26, paste0(species, "/maps/26rcpens_",i,".tif"))
    
    rcp45 <- raster(paste0(species, "/proj_rcp45/individual_projections/",species,"_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"))
    
    rcp45 <- calc(rcp45, function(x){
      x[x < 750] <- 0
      x[x >= 750] <- 1
      x
    })

    writeRaster(rcp45, paste0(species, "/maps/45rcpens_",i,".tif"))
    
    rcp85 <- raster(paste0(species, "/proj_rcp85/individual_projections/",species,"_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"))
    
    rcp85 <- calc(rcp85, function(x){
      x[x < 750] <- 0
      x[x >= 750] <- 1
      x
    })
    
    writeRaster(rcp85, paste0(species, "/maps/85rcpens_",i,".tif"))
    
    
    gc()
    
    
  }
  
  #Calc mean, standard deviation and coefficient of variation
  scenarios <- c("current",
                 "rcp26",
                 "rcp45",
                 "rcp85",
                 "curens",
                 "26rcpens",
                 "45rcpens",
                 "85rcpens")
  
  
  #Run loop for each scenario
  for (z in 1:length(scenarios)) {
    maps <- stack(paste0(species, "/maps/", 
                         list.files(paste0(species,"/maps/"), pattern = scenarios[z])))
    
    if(scenarios[z] == "current" |
       scenarios[z] == "rcp26" |
       scenarios[z] == "rcp45" |
       scenarios[z] == "rcp85"){
      
      m.mean <- calc(maps, mean)
      
      m.sd <- calc(maps, sd)
      
      m.cv <- m.sd/m.mean
      
      
      writeRaster(m.mean, paste0(species, "/", scenarios[z], "_mean.tif"))
      writeRaster(m.sd, paste0(species, "/", scenarios[z], "_sd.tif"))
      writeRaster(m.cv, paste0(species, "/", scenarios[z], "_cv.tif"))
      
    } 
    
    if(scenarios[z] == "curens" |
       scenarios[z] == "26rcpens" |
       scenarios[z] == "45rcpens" |
       scenarios[z] == "85rcpens"){
      
      m.sum <- calc(maps, sum)
      
      m.freq <- m.sum / nlayers(maps)
      
      m.u2 <- calc(maps, function(x){
        
        if (is.na(sum(x))) {
          
          x <- NA
          
          x
          
        }
        
        if (!is.na(sum(x))) {
          
          v0 <- length(x[x == 0])
          v1 <- length(x[x == 1])
          
          f0 <- v0/length(x)
          f1 <- v1/length(x)
          
          u2 <- 1 - (f0^2) - (f1^2)
          
          x <- u2
          
          x
        }
        
        return(x)
        
      })
      
      
      writeRaster(m.sum, paste0(species, "/", scenarios[z], "_sum.tif"))
      writeRaster(m.freq, paste0(species, "/", scenarios[z], "_freq.tif"))
      writeRaster(m.u2, paste0(species, "/", scenarios[z], "_variability.tif"))
      
    }
    

  }
  
  #Return to the previous working directory
  #setwd("../")
  
  gc()
  
  return(paste0("Bootstrap completed for species ", species))
}

####Run the bootstraping function in parallel
#initiate parallel computing
sfInit(parallel = TRUE, cpus = 3) ##I have 4 cores, using 3 here

## Export packages to cores
sfLibrary('raster', character.only = TRUE)
sfLibrary('biomod2', character.only = TRUE)
sfLibrary('dplyr', character.only = TRUE)

#Export environmental layers load function
source("functions/Varload.r")

sfExport('Varload')


## Run
boot.model <- sfLapply(1:3, bootModel)

## stop snowfall
sfStop(nostop = FALSE)

Sys.time()-start.time

###END###
