###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

#### Function to load environmental layers #####


Varload <- function(folder = NULL, layers = NULL, bath = NULL) {
  
  library(raster)
  
  #Load the file with layer names
  l.names <- read.table(paste("data/env/", layers, sep = ""), header = F)
  
  #Create the vector of files
  r.names <- paste("data/env/",folder, l.names$V2, ".tif", sep = "")
  
  #Stack files
  env <- raster::stack(as.character(r.names))
  
  rm(l.names, r.names)
  
  #If bath is loaded
  if (!is.null(bath)) {
    
    bath.l <- raster(paste("data/env/bath_layers/bath_",bath,".tif", sep=""))
    
    env <- stack(env, bath.l)
  }
  
  #Return environmental stack
  return(env)

}