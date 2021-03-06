###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

### Function to generate the cross validation blocks ----

blockGen <- function(species.vector){
        
        # Load libraries ----
        library(raster)
        library(blockCV)
        library(sf)
        
        # Set seed for replicability
        set.seed(2932)
        
        # Load environmental data ----
        env <-
                Varload(folder = "crop_layers/",
                        layers = "env_layers.txt",
                        bath = "2_200")
        
        # Create a separate object for muhi
        env.muhi <- env
        
        # Establish the best block size ----
        
        # Mussismilia hispida, constrained area
        # Load Marine Ecoregions of the World dataset
        meow <- shapefile("gis/meow_edited.shp")
        
        # Apply the filter
        pres.meow <- c("Magellanic", 
                       "Warm Temperate Southwestern Atlantic",
                       "Tropical Southwestern Atlantic", 
                       "North Brazil Shelf")
        
        shape <- meow[meow$PROVINCE %in% pres.meow,]
        
        crs(env.muhi) <- CRS('+proj=longlat +datum=WGS84')
        crs(shape) <- CRS('+proj=longlat +datum=WGS84')
        
        env.muhi <- mask(env.muhi, shape)
        
        # Calculate best block size
        sac <- spatialAutoRange(
                rasterLayer = env.muhi,
                sampleNumber = 5000,
                doParallel = TRUE,
                showPlots = F
        )
        
        muhi.range <- sac$range
        
        # Other species, all area; calculate best block size
        sac <- spatialAutoRange(
                rasterLayer = env,
                sampleNumber = 5000,
                doParallel = TRUE,
                showPlots = F
        )
        
        range <- sac$range
        
        # Executes the blocking for each species ----
        for (i in 1:length(species.vector)) {
                
                # Assign species name
                species <- species.vector[i]
                
                # Load species data
                pts <-
                        read.csv(paste("data/", species, "/",
                                       species, "_pts.csv", sep = ""))
                
                dsp <-
                        read.csv(paste("data/", species, "/",
                                       species, "_dsp.csv", sep = ""))
                
                # Prepare data.table
                sp.data <- pts
                sp.data$occ <- dsp$dsp
                sp.data$occ[is.na(sp.data$occ)] <-
                        0 #Block_CV need binary 0/1, and biomod NA for PA.
                
                # Make a SpatialPointsDataFrame object from data.frame
                pa_data <-
                        st_as_sf(
                                sp.data,
                                coords = c("decimalLongitude",
                                           "decimalLatitude"),
                                crs = crs(env)
                        )
                
                # spatial blocking by specified range with random assignment
                
                if (species == "muhi") {
                        env.block <- env.muhi
                        
                        chos.range <- muhi.range
                        
                }
                
                else {
                        env.block <- env
                        
                        chos.range <- range
                }
                
                print(paste("Block size:", chos.range))
                
                sb <- spatialBlock(
                        speciesData = pa_data,
                        species = "occ",
                        rasterLayer = env.block,
                        theRange = chos.range,
                        k = 5,
                        selection = "random",
                        iteration = 100,
                        biomod2Format = TRUE,
                        showBlocks = FALSE
                )
                
                #Create a file with the split table for BIOMOD2
                split.table <- as.data.frame(sb$biomodTable)
                
                #Save split table
                write.csv(
                        split.table,
                        paste("data/", species, "/", species,
                              "_splittable.csv", sep = ""),
                        row.names = F
                )
                
                #save index for BIOMOD Tuning (if needed - not used on our case)
                write.table(sb$foldID,
                            paste("data/", species, "/", species,
                                  "_foldindex.csv", sep = ""))
                
                
                cat(species, "blocking done!", "\n")
                
                
        }
}