###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

### Function to generate pseudoabsences ----

# For Mussismilia hispida the function generates 
# pseudoabsences in a restricted areas
# and in a quantity of 2x the total number of presences
# For the other two species it generates pseudoabsences
# in the whole area and in an amount of 10x the number of presences.
# In both cases, pseudoabsences are generated in an area outside the 
# area predicted as suitable by a presence only model (Mahalanobis distance). 
# To avoid over prediction we ignore areas with small values of probability 
# of suitability in this pre-model (less than 1%).

pseudoabGen <- function(species){
        
        # Load libraries ----
        library(raster)
        library(adehabitatHS)
        library(dismo)
        library(dplyr)
        library(sf)
        source("functions/Varload.r")
        
        # Set parameters ----
        # Set seed for replicability
        set.seed(2932)
        
        # Set number of pseudoabsences according to species
        if (species == "muhi") {
              
                pa.qt = 2
                  
        }
        
        if (species != "muhi") {
                pa.qt = 10 
        }
        
        # Define path to PA preparing
        path <- paste("data/", species, "/", sep = "")
        
        # Read and prepare data ----
        # Read species data
        if (species == 'muhi') {
                
                sp <- read.csv(paste(path, species, "_cell.csv", sep = ""))
                
        }
        
        if (species != 'muhi') { 
                #For MOCA and SIDE we thinned with 0.5 threshold with occ.thin.
                sp <- read.csv(paste(path, species,
                                     "_cell_thinned.csv", sep = ""))
                
        }
        
        # Separate species data into presences and absences
        sp.p <- sp[sp[, 3] == 1, 1:2]
        sp.a <- sp[sp[, 3] == 0, 1:2]
        
        # Load environmental layers
        env <-
                Varload(folder = "crop_layers/",
                        layers = "env_layers.txt",
                        bath = "2_200")
        
        # Ensure all layers have the same NA values
        #env <- mask(env, env$CO_windspeed)
        #env <- mask(env, env$BO21_curvelmean_ss)
        env <- mask(env, env$BO21_curvelmean_ss)

        # Convert environmental layers to SpatialGrid
        sp.data <- as(env, "SpatialPixelsDataFrame")
        
        # Create spatial constraints ----
        # Load Marine Ecoregions of the World dataset
        meow <- shapefile("gis/meow_edited.shp")
        
        # For Mussismilia hispida we limit the generation to some ecoregions
        # (more details in the article)
        
        if (species == 'muhi') {
                
                pres.meow <- c("Magellanic", 
                               "Warm Temperate Southwestern Atlantic",
                               "Tropical Southwestern Atlantic", 
                               "North Brazil Shelf")

                shape <- meow[meow$PROVINCE %in% pres.meow,]
                
                crs(sp.data) <- CRS('+proj=longlat +datum=WGS84')
                crs(shape) <- CRS('+proj=longlat +datum=WGS84')
                
                outline <- over(sp.data, geometry(shape))
                
                sp.data <- sp.data[!is.na(outline), ]
                
                #Create an object that will be used to restric PA sampling
                buf2 <- shape
                
        }
        
        # Mahalanobis presence only model ----
        #Prepare data to Mahalanobis
        slot(sp.data, "data") <-
                dudi.pca(slot(sp.data, "data"), scannf = FALSE)$tab
        
        #Convert occurrence points to SpatialPoints
        occs <-
                SpatialPointsDataFrame(sp[sp[, 3] == 1, 1:2], 
                                       data.frame(sp[sp[, 3] == 1, ]))
        
        #Correct CRS
        crs(occs) <- crs(sp.data)
        
        #Habitat suitability mapping with Mahalonobis distance
        hsm <- mahasuhab(sp.data, occs, type = "probability")
        
        # Save a copy
        hsm_sf <- st_as_sf(hsm)
        st_write(hsm_sf,
                  paste0("gis/hsm_", species, ".shp"),
                  driver = "ESRI Shapefile",
                  append = FALSE)
        
        # Remove areas from environment ----
        # 1: buffer of 40km (~4 cells) around presences
        buf1 <- circles(sp.p, lonlat = T, d = 40000)
        
        # 2: Remove cells already existing as absences
        env[cellFromXY(env, sp.a[, 1:2])] <- NA
        
        # 3: Remove cells from the suitable area of Mahalonobis distance
        c.mh <- data.frame(cbind(hsm@coords, hsm@data))
        c.mh <- c.mh[c.mh$MD > 0.01, ] # We ignore points with very low values 
                                       # of suitability (less than 1%)
        env[cellFromXY(env, c.mh[, 1:2])] <- NA
        
        # Sampling of points ----
        # Sample random points in the environmental area
        samp <-
                as.data.frame(sampleRandom(
                        env,
                        size = 50000,
                        na.rm = TRUE,
                        xy = TRUE
                )[, 1:2])
        
        #Convert to spatial points
        samp.sp <- SpatialPoints(coords = samp[, 1:2],
                                 proj4string = CRS('+proj=longlat +datum=WGS84'))
        
        #Remove points that are in the buffered area (close to presences)
        out <- over(samp.sp, geometry(buf1))
        samp.cl <- samp[is.na(out), ]
        
        
        # If Mussismilia hispida, limit to the selected area
        if (species == "muhi") {
                
                #Select points that fall just in the second buffer
                samp.muhi <- SpatialPoints(coords = samp.cl[, 1:2],
                                         proj4string = CRS('+proj=longlat +datum=WGS84'))
                
                crs(buf2) <- CRS('+proj=longlat +datum=WGS84')
                
                out <- over(samp.muhi, geometry(buf2))
                
                samp.cl <- samp.cl[!is.na(out), ]
                
        }
        
        
        #Sample final points
        samp.final <-
                sample_n(samp.cl, (length(sp.p$decimalLongitude) * pa.qt) - 
                                 length(sp.a$decimalLongitude))
        
        # Create the datasets in the final format----
        # Create a clean dataframe
        pa.dataset <- as.data.frame(matrix(nrow = 0, ncol = 4))
        
        # Merge sampled points to the dataframe
        samp.final$dsp <- NA
        samp.final$PA1 <- TRUE
        
        pa.dataset <- rbind(pa.dataset, samp.final)
        
        colnames(pa.dataset) <- c("decimalLongitude",
                                  "decimalLatitude",
                                  "dsp",
                                  "PA1")
        
        
        # Create datasets from presence/absence data
        pres <- pa.dataset[0, ]
        abs <- pa.dataset[0, ]
        
        pres <- bind_rows(pres, sp.p)
        pres$dsp <- 1
        pres$PA1 <- TRUE
        
        abs <- bind_rows(abs, sp.a)
        
        if (length(abs$decimalLongitude) > 0) {
                abs$dsp <- 0
                abs$PA1 <- TRUE
        }
        
        # Bind all datasets
        pa.table <- bind_rows(pres, abs, pa.dataset)
        
        # Verify if the dataframe was correctly generated
        print(table(pa.table$dsp[pa.table$PA1 == T], useNA = "ifany"))
        
        # Check if the procedure worked and don't produced any duplicated
        print(table(duplicated(pa.table[, 1:2])))
        
        # Write final file with full table
        write.csv(pa.table,
                  paste(path, species, "_pa_table.csv", sep = ""),
                  row.names = F)
        
        # Write separate final files (for use with biomod2)
        pts <- pa.table[, 1:2]
        dsp <- as.data.frame(pa.table[, 3])
        colnames(dsp) <- "dsp"
        set <- data.frame(pa.table[, 4])
        colnames(set) <- "PA1"
        
        write.csv(pts,
                  paste(path, species, "_pts.csv", sep = ""),
                  row.names = F)
        write.csv(dsp,
                  paste(path, species, "_dsp.csv", sep = ""),
                  row.names = F)
        write.csv(set,
                  paste(path, species, "_set.csv", sep = ""),
                  row.names = F)
        
        cat(species, "pseudoabsence random selection done. \n")
        
        
}