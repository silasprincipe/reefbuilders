###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

#### Creates the kernel density maps for the thinning procedure #####
###Note: thinning was done using the "OccurrenceThinner v1.04" from
###Dr. Heroen Verbruggen /// https://github.com/hverbruggen/OccurrenceThinner

# NOTE: this code may take a while to run, depending on your PC

# Load libraries
library(KernSmooth)
library(raster)

# Define species acronyms
grp <- c("muhi", "side", "moca")

# Run in loop
for (i in 1:length(grp)) {
        
        # Load species occurrence data
        pts <- read.csv(paste0("data/", grp[i], "/", grp[i], "_cell.csv"))
        pts <- pts[pts[,3]==1,1:2]
        pts <- cbind(data.frame(species = rep(grp[i], nrow(pts))),
                     pts)
        
        # Generate the density map
        dens <- bkde2D(pts[,2:3], 
                      bandwidth=c(3,3), 
                      gridsize=c(4320,2160),
                      range.x=list(c(-180,180),c(-90,90)))
        
        # Create a raster to hold results
        dens.r <- raster(list(x = dens$x1, y = dens$x2, z = dens$fhat))
        projection(dens.r) <- CRS("+init=epsg:4326")
        xmin(dens.r) <- -180
        xmax(dens.r) <- 180
        ymin(dens.r) <- -90
        ymax(dens.r) <- 90
        
        # Write the final raster
        writeRaster(dens.r,
                    paste0("data/thinning/", grp[i], "_density.asc"),
                    "ascii",
                    overwrite = T)
        
        # Create a csv file with occurrences on the format used by Occ.Thinner
        colnames(pts) <- c("species", "longitude", "latitude")
        write.csv(pts, paste0("data/thinning/", grp[i], "_occ.csv"), 
                  row.names = F, quote = F)
        
        cat(grp[i], "density map done. \n")
        
        ###END
}

###END of code

