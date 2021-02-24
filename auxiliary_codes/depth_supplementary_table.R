###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

# Prepare a table with depth for each scenario/species

round.code <- "rbf1"

full.data <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(full.data) <- c("Scenario", "Min", "Max", "Median", "Q1", "Q3")

for (j in 1:3) {
        
        all.species <- data.frame(matrix(nrow = 4, ncol = 6))
        colnames(all.species) <- c("Scenario", "Min", "Max", "Median", "Q1", "Q3")
        
        sp <- c("muhi", "moca", "side")[j]
        
        for (i in 1:4) {
                
                scenario <- c("current", "rcp26", "rcp45", "rcp85")[i]
                
                bath <- raster("data/env/bath_layers/bath_2_200.tif")
                
                if (scenario == "current") {
                        
                        r <- raster(
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
                        
                }
                
                if (scenario != "current") {
                        r <- raster(
                                paste0(
                                        sp,
                                        "/proj_", scenario, "_",
                                        sp,
                                        "_",
                                        round.code,
                                        "/individual_projections/",
                                        sp,
                                        "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"
                                )
                        )
                }
                
                
                r[r < 750] <- 0
                r[r >= 750] <- 1
                
                r.val <- cbind(values(r), data.frame(xyFromCell(r, 1:ncell(r))))
                r.val[is.na(r.val[,1]),1] <- 0
                r.val <- r.val[r.val[,1] == 1,2:3]
                
                area.t <- raster::area(r)
                
                bath.total <- cbind(raster::extract(bath, r.val), raster::extract(area.t, r.val), r.val)
                names(bath.total) <- c("depth", "area", "long", "lat")
                
                sum.data <- summary(bath.total$depth)
                
                all.species[i,1] <- scenario
                all.species[i,2] <- sum.data[1]
                all.species[i,3] <- sum.data[6]
                all.species[i,4] <- sum.data[3]
                all.species[i,5] <- sum.data[2]
                all.species[i,6] <- sum.data[5]
                
        }
        
        full.data <- rbind(full.data, all.species)
        
}

full.data$Species <- c(rep("muhi", 4), rep("moca", 4), rep("side", 4))

write.csv(full.data, "depth_supl_table.csv")
