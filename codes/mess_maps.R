###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br
#Collaboration: André L. Acosta

### Generate MESS map analysis and plots of MESS maps ----

# Load libraries ----
library(ecospat)
library(raster)
library(ggplot2)
library(patchwork)

# Load functions and prepare other tools (ploting) ----
source("codes/graphs/plot_maps_base.R")
source("functions/Varload.r")
ibm <- c("#2C3E50", "#648FFF", "#785EF0",
         "#DC267F", "#FE6100", "#FFB000") # blindsafe Color scale for MESSneg

# New base necessary here
base.ro <-
        ggplot() + geom_sf(data = base3,
                           color = c.sea,
                           fill = c.sea) +
        geom_sf(data = base2,
                color = c.land,
                fill = c.land) +
        coord_sf(
                xlim = c(-99, 22),
                ylim = c(-42.5, 42.5),
                expand = FALSE,
                label_axes = list(
                        bottom = "",
                        left = "",
                        right = "N",
                        top = ""
                )
        ) + xlab(NULL) + ylab(NULL)

# Load environmental layers (present and future) ----
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

# Convert to dataframe
env.pts <- rasterToPoints(env)
env.pts <- na.omit(env.pts)

r26.pts <- rasterToPoints(r26)
r26.pts <- na.omit(r26.pts)

r45.pts <- rasterToPoints(r45)
r45.pts <- na.omit(r45.pts)

r85.pts <- rasterToPoints(r85)
r85.pts <- na.omit(r85.pts)

# Generate MESS maps and plots for each species ----
for (i in 1:3) {
        # Section 1: get MESS values ----
        
        species <- c("muhi", "moca", "side")[i]
        
        pts <- read.csv(paste("data/", species, "/",
                              species, "_pts.csv", sep = ""))
        
        colnames(pts) <- c("x", "y")
        
        # Extract raster values of reference values
        env.ext <- extract(env, pts)
        
        env.ext <- cbind(pts, env.ext)
        
        # Create a MESS object
        mess.en <- ecospat.mess(env.pts, env.ext, w="default")
        mess.26 <- ecospat.mess(r26.pts, env.ext, w="default")
        mess.45 <- ecospat.mess(r45.pts, env.ext, w="default")
        mess.85 <- ecospat.mess(r85.pts, env.ext, w="default")
        
        # Gets summary if wanted
        summary(mess.en)
        summary(mess.26)
        summary(mess.45)
        summary(mess.85)
        
        # Make the correction in RCP 2.6
        # One of the variables is presenting 9 points with extreme values
        # which make the plots harder to see. This is a problem with the
        # original climate projection, but don't influences our projections
        # as these points don't overlap the occurrences
        out.points <- mess.26[which(mess.26[,"MESS"] %in% 
                                            sort(mess.26[,"MESS"])[1:9]),]
        out.points <- as.data.frame(out.points)
        mess.26[which(mess.26[,"MESS"] %in% 
                              sort(mess.26[,"MESS"])[1:9]), "MESS"] <- NA 
        
        # Convert to data.frame
        mess.en <- as.data.frame(mess.en)
        mess.26 <- as.data.frame(mess.26)
        mess.45 <- as.data.frame(mess.45)
        mess.85 <- as.data.frame(mess.85)
        
        # Convert to factor
        mess.en$MESSneg <- as.factor(mess.en$MESSneg)
        mess.26$MESSneg <- as.factor(mess.26$MESSneg)
        mess.45$MESSneg <- as.factor(mess.45$MESSneg)
        mess.85$MESSneg <- as.factor(mess.85$MESSneg)
        
        # Change factor levels for ploting
        levels(mess.en$MESSneg) <- 0:5
        levels(mess.26$MESSneg) <- 0:5
        levels(mess.45$MESSneg) <- 0:5
        levels(mess.85$MESSneg) <- 0:5
        
        # Section 2: ploting ----
        # ENV current
        # RCP 26
        ten <- base.tl +
                geom_raster(data = mess.en, aes(x = x, y = y, fill = MESS))+
                scale_fill_gradient2(mid = "#2c3e50", midpoint = 0,
                                     low = "#FFF700", high = "#00F2FF")+
                main.theme + main.s.x + main.s.y +
                n.arrow +
                theme(legend.position = "none")+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "A",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 10
                )
        
        nen <- base.tr +
                geom_raster(data = mess.en, aes(x = x, y = y, fill = MESSneg))+
                scale_fill_manual(values = ibm, drop = FALSE, 
                                  guide = guide_legend(reverse=TRUE),
                                  name = "Number of \npredictors")+
                main.theme + main.s.x + main.s.y + 
                theme(legend.text = element_text(color = "white", face = "bold",
                                                 size = 14),
                      legend.title = element_text(color = "white", face = "bold",
                                                  size = 14),
                      legend.position = c(0.17, 0))+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "B",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 10
                )
        
        # RCP 26
        t26 <- base.lo +
                geom_raster(data = mess.26, aes(x = x, y = y, fill = MESS))+
                scale_fill_gradient2(mid = "#2c3e50", midpoint = 0,
                                     low = "#FFF700", high = "#00F2FF")+
                geom_point(data = out.points, aes(x = x, y = y),
                           color = "yellow", size = 0.8)+
                main.theme + main.s.x + main.s.y +
                n.arrow +
                theme(legend.position = "none")+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "C",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 10
                )
        
        n26 <- base.ro +
                geom_raster(data = mess.26, aes(x = x, y = y, fill = MESSneg))+
                scale_fill_manual(values = ibm, drop = FALSE, 
                                  guide = guide_legend(reverse=TRUE),
                                  name = "Number of \npredictors")+
                main.theme + main.s.x + main.s.y + 
                theme(legend.text = element_text(color = "white", face = "bold",
                                                 size = 14),
                      legend.title = element_text(color = "white", face = "bold",
                                                  size = 14),
                      legend.position = "none")+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "D",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 10
                )
        
        # RCP 45
        t45 <- base.lo +
                geom_raster(data = mess.45, aes(x = x, y = y, fill = MESS))+
                scale_fill_gradient2(mid = "#2c3e50", midpoint = 0,
                                     low = "#FFF700", high = "#00F2FF")+
                main.theme + main.s.x + main.s.y +
                theme(legend.position = "none")+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "E",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 10
                )
        
        n45 <- base.ro +
                geom_raster(data = mess.45, aes(x = x, y = y, fill = MESSneg))+
                scale_fill_manual(values = ibm, drop = FALSE, 
                                  guide = guide_legend(reverse=TRUE),
                                  name = "Number of \npredictors")+
                main.theme + main.s.x + main.s.y + 
                theme(legend.position = "none")+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "F",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 10
                )
        
        # RCP 85
        t85 <- base +
                geom_raster(data = mess.85, aes(x = x, y = y, fill = MESS))+
                scale_fill_gradient2(mid = "#2c3e50", midpoint = 0,
                                     low = "#FFF700", high = "#00F2FF")+
                main.theme + main.s.x + main.s.y +
                theme(legend.position = "none")+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "G",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 10
                )
        
        n85 <- base.br +
                geom_raster(data = mess.85, aes(x = x, y = y, fill = MESSneg))+
                scale_fill_manual(values = ibm, drop = FALSE, 
                                  guide = guide_legend(reverse=TRUE),
                                  name = "Number of \npredictors")+
                main.theme + main.s.x + main.s.y + 
                theme(legend.position = "none")+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "H",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 10
                )
        
        
        final <- (ten + nen) / (t26 + n26) / (t45 + n45) / (t85 + n85)
        
        ggsave(paste0("figures/sf",(i+3),"_mess_", species, ".tiff"), final,
               width = 41, height = 50, dpi = 300, units = 'cm')
}

