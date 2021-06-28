###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br
#Collaboration: André L. Acosta

### Generate base objects for the creation of maps ----
# This code is the base for the other 'plot_maps_*' codes
# Here it's possible to change colors, themes, etc.

# Load libraries ----
library(ggplot2)
library(dplyr)
library(sf)
library(raster)
library(grid)
library(png)
library(patchwork)

#### Define project colors ----
#Base maps
c.sea <- c("#696969") #### opt lightgrey
c.land <- c("#545454") #### opt darkgrey

#Presence/absence maps
c.pa.1 <- c("#2c3e50") ### opt 34495e
c.pa.2 <- c("#00DBC7")

#RCP maps
c.gain <- c("#a13eda")
c.kept <- c("#00c5b3")
c.lost <- c("#f1e40e")
c.study <- c("#2c3e50")


#### Establish base configurations and maps ####

### Main theme ----
main.theme <-
        theme(
                panel.background = element_rect(fill = "#232227", 
                                                colour = "#494949"), ###opt DFDFDF
                panel.grid.major = element_line(linetype = 'solid', 
                                                colour = "#494949"),
                #panel.grid.major = element_blank(), #If wanted to remove line grid
                axis.ticks.length = unit(0.8, "mm"),
                axis.text = element_text(size = 18, face = "bold"),
                legend.justification = c(1, 0),
                legend.position = c(0.28, 0.05),
                legend.background = element_blank(),
                legend.title = element_text(size = 16, color = "white", face = "bold"),
                legend.text = element_text(size = 16, color = "white", face = "bold"),
                legend.key.size = unit(6, 'mm'),
                legend.key = element_rect(colour = "#696969", size =
                                                  2.5),
                plot.margin = unit(c(0.3, 0.35, 0.3, 0), 'mm')
        )


### Inset maps theme ----
inset.theme <- theme_void() + theme(
        legend.position = 'none',
        axis.title = element_blank(),
        panel.background = element_rect(fill = "#232227"),
        panel.border = element_rect(fill = 'transparent', colour = "grey50", size = 1),
        axis.ticks.length = unit(-0.25, "cm"),
        axis.text = element_blank()
)

### Base maps ----

#Pacific ocean
base3 <- shapefile("gis/basemaps/ne_110m_land_no_oc.shp")
#America
base2 <- shapefile("gis/basemaps/ne_110m_land_edited.shp")

#Convert to sf
base3 <- st_as_sf(base3)
base2 <- st_as_sf(base2)

#Create different kinds of base according to the position of
#the axis annotation
base <-
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
        ) + xlab(NULL) + ylab(NULL)

base.tr <-
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
                label_axes = list(top = "E", right = "N")
        ) + xlab(NULL) + ylab(NULL)

base.tl <-
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
                label_axes = list(top = "E", left = "N")
        ) + xlab(NULL) + ylab(NULL)

base.br <-
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
                        bottom = "E",
                        right = "N",
                        top = ""
                )
        ) + xlab(NULL) + ylab(NULL)

base.lo <-
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
                        left = "N",
                        top = ""
                )
        ) + xlab(NULL) + ylab(NULL)

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
                        right = "N",
                        top = ""
                )
        ) + xlab(NULL) + ylab(NULL)

### Color scales ----
### Suitability color scale
main.scale <-
        scale_fill_manual(
                values = c(alpha(c("#F3F1EA"), 0), c.pa.1, c.pa.2),
                limits = c("NA", "A", "P"),
                breaks = c("A", "P"),
                name = "",
                labels = c("Unsuitable", "Suitable")
        )

### Gain/lost color scale
rcp.scale <-
        scale_fill_manual(
                values = c(alpha(c("#F3F1EA"), 0), c.study, c.gain, c.kept, c.lost),
                limits = c("NA", "B", "G", "K", "L"),
                breaks = c("G", "K", "L"),
                name = "Suitable area",
                labels = c("Gain", "Kept", "Lost")
        )


### Main maps xis scales
# main.s.x <-
#         scale_x_continuous(breaks = seq(18,-100,-10),
#                            limits = c(-100,22))
# main.s.y <-
#         scale_y_continuous(breaks = seq(-40, 40, 10),
#                            limits = c(-42.5, 42.5))
main.s.x <-
        scale_x_continuous(breaks = seq(15,-100,-15),
                           limits = c(-100,22))
main.s.y <-
        scale_y_continuous(breaks = seq(-30, 30, 15),
                           limits = c(-42.5, 42.5))

### Arrow ----
img <- readPNG("gis/n_arrow.png")
g <- rasterGrob(img, interpolate = TRUE)
n.arrow <- annotation_custom(
        g,
        xmin = -99,
        xmax = -89,
        ymin = 34,
        ymax = 41.5
)

### Break functions ----
#Current composite function
currentSuit <-
        function(round.code,
                 theme = 'main',
                 nar = F,
                 axis.p = 'left') {
                
                current <- stack()
                
                for (i in 1:3) {
                        sp <- c("muhi", "moca", "side")[i]
                        
                        orig <- raster(
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
                        
                        # eval <-
                        #         read.csv(
                        #                 paste0(
                        #                         sp,
                        #                         "/evals/ensemble_eval_",
                        #                         sp,
                        #                         "_",
                        #                         round.code,
                        #                         ".csv"
                        #                 )
                        #         )
                        # 
                        # eval <-
                        #         eval[eval$Model.name %in% c("EMwmeanByTSS_mergedAlgo_mergedRun_mergedData"),]
                        # eval <-
                        #         eval %>% filter(Eval.metric == 'TSS')
                        # thres <- eval$Cutoff
                        thres <-750
                        
                        orig[orig < thres] <- 0
                        orig[orig >= thres] <- i
                        
                        current <- stack(current, orig)
                        
                }
                
                current[[3]] <- current[[3]] * 2
                
                r <- sum(current)
                
                temp <- as.data.frame(xyFromCell(r, 1:length(r)))
                temp$values <- values(r)
                
                
                #
                temp <- temp %>%
                        mutate(
                                level = case_when(
                                        values == 1 ~ "MUHI",
                                        values == 2 ~ "MOCA",
                                        values == 3 ~ "MUHI+MOCA",
                                        values == 6 ~ "SIDE",
                                        values == 7 ~ "SIDE+MUHI",
                                        values == 8 ~ "SIDE+MOCA",
                                        values == 9 ~ "ALL",
                                        values == 0 ~ "NONE",
                                        is.na(values) ~ "NA",
                                        TRUE ~ as.character(values)
                                )
                        )
                
                
                main.scale <- scale_fill_manual(
                        values = c(
                                alpha(c("#F3F1EA"), 0),
                                c.pa.1,
                                "#e69f00",
                                "#56b4e9",
                                "#009e73",
                                "#f0e442",
                                "#0072b2",
                                "#d55e00",
                                "#ab59ad"
                        ),
                        limits = c(
                                "NA",
                                "NONE",
                                "MUHI",
                                "MOCA",
                                "SIDE",
                                "MUHI+MOCA",
                                "SIDE+MUHI",
                                "SIDE+MOCA",
                                "ALL"
                        ),
                        breaks = c(
                                "MUHI",
                                "MOCA",
                                "SIDE",
                                "MUHI+MOCA",
                                "SIDE+MUHI",
                                "SIDE+MOCA",
                                "ALL"
                        ),
                        labels = c(
                                expression(italic("M. hispida")),
                                expression(italic("M. cavernosa")),
                                expression(italic("Siderastrea")),
                                expression(paste(
                                        italic("M. hispida"),
                                        "  and  ",
                                        italic("M. cavernosa")
                                )),
                                expression(paste(
                                        italic("Siderastrea"),
                                        "  and  ",
                                        italic("M. hispida")
                                )),
                                expression(paste(
                                        italic("Siderastrea"),
                                        "  and  ",
                                        italic("M. cavernosa")
                                )),
                                "All"
                        ),
                        name = "Composition"
                )
                
                
                if (axis.p == 'left') {
                        first <- base
                }
                
                if (axis.p == 'right') {
                        first <- base.tr
                }
                
                if (axis.p == 'left-top') {
                        first <- base.tl
                }
                
                if (axis.p == 'right-bottom') {
                        first <- base.br
                }
                
                if (axis.p == 'left-only') {
                        first <- base.lo
                }
                
                
                if (axis.p == 'right-only') {
                        first <- base.ro
                }
                
                if (axis.p == 'none') {
                        first <-
                                base + theme(axis.ticks = element_blank(),
                                             axis.text = element_blank())
                }
                
                if (theme == 'main') {
                        if (nar == T) {
                                map <- first +
                                        geom_raster(data = temp,
                                                    aes(
                                                            x = x,
                                                            y = y,
                                                            fill = level
                                                    )) +
                                        main.scale + main.theme + main.s.x + main.s.y + n.arrow
                        }
                        if (nar == F) {
                                map <- first +
                                        geom_raster(data = temp,
                                                    aes(
                                                            x = x,
                                                            y = y,
                                                            fill = level
                                                    )) +
                                        main.scale + main.theme + main.s.x + main.s.y
                        }
                }
                
                if (theme == 'inset') {
                        map <- first +
                                geom_raster(data = temp, aes(
                                        x = x,
                                        y = y,
                                        fill = level
                                )) +
                                main.scale + inset.theme
                }
                
                return(map)
        }


bootMap <-
        function(sp,
                 round.code,
                 scen,
                 theme = 'main',
                 nar = T,
                 axis.p = 'left',
                 leg = T) {
                
                r <- raster(
                        paste0(
                                "boot/",
                                sp, "/",
                                scen, "_cv.tif"
                        )
                )
                
                xyrast <- as.data.frame(xyFromCell(r, 1:ncell(r)))
                xyrast$val <- values(r)
                
                con.scale <- scale_fill_gradient(low = "#0ECCAB",
                                                 high = "#FF6507",
                                                 na.value = alpha("grey", 0),
                                                 name = "CV",
                                                 limits = c(0,5),
                                                 breaks = c(0,2.5,5))
                
                if (axis.p == 'left') {
                        first <- base
                }
                
                if (axis.p == 'right') {
                        first <- base.tr
                }
                
                if (axis.p == 'left-top') {
                        first <- base.tl
                }
                
                if (axis.p == 'right-bottom') {
                        first <- base.br
                }
                
                if (axis.p == 'left-only') {
                        first <- base.lo
                }
                
                
                if (axis.p == 'right-only') {
                        first <- base.ro
                }
                
                if (axis.p == 'none') {
                        first <-
                                base + theme(axis.ticks = element_blank(),
                                             axis.text = element_blank())
                }
                
                
                if (theme == 'main') {
                        if (nar == T) {
                                map <- first +
                                        geom_raster(data = xyrast,
                                                    aes(
                                                            x = x,
                                                            y = y,
                                                            fill = val
                                                    ))+con.scale+
                                        guides(fill = guide_colorbar(
                                                draw.ulim = FALSE,
                                                draw.llim = FALSE,
                                                ticks = F))+
                                        main.theme +
                                        main.s.x + main.s.y +
                                        n.arrow
                        }
                        if (nar == F) {
                                map <- first +
                                        geom_raster(data = xyrast,
                                                    aes(
                                                            x = x,
                                                            y = y,
                                                            fill = val
                                                    ))+con.scale+
                                        guides(fill = guide_colorbar(
                                                draw.ulim = FALSE,
                                                draw.llim = FALSE,
                                                ticks = F))+
                                        main.theme +
                                        main.s.x + main.s.y
                        }
                }
                
                if (leg == T) {
                        map <- map+theme(legend.position = c(0.15, 0))   
                }
                
                if (leg == F) {
                        map <- map+theme(legend.position = "none")   
                }
                
                return(map)
        }



bootMapVar <-
        function(sp,
                 round.code,
                 scen,
                 theme = 'main',
                 nar = T,
                 axis.p = 'left',
                 leg = T) {
                
                r <- raster(
                        paste0(
                                "boot/",
                                sp, "/",
                                scen, "_variability.tif"
                        )
                )
                
                xyrast <- as.data.frame(xyFromCell(r, 1:ncell(r)))
                xyrast$val <- values(r)
                
                xyrast <- xyrast %>%
                        mutate(
                                level = case_when(
                                        val == 0 ~ "0",
                                        val > 0 & val < 0.1 ~ "<0.1",
                                        val >= 0.1 & val < 0.2 ~ "<0.2",
                                        val >= 0.2 & val < 0.3 ~ "<0.3",
                                        val >= 0.3 & val < 0.4 ~ "<0.4",
                                        val >= 0.4 ~ "<=0.5",
                                        is.na(val) ~ "NA",
                                        TRUE ~ as.character(val)))
                
                
                con.scale <- scale_fill_manual(
                        values = c(
                                alpha(c("#F3F1EA"), 0),
                                "#263545",
                                "#127AC9",
                                "#00E7FF",
                                "#DDFF00",
                                "#FFB000",
                                "#FE6100"
                        ),
                        limits = c(
                                "NA",
                                "0",
                                "<0.1",
                                "<0.2",
                                "<0.3",
                                "<0.4",
                                "<0.5"
                        ),
                        breaks = c(
                                "<0.5",
                                "<0.4",
                                "<0.3",
                                "<0.2",
                                "<0.1",
                                "0"
                        ),
                        name = "Unalikeability"
                )
                
                
                if (axis.p == 'left') {
                        first <- base
                }
                
                if (axis.p == 'right') {
                        first <- base.tr
                }
                
                if (axis.p == 'left-top') {
                        first <- base.tl
                }
                
                if (axis.p == 'right-bottom') {
                        first <- base.br
                }
                
                if (axis.p == 'left-only') {
                        first <- base.lo
                }
                
                if (axis.p == 'right-only') {
                        first <- base.ro
                }
                
                if (axis.p == 'none') {
                        first <-
                                base + theme(axis.ticks = element_blank(),
                                             axis.text = element_blank())
                }
                
                
                if (theme == 'main') {
                        if (nar == T) {
                                map <- first +
                                        geom_raster(data = xyrast,
                                                    aes(
                                                            x = x,
                                                            y = y,
                                                            fill = level
                                                    ))+con.scale+
                                        main.theme +
                                        main.s.x + main.s.y +
                                        n.arrow
                        }
                        if (nar == F) {
                                map <- first +
                                        geom_raster(data = xyrast,
                                                    aes(
                                                            x = x,
                                                            y = y,
                                                            fill = level
                                                    ))+con.scale+
                                        main.theme +
                                        main.s.x + main.s.y
                        }
                }
                
                if (leg == T) {
                        map <- map+theme(legend.position = c(0.15, 0))   
                }
                
                if (leg == F) {
                        map <- map+theme(legend.position = "none")   
                }
                
                return(map)
        }

###END