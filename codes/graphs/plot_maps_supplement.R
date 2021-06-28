###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br
#Collaboration: André L. Acosta

### Generate map for current projection of Siderastrea ----

# Load base objects for maps ----
source("codes/graphs/plot_maps_base_supplement.R")

#Composition of current period
comp.cur <- currentSuit("rbf1",
                        nar = T)

comp.cur <- comp.cur+theme(legend.position = c(0.31, 0.02),
                           legend.text.align = 0)

ggsave("figures/sf7_current_composition.tiff", comp.cur, dpi = 300,
       width = 17,
       height = 9)


# Bootstrap
for (i in 1:3) {
        
        species <- c("muhi", "moca", "side")[i]
        
        current <- bootMap(sp = species, scen = "current", 
                           nar = T, axis.p = "left-top")+
                theme(legend.position = c(0.15, 0.05))+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "A",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 8
                )
        current.var <- bootMapVar(sp = species, scen = "curens",
                                  nar = F, axis.p = "right")+
                theme(legend.position = c(0.27, 0.05))+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "B",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 8
                )
        
        rcp26 <- bootMap(sp = species, scen = "rcp26",
                         nar = F, axis.p = "left-only", leg = F)+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "C",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 8
                )
        rcp26.var <- bootMapVar(sp = species, scen = "26rcpens",
                                nar = F, axis.p = "right-only", leg = F)+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "D",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 8
                )
        
        rcp45 <- bootMap(sp = species, scen = "rcp45",
                         nar = F, axis.p = "left-only", leg = F)+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "E",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 8
                )
        rcp45.var <- bootMapVar(sp = species, scen = "45rcpens",
                                nar = F, axis.p = "right-only", leg = F)+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "F",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 8
                )
        
        rcp85 <- bootMap(sp = species, scen = "rcp85",
                         nar = F, axis.p = "left", leg = F)+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "G",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 8
                )
        rcp85.var <- bootMapVar(sp = species, scen = "85rcpens",
                                nar = F, axis.p = "right-bottom", leg = F)+
                annotate(
                        "text",
                        x = 18,
                        y = 39.5,
                        label = "H",
                        fontface = "bold",
                        color = "lightgrey",
                        size = 8
                )
        
        sf.ready <- (current + current.var) / (rcp26 + rcp26.var) / (rcp45 + rcp45.var) / (rcp85 + rcp85.var)
        
        ggsave(paste0("figures/sf", i,"_", species, "_bootstrap.jpg"),
               sf.ready, width = 41, height = 50, dpi = 300, units = 'cm',
               quality = 100)
        
}

rm(list = ls())
gc()

###END