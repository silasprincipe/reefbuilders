###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br
#Collaboration: André L. Acosta

### Generate map with the composition of species ----

# Load base objects for maps ----
source("codes/graphs/plot_maps_base.R")

# Establish the round code to generate map ----
rd <- "rbf1"

#Change of composition (amount) function ----
changeSuit <-
        function(round.code,
                 rcp,
                 year,
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
                        thres <- 750
                        
                        orig[orig < thres] <- 0
                        orig[orig >= thres] <- 1
                        
                        current <- stack(current, orig)
                }
                
                future <- stack()
                
                for (i in 1:3) {
                        sp <- c("muhi", "moca", "side")[i]
                        
                        fut <-
                                raster(
                                        paste0(
                                                sp,
                                                "/proj_",
                                                rcp,
                                                "_",
                                                #year,
                                                #"_",
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
                        thres <- 750
                        
                        fut[fut < thres] <- 0
                        fut[fut >= thres] <- 1
                        
                        future <- stack(future, fut)
                }
                
                #Sum to get species composition
                current <- sum(current)
                
                future <- sum(future)
                
                #Get the difference
                r <- current - future
                
                temp <- as.data.frame(xyFromCell(r, 1:length(r)))
                temp$values <- values(r)
                
                
                #Create the levels according to code
                temp <- temp %>%
                        mutate(
                                level = case_when(
                                        values == -3 ~ "3",
                                        values == -2 ~ "2",
                                        values == -1 ~ "1",
                                        values == 3 ~ "-3",
                                        values == 2 ~ "-2",
                                        values == 1 ~ "-1",
                                        values == 0 ~ "0",
                                        is.na(values) ~ "NA",
                                        TRUE ~ as.character(values)
                                )
                        )
                
                #Create the scale of colors
                main.scale <- scale_fill_manual(
                        values = c(
                                alpha(c("#F3F1EA"), 0),
                                "#fa5f05",
                                "#f3b70d",
                                "#fcff2d",
                                c.pa.1,
                                "#b0f4ef",
                                "#44eab0",
                                "#0a7b4e"
                        ),
                        limits = c("NA", "-3", "-2", "-1", "0", "1", "2", "3"),
                        breaks = c("3", "2", "1", "0", "-1", "-2", "-3"),
                        labels = c("+3", "+2", "+1", "No change", "-1", "-2", "-3"),
                        name = "Change in composition"
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
                
                if (axis.p == 'bottom') {
                        first <- base.b
                }
                
                if (axis.p == 'left-only') {
                        first <- base.lo
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
                                        main.scale + main.theme + 
                                        main.s.x + main.s.y + n.arrow
                        }
                        if (nar == F) {
                                map <- first +
                                        geom_raster(data = temp,
                                                    aes(
                                                            x = x,
                                                            y = y,
                                                            fill = level
                                                    )) +
                                        main.scale + main.theme + 
                                        main.s.x + main.s.y
                        }
                }
                
                return(map)
        }




### Generate change maps for each RCP ----
rcp26 <- changeSuit(rd, "rcp26", nar = T, axis.p = "left-top") +
        theme(legend.position = c(0.24, 0)) +
        # The lines below are a "hack" to adjust the tag 
        # annotation position
        theme(
                plot.margin = unit(c(0.5, 5, 0.5, 5), 'mm'),
                axis.title.y = element_text(
                        margin = margin(t = 0, r = 20, b = 0, l = 0))
        )+ylab(" ") 

rcp45 <-
        changeSuit(rd, "rcp45", nar = F, axis.p = "left-only") + 
        theme(legend.position = "none") +
        # The lines below are a "hack" to adjust the tag 
        # annotation position
        theme(
                plot.margin = unit(c(0.5, 5, 0.5, 5), 'mm'),
                axis.title.y = element_text(
                        margin = margin(t = 0, r = 20, b = 0, l = 0))
        )+ylab(" ") 

rcp85 <-
        changeSuit(rd, "rcp85", nar = F, axis.p = "left") + 
        theme(legend.position = "none") +
        # The lines below are a "hack" to adjust the tag 
        # annotation position
        theme(
                plot.margin = unit(c(0.5, 5, 0.5, 5), 'mm'),
                axis.title.y = element_text(
                        margin = margin(t = 0, r = 20, b = 0, l = 0))
        )+ylab(" ") 

# Assemble using patchwork
ga <- rcp26 / rcp45 / rcp85 + 
        plot_annotation(tag_levels = "A") &
        theme(plot.tag.position = c(0, 0.95),
              plot.tag = element_text(size = 18))


#Save figure ----
ggsave(
        "figures/fig7_composition.tiff",
        ga,
        width = 22.5,
        height = 41,
        dpi = 300,
        units = 'cm'
)

rm(list = ls())
gc()

###END OF MAP GENERATION