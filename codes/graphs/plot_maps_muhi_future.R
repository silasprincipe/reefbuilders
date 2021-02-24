###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br
#Collaboration: André L. Acosta

### Generate map for future scenarios of Mussismilia hispida ----

# Load base objects for maps ----
source("codes/graphs/plot_maps_base.R")

# Establish the round code to generate map ----
rd <- "rbf1"

# Create a list to hold plots
plots.list <- list()

# Execute all the scenarios in loop ----
for (i in 1:3) {
        
        rcp.code <- c("rcp26", "rcp45", "rcp85")[i]
        
        
        if (i == 1) {
                axis.sel <- "left-top"
                nar.sel = TRUE
        }
        
        
        if (i == 2) {
                axis.sel <- "left-only"
                nar.sel = FALSE
        }
        
        
        if (i == 3) {
                axis.sel <- "left"
                nar.sel = FALSE
        }
        
        
        
        if (i == 1) {
                rcp.plot <-
                        rcpSuit("muhi", rd, rcp.code, 100, axis.p = axis.sel, nar = nar.sel)
        }
        
        
        if (i != 1) {
                rcp.plot <-
                        rcpSuit("muhi", rd, rcp.code, 100, axis.p = axis.sel, nar = nar.sel) +
                        theme(legend.position = 'none') 
        }
        
        
        ins1 <-
                rcpSuit("muhi",
                        rd,
                        rcp.code,
                        100,
                        axis.p = 'none',
                        theme = 'inset') +
                coord_sf(
                        xlim = c(-42, -31),
                        ylim = c(-10.5, -1),
                        expand = FALSE
                )
        
        ins2 <-
                rcpSuit("muhi",
                        rd,
                        rcp.code,
                        100,
                        axis.p = 'none',
                        theme = 'inset') +
                coord_sf(
                        xlim = c(-40.3, -33),
                        ylim = c(-21.5, -16.5),
                        expand = FALSE
                )
        
        final.plot <- rcp.plot +
                annotate(
                        "rect",
                        xmin = -42,
                        xmax = -31,
                        ymin = -10.5,
                        ymax = -1,
                        color = "grey50",
                        alpha = 0,
                        size = 0.8
                ) +
                annotate(
                        "rect",
                        xmin = -40.3,
                        xmax = -33,
                        ymin = -21.5,
                        ymax = -16.5,
                        color = "grey50",
                        alpha = 0,
                        size = 0.8
                ) +
                annotate(
                        "segment",
                        x = c(-65, -33),
                        y = c(-5, -18),
                        xend = c(-42, -20),
                        yend = c(-5, -18),
                        lineend = "round",
                        colour = "grey50",
                        size = 0.8
                ) +
                annotation_custom(
                        grob = ggplotGrob(ins1),
                        xmin = -85,
                        xmax = -55,
                        ymin = 5,
                        ymax = -21
                ) +
                annotation_custom(
                        grob = ggplotGrob(ins2),
                        xmin = -26,
                        xmax = -2,
                        ymin = -27,
                        ymax = -9
                )+  # The lines below are a "hack" to adjust the tag 
                # annotation position
                theme(
                        plot.margin = unit(c(0.5, 5, 0.5, 5), 'mm'),
                        axis.title.y = element_text(
                                margin = margin(t = 0, r = 20, b = 0, l = 0))
                )+ylab(" ") 
        
        plots.list[[i]] <- final.plot

}

# Create the figure using pacthwork and save ----
muhi.fig2 <- plots.list[[1]] / plots.list[[2]] / plots.list[[3]] +
        plot_annotation(tag_levels = "A") &
        theme(plot.tag.position = c(0, 0.95),
              plot.tag = element_text(size = 18))

ggsave("figures/fig2_muhi_future.tiff", muhi.fig2,
       width = 22.5, height = 41, dpi = 300, units = 'cm')

rm(list = ls())
gc()