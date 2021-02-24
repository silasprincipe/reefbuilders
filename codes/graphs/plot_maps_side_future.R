###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br
#Collaboration: André L. Acosta

### Generate map for future scenarios of Siderastrea ----

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
                        rcpSuit("side", rd, rcp.code, 100, axis.p = axis.sel, nar = nar.sel)
        }
        
        if (i != 1) {
                rcp.plot <-
                        rcpSuit("side", rd, rcp.code, 100, axis.p = axis.sel, nar = nar.sel) +
                        theme(legend.position = 'none')
        }
        
        ins1 <-
                rcpSuit("side",
                        rd,
                        rcp.code,
                        100,
                        axis.p = 'none',
                        theme = 'inset') +
                coord_sf(
                        xlim = c(-67.5, -59),
                        ylim = c(11, 19),
                        expand = FALSE
                )
        
        ins2 <-
                rcpSuit("side",
                        rd,
                        rcp.code,
                        100,
                        axis.p = 'none',
                        theme = 'inset') +
                coord_sf(
                        xlim = c(-40, -33),
                        ylim = c(-22, -16),
                        expand = FALSE
                )
        
        ins3 <-
                rcpSuit("side",
                        rd,
                        rcp.code,
                        100,
                        axis.p = 'none',
                        theme = 'inset') +
                coord_sf(
                        xlim = c(3, 9),
                        ylim = c(-3, 3),
                        expand = FALSE
                )
        
        ins4 <-
                rcpSuit("side",
                        rd,
                        rcp.code,
                        100,
                        axis.p = 'none',
                        theme = 'inset') +
                coord_sf(
                        xlim = c(-27, -21),
                        ylim = c(14, 20),
                        expand = FALSE
                )
        
        
        final.plot <- rcp.plot +
                annotate(
                        "rect",
                        xmin = -67.5,
                        xmax = -59,
                        ymin = 11,
                        ymax = 19,
                        color = "grey50",
                        alpha = 0,
                        size = 0.8
                ) +
                annotate(
                        "rect",
                        xmin = -40,
                        xmax = -33,
                        ymin = -22,
                        ymax = -16,
                        color = "grey50",
                        alpha = 0,
                        size = 0.8
                ) +
                annotate(
                        "rect",
                        xmin = 3,
                        xmax = 9,
                        ymin = -3,
                        ymax = 3,
                        color = "grey50",
                        alpha = 0,
                        size = 0.8
                )+
                annotate(
                        "rect",
                        xmin = -27,
                        xmax = -21,
                        ymin = 14,
                        ymax = 20,
                        color = "grey50",
                        alpha = 0,
                        size = 0.8
                ) +
                annotate(
                        "segment",
                        x = c(-63,-33, 6.5, -24),
                        y = c(11, -18, 3, 20),
                        xend = c(-63,0, 6.5, -24),
                        yend = c(-5, -18, 15, 30),
                        lineend = "round",
                        colour = "grey50",
                        size = 0.8
                ) +
                annotation_custom(
                        grob = ggplotGrob(ins1),
                        xmin = -57,
                        xmax = -87,
                        ymin = -20,
                        ymax = 3
                ) + annotation_custom(
                        grob = ggplotGrob(ins2),
                        xmin = 6,
                        xmax = -24,
                        ymin = -15,
                        ymax = -38
                ) +
                annotation_custom(
                        grob = ggplotGrob(ins3),
                        xmin = 0,
                        xmax = 18,
                        ymin = 10,
                        ymax = 30
                )+
                annotation_custom(
                        grob = ggplotGrob(ins4),
                        xmin = -17,
                        xmax = -37,
                        ymin = 24,
                        ymax = 41
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
side.fig2 <- plots.list[[1]] / plots.list[[2]] / plots.list[[3]] +
        plot_annotation(tag_levels = "A") &
        theme(plot.tag.position = c(0, 0.95),
              plot.tag = element_text(size = 18))

ggsave("figures/fig6_side_future.tiff", side.fig2,
       width = 22.5, height = 41, dpi = 300, units = 'cm')

rm(list = ls())
gc()

###END