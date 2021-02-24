###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br
#Collaboration: André L. Acosta

### Generate map for current projection of Montastraea cavernosa ----

# Load base objects for maps ----
source("codes/graphs/plot_maps_base.R")

# Establish the round code to generate map ----
rd <- "rbf1"

#Presence points map ----
moca.iucn <- st_as_sf(shapefile("gis/iucn_maps/montastraea_cavernosa.shp"))
moca.pts <- read.csv("data/moca/moca_cell_thinned.csv")

moca.range <- base.tr + geom_sf(data = moca.iucn, fill = alpha("#785EF0", 1), color = NA) +
        coord_sf(
                xlim = c(-99, 22),
                ylim = c(-42.5, 42.5),
                expand = FALSE,
                label_axes = list(top = "E", left = "N")
        ) +
        geom_point(data = moca.pts, aes(x = decimalLongitude, y = decimalLatitude),
                   color = "#FFD400", size = 1, alpha = 0.8) + n.arrow +
        main.scale + main.theme + 
        main.s.x + main.s.y 

ins1 <- moca.range +
        coord_sf(
                xlim = c(-49, -35),
                ylim = c(-28.5, -15.5),
                expand = FALSE
        ) + inset.theme

ins2 <- moca.range +
        coord_sf(
                xlim = c(3, 8),
                ylim = c(-3, 3),
                expand = FALSE
        ) + inset.theme

range.f <- moca.range +
        annotate(
                "rect",
                xmin = -49,
                xmax = -35,
                ymin = -28.5,
                ymax = -15.5,
                color = "grey50",
                alpha = 0,
                size = 0.8
        ) +
        annotate(
                "rect",
                xmin = 8,
                xmax = 3,
                ymin = -3,
                ymax = 3,
                color = "grey50",
                alpha = 0,
                size = 0.8
        ) +
        annotate(
                "segment",
                #x1 > insh1 ; x2 > insh2
                x = c(-49, 6.5),
                y = c(-23, 3),
                xend = c(-80, 6.5),
                yend = c(-23, 15),
                lineend = "round",
                colour = "grey50",
                size = 0.8
        ) +
        annotation_custom(
                grob = ggplotGrob(ins1),
                xmin = -92,
                xmax = -56,
                ymin = -30,
                ymax = 2
        ) +
        annotation_custom(
                grob = ggplotGrob(ins2),
                xmin = 0,
                xmax = 18,
                ymin = 10,
                ymax = 30
        )+
        ggtitle("A")


#Current map ----
m.cav <- paSuit("moca", rd, axis.p = 'right', nar = F)

insc1 <- paSuit('moca', rd, 'inset') +
        coord_sf(
                xlim = c(-67.5, -59),
                ylim = c(11, 19),
                expand = FALSE
        )

insc2 <- paSuit('moca', rd, 'inset') +
        coord_sf(
                xlim = c(-40, -33),
                ylim = c(-22, -16),
                expand = FALSE
        )

insc3 <- paSuit('moca', rd, 'inset') +
        coord_sf(
                xlim = c(3, 9),
                ylim = c(-3, 3),
                expand = FALSE
        )


m.cav.main <- m.cav +
        annotate(
                "rect",
                xmin = -59,
                xmax = -67.5,
                ymin = 11,
                ymax = 19,
                color = "grey50",
                alpha = 0,
                size = 0.8
        ) +
        annotate(
                "rect",
                xmin = -33,
                xmax = -40,
                ymin = -16,
                ymax = -22,
                color = "grey50",
                alpha = 0,
                size = 0.8
        ) +
        annotate(
                "rect",
                xmin = 9,
                xmax = 3,
                ymin = -3,
                ymax = 3,
                color = "grey50",
                alpha = 0,
                size = 0.8
        )+
        annotate(
                "segment",
                x = c(-59,-60, 6.5),
                y = c(16, -18, 3),
                xend = c(-55,-40, 6.5),
                yend = c(16, -18, 15),
                lineend = "round",
                colour = "grey50",
                size = 0.8
        ) +
        annotation_custom(
                grob = ggplotGrob(insc1),
                xmin = -55,
                xmax = -35,
                ymin = 15,
                ymax = 35
        ) + annotation_custom(
                grob = ggplotGrob(insc2),
                xmin = -55,
                xmax = -80,
                ymin = 5,
                ymax = -25
        ) +
        annotation_custom(
                grob = ggplotGrob(insc3),
                xmin = 0,
                xmax = 18,
                ymin = 10,
                ymax = 30
        )+
        ggtitle("B")


# Create the figure using pacthwork and save ----
mcav.fig1 <- range.f + m.cav.main

ggsave("figures/fig3_moca_current.tiff", mcav.fig1, width = 41, height = 15, dpi = 300, units = 'cm')

rm(list = ls())
gc()

###END