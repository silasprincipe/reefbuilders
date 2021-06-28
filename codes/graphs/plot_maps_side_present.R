###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br
#Collaboration: André L. Acosta

### Generate map for current projection of Siderastrea ----

# Load base objects for maps ----
source("codes/graphs/plot_maps_base.R")

# Modify theme components ----
# The lines below includes a "hack" to adjust the tag 
# annotation position
mod.theme <- theme(
        axis.text = element_text(size = 14, face = "bold"),
        legend.position = c(0.23, 0.04),
        plot.margin = unit(c(0.5, 5, 0.5, 5), 'mm'),
        axis.title.y = element_text(
                margin = margin(t = 0, r = 20, b = 0, l = 0)))

# Establish the round code to generate map ----
rd <- "rbf1"

#Presence points map ----
side1.iucn <- st_as_sf(shapefile("gis/iucn_maps/siderastrea_stellata.shp"))
side2.iucn <- st_as_sf(shapefile("gis/iucn_maps/siderastrea_radians.shp"))

side.pts <- read.csv("data/side/side_cell_thinned.csv")

side.range <- base.tr + geom_sf(data = side1.iucn, fill = alpha("#785EF0", 1), color = NA) +
        geom_sf(data = side2.iucn, fill = alpha("#785EF0", 1), color = NA) +
        coord_sf(
                xlim = c(-99, 22),
                ylim = c(-42.5, 42.5),
                expand = FALSE,
                label_axes = list(top = "E", left = "N")
        ) +
        geom_point(data = side.pts, aes(x = decimalLongitude, y = decimalLatitude),
                   color = "#FFD400", size = 1, alpha = 0.8) + n.arrow +
        main.scale + main.theme + 
        main.s.x + main.s.y 

ins1 <- side.range +
        coord_sf(
                xlim = c(-49, -35),
                ylim = c(-28.5, -15.5),
                expand = FALSE
        ) + inset.theme

ins2 <- side.range +
        coord_sf(
                xlim = c(3, 8),
                ylim = c(-3, 3),
                expand = FALSE
        ) + inset.theme

ins3 <- side.range +
        coord_sf(
                xlim = c(-27, -21),
                ylim = c(14, 20),
                expand = FALSE
        ) + inset.theme

range.f <- side.range +
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
                #x1 > insh1 ; x2 > insh2
                x = c(-49, 6.5, -24),
                y = c(-23, 3, 20),
                xend = c(-80, 6.5, -24),
                yend = c(-23, 15, 30),
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
        annotation_custom(
                grob = ggplotGrob(ins3),
                xmin = -17,
                xmax = -37,
                ymin = 24,
                ymax = 41
        )+
        mod.theme +
        ylab(" ") 


#Current map ----
side <- paSuit("side", rd, axis.p = 'left', nar = F)

inss1 <- paSuit('side', rd, 'inset') +
        coord_sf(
                xlim = c(-67.5, -59),
                ylim = c(11, 19),
                expand = FALSE
        )

inss2 <- paSuit('side', rd, 'inset') +
        coord_sf(
                xlim = c(-40, -33),
                ylim = c(-22, -16),
                expand = FALSE
        )

inss3 <- paSuit('side', rd, 'inset') +
        coord_sf(
                xlim = c(3, 9),
                ylim = c(-3, 3),
                expand = FALSE
        )

inss4 <- paSuit('side', rd, 'inset') +
        coord_sf(
                xlim = c(-27, -21),
                ylim = c(14, 20),
                expand = FALSE
        )



side.main <- side +
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
                grob = ggplotGrob(inss1),
                xmin = -57,
                xmax = -87,
                ymin = -20,
                ymax = 3
        ) + annotation_custom(
                grob = ggplotGrob(inss2),
                xmin = 6,
                xmax = -24,
                ymin = -15,
                ymax = -38
        ) +
        annotation_custom(
                grob = ggplotGrob(inss3),
                xmin = 0,
                xmax = 18,
                ymin = 10,
                ymax = 30
        )+
        annotation_custom(
                grob = ggplotGrob(inss4),
                xmin = -17,
                xmax = -37,
                ymin = 24,
                ymax = 41
        )+
        mod.theme +
        ylab(" ") 

# Create the figure using pacthwork and save ----
side.fig1 <- range.f / side.main +
        plot_annotation(tag_levels = "A") &
        theme(plot.tag.position = c(0, 0.95),
              plot.tag = element_text(size = 18))

ggsave("figures/fig5_side_current.tiff", side.fig1, width = 20, height = 30,
       dpi = 300, units = 'cm')

rm(list = ls())
gc()

###END