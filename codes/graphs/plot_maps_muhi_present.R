###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br
#Collaboration: André L. Acosta

### Generate map for current projection of Mussismilia hispida ----

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
m.hisp.iucn <- st_as_sf(shapefile("gis/iucn_maps/mussismilia_hispida.shp"))
m.hisp.pts <- read.csv("data/muhi/muhi_cell.csv")

m.hisp.range <- base.tr + geom_sf(data = m.hisp.iucn, fill = alpha("#785EF0", 1), color = NA) +
        coord_sf(
                xlim = c(-99, 22),
                ylim = c(-42.5, 42.5),
                expand = FALSE,
                label_axes = list(top = "E", left = "N")
        ) +
        geom_point(data = m.hisp.pts, aes(x = decimalLongitude, y = decimalLatitude),
                   color = "#FFD400", size = 1, alpha = 0.8) + n.arrow +
        main.scale + main.theme + 
        main.s.x + main.s.y 

m.hisp.r.ins <- m.hisp.range +
        coord_sf(
                xlim = c(-49, -35),
                ylim = c(-28.5, -15.5),
                expand = FALSE
        ) + inset.theme

m.hisp.range.f <- m.hisp.range +
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
                "segment",
                #x1 > insh1 ; x2 > insh2
                x = c(-35),
                y = c(-23),
                xend = c(-10),
                yend = c(-23),
                lineend = "round",
                colour = "grey50",
                size = 0.8
        ) +
        annotation_custom(
                grob = ggplotGrob(m.hisp.r.ins),
                xmin = -22,
                xmax = 19,
                ymin = -30,
                ymax = 3
        ) +
        mod.theme +
        ylab(" ") 




#Current map ----
m.hisp <- paSuit("muhi", rd, axis.p = 'left', nar = F) +
        theme(axis.text = element_text(size = 10))

insh1 <- paSuit('muhi', rd, 'inset') +
        coord_sf(
                xlim = c(-40.3, -33),
                ylim = c(-21.5, -16.5),
                expand = FALSE
        )


insh2 <- paSuit('muhi', rd, 'inset') +
        coord_sf(
                xlim = c(-44.6, -38),
                ylim = c(-4, -0.3),
                expand = FALSE
        )


m.hisp.main <- m.hisp +
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
                "rect",
                xmin = -44.6,
                xmax = -38,
                ymin = -4,
                ymax = -0.3,
                color = "grey50",
                alpha = 0,
                size = 0.8
        ) +
        annotate(
                "segment",
                #x1 > insh1 ; x2 > insh2
                x = c(-33, -44.6),
                y = c(-18, -2.3),
                xend = c(-28, -54),
                yend = c(-18, -2.3),
                lineend = "round",
                colour = "grey50",
                size = 0.8
        ) +
        annotation_custom(
                grob = ggplotGrob(insh1),
                xmin = -28,
                xmax = -10,
                ymin = -27,
                ymax = -10
        ) + annotation_custom(
                grob = ggplotGrob(insh2),
                xmin = -80,
                xmax = -52,
                ymin = 0,
                ymax = -16
        ) +
       mod.theme +
       ylab(" ") 



# Create the figure using pacthwork and save ----
muhi.fig1 <- m.hisp.range.f / m.hisp.main +
        plot_annotation(tag_levels = "A") &
        theme(plot.tag.position = c(0, 0.95),
              plot.tag = element_text(size = 18))

ggsave("figures/fig1_muhi_current.tiff", muhi.fig1, width = 20, height = 30,
       dpi = 300, units = 'cm')

rm(list = ls())
gc()

###END