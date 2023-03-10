# -------------------------------------
# Script: SUPP-MAT_plot-decision-points
# Author: Dr Natasha Gillies
# Purpose: Plot location of decision points used in infrasound analysis
# Notes:
# Date: 2023-01-30
# -------------------------------------

### 0.0 Load functions & packages ----------------------------------------------

# 0.0.0 Functions

# 0.0.1 Define packages
packages <- c("ggplot2", "dplyr", "raster", "rgdal", "rnaturalearth", "rnaturalearthdata", 
              "ggspatial", "ggpubr")

# 0.0.2 Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# 0.0.3 Load packages
invisible(lapply(packages, library, character.only = TRUE))


### 1.0 Load & process data ----------------------------------------------------

plotDat <- data.table::fread("Data_inputs/WAAL_2013_gps_processed_aperture60deg.csv", 
                            data.table = F)

# 1.0.1 Keep one row per decision point
plotDat <- subset(plotDat, segment_ID == 1)

## 1.1 Set projections ---------------------------------------------------------

# 1.1.0 Set the map projections - Azimuthal Equal Area projection centred on Crozet 
proj.dec <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj.utm <- "+proj=laea +lat_0=-46.358639 +lon_0=51.706972 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# 1.1.2 Convert projections of the GPS dataset
plotDat.proj <- plotDat
coordinates(plotDat.proj) <- ~ x_lon+y_lat
proj4string(plotDat.proj) <- proj.dec

plotDat.sp <- spTransform(plotDat.proj, CRS(proj.utm))
plotDat.df <- as.data.frame(plotDat.sp)

# 1.1.3 Set the position of Crozet
colony <- data.frame(Lon = 51.706972, Lat = -46.358639)

# 1.1.4 Set Crozet projection
coordinates(colony) <- ~Lon+Lat
proj4string(colony) <- proj.dec
colony.sp <- spTransform(colony, CRS(proj.utm))
colony.df <- as.data.frame(colony.sp)


### 2.0 Build the GPS plot -----------------------------------------------------

# 2.0.0 Download the world map
world <- ne_countries(scale = "medium", returnclass = "sf")
world2 <- sf::st_transform(world, crs = proj.utm)

# 2.0.1 Set male/female colours
col_M <- "#E1BE6A" # yellow
col_F <- "#40B0A6" # teal


#### FIGURE SX - Map of all decision points ------------------------------------
png("Figures/FIGSX_decision-points-map.png", width = 7, height = 7, units = "in", res = 350)
ggplot(data = world2) + 
  geom_sf(fill = "cadetblue", colour = "grey") +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25, style = "bar") +
  coord_sf(crs = proj.utm, xlim = c(-3100000, 3200000), ylim = c(-2500000,2500000),
           label_axes = list(top = "E", left = "N", bottom = "E", right = "N")) +
  geom_point(aes(x = x_lon, y = y_lat, colour = Sex, group = "identity"), 
             size = 1, dat = plotDat.df, alpha = 0.8) + # Add 'alpha = 0.8' for readability (runs very slow)
  scale_colour_manual(values = c(col_F, col_M)) +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "grey80"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x.bottom = element_blank(), 
        axis.title.x.bottom = element_blank(),
        axis.text.y.right = element_blank(), 
        axis.title.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        legend.position = c(0.93,0.9),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)) +
  annotate("point", shape = 17, (colony.df$Lon + 100), colony.df$Lat) +
  annotate("text", label = "Crozet", colony.df$Lon, (colony.df$Lat - 65000))
dev.off()
