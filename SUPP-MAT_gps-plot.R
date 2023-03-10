
## ---------------------------
##
## Script name: SUPP-MAT_gps-plot.R
##
## Purpose of script: Plot all the GPS tracks, coloured by sex
##
## Author: Dr. Natasha Gillies
##
## Date Created: 2022-04-01
##
## Email: gilliesne@gmail.com
##
## ---------------------------


### 0.0 Load the packages ------------------------------------------------------

packages <- c("raster", "rgdal", "rnaturalearth", "rnaturalearthdata", "ggplot2", 
              "ggspatial", "ggpubr")

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))


## 0.1 Load data and set map projections ---------------------------------------

# 0.1.0 Load the HMM-labelled GPS dataset
gpsDat <- read.csv("Data_inputs/WAAL_2013_gps_labelled.csv")

# 0.1.1 Load the decision point dataset
pointDat <- data.table::fread("Data_inputs/WAAL_2013_gps_processed_aperture60deg.csv", 
                             data.table = F)

# 0.1.2 Keep one row per decision point
pointDat <- subset(pointDat, segment_ID == 1)


## 0.2 Set the map projections - Azimuthal Equal Area projection centred on Crozet ----
proj.dec <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj.utm <- "+proj=laea +lat_0=-46.358639 +lon_0=51.706972 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# 0.2.1 Convert projections of the GPS dataset
gpsDat.proj <- gpsDat
coordinates(gpsDat.proj) <- ~ x_lon+y_lat
proj4string(gpsDat.proj) <- proj.dec

gpsDat.sp <- spTransform(gpsDat.proj, CRS(proj.utm))
gpsDat.df <- as.data.frame(gpsDat.sp)

# 0.2.2 Convert projections of the points dataset
pointDat.proj <- pointDat
coordinates(pointDat.proj) <- ~ x_lon+y_lat
proj4string(pointDat.proj) <- proj.dec

pointDat.sp <- spTransform(pointDat.proj, CRS(proj.utm))
pointDat.df <- as.data.frame(pointDat.sp)

# 0.2.3 Set the position of Crozet
colony <- data.frame(Lon = 51.706972, Lat = -46.358639)

# 0.2.4 Set Crozet projection
coordinates(colony) <- ~Lon+Lat
proj4string(colony) <- proj.dec
colony.sp <- spTransform(colony, CRS(proj.utm))
colony.df <- as.data.frame(colony.sp)


### 1.0 Make the plot ----------------------------------------------------------

# 1.0.0 Download the world map
world <- ne_countries(scale = "medium", returnclass = "sf")
world2 <- sf::st_transform(world, crs = proj.utm)

# 1.0.1 Set male/female colours
col_M <- "#E1BE6A" # yellow
col_F <- "#40B0A6" # teal


## 1.1 Map of all GPS fixes ----------------------------------------------------
gps_map <- ggplot(data = world2) + 
  geom_sf(fill = "cadetblue", colour = "grey") +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25, style = "bar") +
  coord_sf(crs = proj.utm, xlim = c(-3100000, 3200000), ylim = c(-2500000,2500000),
           label_axes = list(top = "E", left = "N", bottom = "E", right = "N")) +
  geom_path(aes(x = x_lon, y = y_lat, colour = Sex, group = "identity"), 
            size = 1, dat = gpsDat.df) + # Add 'alpha = 0.8' for readability (runs very slow)
  scale_colour_manual(values = c(col_F, col_M)) +
  labs(tag = "(a)") +
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
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.tag = element_text(size = 18)) +
  annotate("point", shape = 17, (colony.df$Lon + 100), colony.df$Lat) +
  annotate("text", label = "Crozet", colony.df$Lon, (colony.df$Lat - 65000))

## 1.1.1 Map of decision points ------------------------------------------------

points_map <- ggplot(data = world2) + 
  geom_sf(fill = "cadetblue", colour = "grey") +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25, style = "bar") +
  coord_sf(crs = proj.utm, xlim = c(-3100000, 3200000), ylim = c(-2500000,2500000),
           label_axes = list(top = "E", left = "N", bottom = "E", right = "N")) +
  geom_point(aes(x = x_lon, y = y_lat, colour = Sex, group = "identity"), 
             size = 1, dat = pointDat.df, alpha = 0.8) + # Add 'alpha = 0.8' for readability (runs very slow)
  scale_colour_manual(values = c(col_F, col_M)) +
  labs(tag = "(b)") +
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
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.tag = element_text(size = 18)) +
  annotate("point", shape = 17, (colony.df$Lon + 100), colony.df$Lat) +
  annotate("text", label = "Crozet", colony.df$Lon, (colony.df$Lat - 65000))

#### FIGURE S1 - Map of GPS fixes and decision points --------------------------

tiff("Figures/FIGS1_gps-map.tif", width = 15, height = 7, units = "in", res = 350)
ggarrange(gps_map, points_map, ncol = 2)
dev.off()

####  FIGURE S3 - Two examplar looping tracks ----------------------------------

### Bird 1 - 201352 - Male
bird1_plot <- ggplot(data = world2) + 
  geom_sf(fill = "cadetblue", colour = "grey") +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25, style = "bar") +
  coord_sf(crs = proj.utm, xlim = c(-3100000, 3200000), ylim = c(-2500000,2500000),
           label_axes = list(top = "E", left = "N", bottom = "E", right = "N")) +
  geom_path(aes(x = x_lon, y = y_lat), size = 1, dat = subset(gpsDat.df, BirdID == "201352")) + 
  labs(tag = "(a)") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "grey80"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x.bottom = element_blank(), 
        axis.title.x.bottom = element_blank(),
        axis.text.y.right = element_blank(), 
        axis.title.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        plot.tag = element_text(size = 18)) +
  annotate("point", shape = 17, (colony.df$Lon + 100), colony.df$Lat) +
  annotate("text", label = "Crozet", colony.df$Lon, (colony.df$Lat + 145000))


### Bird 2 - 201355 - Female
bird2_plot <- ggplot(data = world2) + 
  geom_sf(fill = "cadetblue", colour = "grey") +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25, style = "bar") +
  coord_sf(crs = proj.utm, xlim = c(-3100000, 3200000), ylim = c(-2500000,2500000),
           label_axes = list(top = "E", left = "N", bottom = "E", right = "N")) +
  geom_path(aes(x = x_lon, y = y_lat), size = 1, dat = subset(gpsDat.df, BirdID == "201355")) + 
  labs(tag = "(b)") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "grey80"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x.bottom = element_blank(), 
        axis.title.x.bottom = element_blank(),
        axis.title.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.left = element_blank(),
        plot.tag = element_text(size = 18)) +
  annotate("point", shape = 17, (colony.df$Lon + 100), colony.df$Lat) +
  annotate("text", label = "Crozet", colony.df$Lon, (colony.df$Lat - 115000))

### Output the plot
tiff("Figures/FIGS3_examplar_loop_maps.tif", width = 15, height = 7, units = "in", res = 350)
ggarrange(bird1_plot, bird2_plot, ncol = 2, nrow = 1)
dev.off()
