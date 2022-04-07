#' *AIM: Plot GPS tracks separated by sex*

# Load packages
packages <- c("raster", "rgdal", "rnaturalearth", "rnaturalearthdata", "ggplot2", "ggspatial")

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))


# Load data and set map projections ---------------------------------------

gpsDat <- read.csv("Data_inputs/WAAL_GPS_2013_HMM-classification.csv")

## Set projection of GPS data
proj.dec <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj.utm <- "+proj=laea +lat_0=-46.358639 +lon_0=51.706972 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

gpsDat.proj <- gpsDat
coordinates(gpsDat.proj) <- ~ x_lon+y_lat
proj4string(gpsDat.proj) <- proj.dec

gpsDat.sp <- spTransform(gpsDat.proj, CRS(proj.utm))
gpsDat.df <- as.data.frame(gpsDat.sp)

## Colony projection (for plotting)
colony <- data.frame(Lon = 51.706972, Lat = -46.358639)

coordinates(colony) <- ~Lon+Lat
proj4string(colony) <- proj.dec
colony.sp <- spTransform(colony, CRS(proj.utm))
colony.df <- as.data.frame(colony.sp)

## Project back to original coordinates
gpsDat.sp2 <- spTransform(gpsDat.sp, CRS(proj.dec))
gpsDat.df2 <- as.data.frame(gpsDat.sp2)



# Build the plot ----------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")
world2 <- sf::st_transform(world, crs = proj.utm)

col_M <- "#E1BE6A" # sandy yellow
col_F <- "#40B0A6" # turquoise

png("Figures/FIGX_gps-map.png", width = 7, height = 7, units = "in", res = 350)
ggplot(data = world2) + 
  geom_sf(fill = "cadetblue", colour = "grey") +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25, style = "bar") +
  coord_sf(crs = proj.utm, xlim = c(-3100000, 3200000), ylim = c(-2500000,2500000),
           label_axes = list(top = "E", left = "N", bottom = "E", right = "N")) +
  geom_path(aes(x = x_lon, y = y_lat, colour = Sex, group = "identity"), alpha = 0.8, size = 1, dat = gpsDat.df) +
  scale_colour_manual(values = c(col_F, col_M)) +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "grey80"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x.bottom = element_blank(), axis.title.x.bottom = element_blank(),
        axis.text.y.right = element_blank(), axis.title.y.right = element_blank(),
        axis.title.y.left = element_blank()) +
  annotate("point", shape = 17, (colony.df$Lon+100), colony.df$Lat) +
  annotate("text", label = "Crozet", colony.df$Lon, (colony.df$Lat-65000))
dev.off()




