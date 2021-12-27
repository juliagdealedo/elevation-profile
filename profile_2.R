# Script to elevation profile
# Data from IGN - http://www.ign.es/web/ign/portal 
# Dic 2021
# Julia G. de Aledo


# Packages

library(sp)
library(rgdal)
library(raster)
library(mapmisc)
library(RColorBrewer)
library(cartography)
library(sf)
library(spatstat)
library(ggplot2)
library(maptools)
library(tidyr)
library(ggthemes)
library(ggtext)
library(MetBrewer) # https://github.com/BlakeRMills/MetBrewer 
library(stringr)
library(sysfonts)
library(showtext)
library(ggrepel)
library(patchwork)  

# Add fonts
font_paths()
pia <- font_files() %>% tibble() %>% filter(str_detect(family, "Piazz"))
font_add(family = "piazzolla", regular="Piazzolla-ExtraLight.ttf")
showtext_auto()

# A. Prepare the data ####

setwd("your_working_directory")
dir()

# 1. Contour world ountries 
countries <- readOGR("TM_WORLD_BORDERS-0.3.shp", stringsAsFactors = TRUE) 
Spain <- subset(countries, name == "Spain")
e2 <- extent(-15,5,30,50) # To crop Canary islands out
Spain_2 <- crop(Spain, e2)
r0 <- raster("combinado_ASC.asc") # elevation from IGN

# 2. Line 
setwd("your_working_directory")
line <- readOGR("./", "Elevacion_Julia") # Line from Asturias to Granada obtained in GIS
ex <- extent(line) # Max and min values of the line to dataframe
my_ex <- data.frame(id = 1:3, lon_2 = ex[1], lon_1 =ex[2], lat_1 =ex[3], lat_2 = ex[4])
# Obtain line elevation values
line2 <- spTransform(line, "+init=epsg:25830")
psp <- as.psp.SpatialLines(line2)
pts <- pointsOnLines(psp, eps=10)
points(pts)
# Convert to spatial points
pts1 <- SpatialPoints(coords = cbind(pts$x, pts$y), proj4string = CRS("+init=epsg:25830"))
pdf_data <- as.data.frame(pts1)
pdf_data$dem <- raster::extract(r0, pts1)
pdf_data$dist <- seq(0, (nrow(pdf_data)-1)*10, 10)
pdf_data$dist <- pdf_data$dist/1000 # to km
# Create labels data frame
labe <- as.data.frame(cbind(c("2289.789 50.94","1844.025 298.43","1109.351 468.89","1205.365 567.89","1761.537 644.61","3089.4 718.4"),
                            c("Picos de Europa", "Sierra de Guadarrama", "Montes de Toledo", "Despeñaperros", "Parque Natural Sierra Mágina", "Sierra Nevada")))
colnames(labe) <- c("uni", "lab")
pdf_data$uni <- paste(round(pdf_data$dem,3), pdf_data$dist)
pdf_labe2 <- merge(pdf_data, labe, by="uni", all=T)


# B. Plot it ####

# 1. Plot elevation and distance
g <- ggplot(data = pdf_labe2, aes(dist, dem, color = dem, label=lab)) +
  geom_line(size=1)+
  scale_color_gradientn(colors = rev(MetPalettes$Hokusai[[1]])) +
  labs(
    title = " \n SPAIN ELEVATION PROFILE",
    subtitle = " \n From north (Asturias) to south (Granada) \n",
    caption = "Source: IGN · Graphic: Julia G. de Aledo",
    y = "Elevation [m]", x="Distance along profile [km]",  color = "Elevation [m]") 

# 2. Plot labels
g1 <- g + geom_label_repel(fill = NA,   xlim = c(-Inf, 700), ylim = c(-Inf, Inf), min.segment.length = 0, seed = 42, 
                           box.padding = 1.3, label.padding=0.3, family="piazzolla", segment.colour = "white", colour="white", 
                           force_pull=2, na.rm=T, direction="both", nudge_y=9, nudge_x=9) +
  theme_solarized(light=F, base_family="piazzolla", base_size = 12) +
  theme (plot.title = element_text(color="white", size=40, face="bold"),
         plot.subtitle = element_text(size=13) ,
         panel.grid.major = element_line(size = 0.01, linetype = 'solid',colour = "white") ) +
  annotate("text", label = "Celorio", x = 4, y = -10, size = 4, colour = "white", family="piazzolla")+
  annotate("text", label = "Playa La Mamola",x = 740, y = -10, size = 4, colour = "white", family="piazzolla")

# 3. Plot map contour and line
g_contour <- ggplot() +
  geom_polygon(data = Spain_2, aes(x = long,  y = lat, group=group), fill = "#002b36", colour = "white",  size=0.5) + theme_void() + 
  geom_segment(data = my_ex, aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2), 
               color = "white", size = 1, alpha = 0.8, lineend = "round")

# 4. Join both plots with tha map to the right up side
g3 <- g1 + inset_element(g_contour, 0.65, 1.03, 0.8, 1.23)

g3

# 5. Save it
ggsave ("profile_1.png", g3, width = 15, height = 10, scale=0.9)

# enjoy!


