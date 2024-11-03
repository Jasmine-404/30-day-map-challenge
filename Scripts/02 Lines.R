rm(list=ls())

library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(raster)
library(sp)
# library(ggpattern)

# Prepare data

## Delaware water
## Data Source: https://www.nj.gov/drbc/basin/map/GIS.html
delaware_river <- st_read("Delawarewater/delawareriver.shp")%>%
  st_transform('EPSG:2272')

## Philadelphia neighborhood
## Data Source: https://opendataphilly.org/datasets/philadelphia-neighborhoods/
phlNeighbhd <- st_read("./philadelphia-neighborhoods/philadelphia-neighborhoods.shp")%>%
  st_transform('EPSG:2272')%>%
  st_union()

## DEM data
## Source: https://srtm.csi.cgiar.org/srtmdata/
dem <- raster("./N30W090/cut_n30w090.tif")

## clip DEM data
crop_extent <- extent(-75.5, -74.6, 39.78, 40.85)
dem_crop <- crop(dem, crop_extent)
## project raster
target_crs <- CRS("+init=EPSG:2272")
dem_projected <- projectRaster(dem_crop, crs = target_crs)
## raster to dataframe
dem_df <- as.data.frame(dem_projected, xy = TRUE, na.rm = TRUE)
colnames(dem_df) <- c("x", "y", "elevation")


## Contours
contour_lines <- rasterToContour(dem_crop, levels = seq(minValue(dem_crop), maxValue(dem_crop), by = 30))
contours_sf <- st_as_sf(contour_lines)%>%
  st_transform('EPSG: 2272')
contours_sf$level <- as.numeric(contours_sf$level)

# Mapping
p <- ggplot() +
  # DEM
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation), alpha = 0.3)+
  scale_fill_gradient(low = "transparent", high = "#5C3B31", name = "Elevation") +  # Color scale for DEM
  
  # neighborhood
  geom_sf(data = phlNeighbhd, fill = "#975558", color = "transparent", alpha = 0.1)+
  # stripe pattern
  # geom_sf_pattern(data = phlNeighbhd,
  #                 pattern = "stripe",   
  #                 pattern_density = 0.5,  
  #                 pattern_fill = NA,                # Transparent fill
  #                 pattern_color = "#975558",
  #                 pattern_angle = -45,  
  #                 pattern_size = 0.2,
  #                 color = "transparent",  
  #                 alpha = 0.1) +  

  #contour
  geom_sf(data = contours_sf, aes(color = level), size = 0.3,alpha = 0.3) +
  scale_color_gradient(low = "#BFAE9E", high = "#5C3B31", name = "Contour Level") +
  # delaware river
  geom_sf(data = delaware_river, fill = "#4e7f84", color = "transparent")+
  
  coord_sf(xlim = c(2625000, 2840000), ylim = c(180000, 560000), expand = FALSE) +
  theme(panel.grid = element_blank(),          
        axis.text = element_blank(),           
        axis.ticks = element_blank(),          
        axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "#ede5da", color = NA))+
  # left side note
  annotate("text", x = 2634000, y = 339000, label = "#30 day map challenge | Day2 - Line", 
           hjust = 0, vjust = 1, size = 7, color = "#5C3B31", fontface = "bold", alpha = 0.7)+
  annotate("text", x = 2634000, y = 327000, label = "Data: OpenDataPhilly, SRTM, Delaware River Basin Commission\nAuthor: Jingmiao Fei  | Tool: R | 11.02.2024", 
           hjust = 0, vjust = 1, size = 4, color = "#5C3B31", alpha = 0.7)+
  # title
  annotate("text", x = 2700000, y = 520000, label = "The Delaware River", 
           hjust = 0, vjust = 1, size = 12, color = "#5C3B31", fontface = "bold.italic", alpha = 0.7)+
  # Philadelphia label
  annotate("text", x = 2672800, y = 225000, label = "Philadelphia", 
           hjust = 0, vjust = 1, size = 4, color = "#794844", alpha = 0.6, fontface = "italic")+
  # introduction of delaware river
  annotate("text", x = 2715000, y = 488000, label = "The Delaware River is a 330-mile waterway. \n      As the longest undammed free-flowing\n         river in the Eastern U.S., it forms the\n            border between Pennsylvania and \n              New Jersey. \n               It is vital for local ecosystems and\n               communities.", 
           hjust = 0, vjust = 1, size = 5, color = "#5C3B31", alpha = 0.7, fontface = "bold.italic")+
  annotate("text", x = 2760000, y = 428500, label = "(Delaware River Basin Commission)", 
           hjust = 0, vjust = 1, size = 4, color = "#5C3B31", alpha = 0.7)+       
  annotation_scale(location = "br", 
                   width_hint = 0.5,
                   style = "bar", 
                   height = unit(0.2, "cm"),
                   pad_x = unit(1, "cm"), pad_y = unit(1, "cm"),  # padding
                   bar_cols = c("#DFD3CB", "#DFD3CB"),  # fill color
                   text_col = "#5C3B31"         # text color
  ) +  
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         height = unit(1.4, "cm"),
                         width = unit(0.8, "cm"),
                         style = north_arrow_orienteering(text_col = "#5C3B31", text_size = 8),       
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),  # padding
  )  

# Save image
ggsave(filename = "./output/Day2_Line_The Delaware River.png", plot = p, 
       width = 7.5, height = 13, units = "in",  
       dpi = 600)  