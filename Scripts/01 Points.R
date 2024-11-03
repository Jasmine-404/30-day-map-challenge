rm(list = ls())

library(tidycensus)
library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(ggforce)

census_api_key("YOUR API KEY", overwrite = TRUE) # type your api key here

tracts22 <- 
  get_acs(geography = "tract", 
          variables = c("B03002_001",  # total
                        "B03002_003",  # white
                        "B03002_004",  # black
                        "B03002_006",   # asian
                        "B03002_012" # hispanic
                        ), 
          year=2022, 
          state = "PA", 
          county = "Philadelphia", 
          geometry=TRUE, 
          output="wide") %>%
          st_as_sf(crs = 4326)%>%
          st_transform('EPSG:2272')%>%
  rename(Total = B03002_001E,
         White = B03002_003E,
         Black = B03002_004E,
         Asian = B03002_006E,
         Hispanic = B03002_012E)%>%
  dplyr::select(-ends_with("M"))%>%
  mutate(others = Total - White - Black - Asian - Hispanic)

# Delaware water
# Data Source: https://www.nj.gov/drbc/basin/map/GIS.html
delaware_river <- st_read("Data/Delawarewater/delawareriver.shp")%>%
  st_transform('EPSG:2272')

# Philadelphia landuse
# Data Source: https://opendataphilly.org/datasets/land-use/
phlLanduse <- st_read("Data/Land_Use/Land_Use.shp")%>%
  st_transform('EPSG:2272')

phlWater <- phlLanduse %>%
  filter(C_DIG1 == 8) %>%
  select(geometry)%>%
  mutate(landuse = "water")


# Create a function to generate random dots
generate_random_points <- function(tract_geom, n_points, color) {
  # Generate points randomly, ensuring that the points are within the given geometric area
  points <- st_sample(tract_geom, size = n_points, type = "random")
  points_df <- data.frame(st_coordinates(points))
  points_df$color <- color
  return(points_df)
}

# Initialize an empty data frame to store all points
points_df <- data.frame()

# define the color of each race
colors <- c("Asian" = "#FF5700", "Black" = "#FFFF00", "Hispanic" = "#57FF00", "White" = "#73E0FF", "others" = "#FFAA00")

# Process each tract
for (i in 1:nrow(tracts22)) {
  tract <- tracts22[i, ]
  
  # calculate the number of points of each race (1 point = 30 people)
  for (race in names(colors)) {
    count <- tract[[race]]
    n_points <- round(count / 30)  
    
    if (n_points > 0) {
      # Generate random points and store them in points_df
      race_points <- generate_random_points(tract$geometry, n_points, colors[race])
      points_df <- rbind(points_df, race_points)
    }
  }
}

# transform points_df into sf object
points_sf <- st_as_sf(points_df, coords = c("X", "Y"), crs = st_crs(tracts22))
points_sf<-points_sf%>%
  mutate(X = st_coordinates(points_sf)[,1])%>%
  mutate(Y = st_coordinates(points_sf)[,2])

# legend circle
circle_data <- data.frame(
  lon = c(2712016, 2726039, 2740062, 2712016, 2726039),  # 圆心的 x 坐标
  lat = c(220700, 220700, 220700, 213942, 213942),       # 圆心的 y 坐标
  size = c(5, 5, 5, 5, 5),                     # 圆的大小
  colorC = c("#73E0FF", "#FF5700", "#FFAA00", "#FFFF00", "#57FF00"),   # 圆的颜色
  type = c("White", "Asian", "Others", "Black", "Hispanic")  # 组别
)


# mapping
p<- ggplot() +
  # water data
  geom_sf(data = phlWater, fill = "#202025", color = "transparent")+
  geom_sf(data = delaware_river, fill = "#202025", color = "transparent")+
  # points
  geom_point(data = subset(points_sf, color != "#FF5700"), aes(x = X, y = Y, color = color), size = 0.05) +  
  geom_point(data = subset(points_sf, color == "#FF5700"), aes(x = X, y = Y, color = color), size = 0.05) +  
  geom_sf(data = tracts22, fill = NA, color = "#B5B0B1") +  # border
  # legend circle
  geom_point(data = circle_data, aes(x = lon, y = lat, color = colorC), size = 5) + 
  coord_sf(xlim = c(2655000, 2755000), ylim = c(202000, 312000), expand = FALSE) +
  scale_color_identity() +
  theme_void()+
  theme(plot.background = element_rect(fill = "#4d4d4f"))+
  # top-left note
  annotate("text", x = 2659000, y = 307000, label = "#30 day map challenge | Day1 - Point", 
           hjust = 0, vjust = 1, size = 7, color = "#C1C1C1", fontface = "bold")+
  annotate("text", x = 2659000, y = 301000, label = "Data: 2022 U.S. Census, OpenDataPhilly, Delaware River Basin Commission\nAuthor: Jingmiao Fei  | Tools: R | 11.01.2024", 
           hjust = 0, vjust = 1, size = 4, color = "#C1C1C1")+
  # title
  annotate("text", x = 2710000, y = 239000, label = "Population Distribution\nin Philadelphia", 
           hjust = 0, vjust = 1, size = 8, color = "white", fontface = "bold.italic")+
  # footnote
  annotate("text", x = 2728100, y = 206000, label = "Inspiration: Wanmei Liang", 
           hjust = 0, vjust = 1, size = 4, color = "#C1C1C1")+
  # legend
  annotate("text", x = 2710000, y = 227000, label = "1 dot = 30 people", 
           hjust = 0, vjust = 1, size = 6, color = "#C1C1C1", fontface = "bold.italic")+
  annotate("text", x = 2715000, y = 221500, label = "White           Asian            Others\n\nBlack           Hispanic", 
           hjust = 0, vjust = 1, size = 5, color = "white", fontface = "bold")+
  
  annotation_scale(location = "br", 
                   width_hint = 0.5,
                   style = "bar", 
                   height = unit(0.2, "cm"),
                   pad_x = unit(1.1, "cm"), pad_y = unit(1.4, "cm"),  # padding
                   bar_cols = c("white", "white"),  # fill color
                   text_col = "white"         # text color
                   ) +  
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         height = unit(1.4, "cm"),
                         width = unit(0.8, "cm"),
                         style = north_arrow_orienteering(text_col = "white", text_size = 5),       # 设置文本大小,
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),  # padding
                         )  

# save image
ggsave(filename = "Day1_Point_Population Distribution in Philadelphia.png", plot = p, 
       width = 8.5, height = 9.4, units = "in",  
       dpi = 600)  

