rm(list = ls())

library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(classInt)
library(colorspace)
library(grid)
library(png)
library(jpeg)
library(magick)
library(grDevices)  # For color adjustment
library(scales)


building <- st_read("data/LI_BUILDING_FOOTPRINTS/LI_BUILDING_FOOTPRINTS.shp")%>%
  st_transform('EPSG:2272')

# Define center coordinates and radius (in the same CRS as your buildings)
# 北纬39°57'，西经75°10'
center_x <- -75.163
center_y <- 39.955
center <- st_sfc(st_point(c(center_x, center_y)), crs = 4326)%>% 
  st_transform('EPSG:2272')
radius <- 10000  # Set the radius in the same units as your building data

# Create a circular polygon using st_buffer
circle <- st_buffer(center, dist = radius)
# st_write(circle, "data/circle.shp", delete_dsn = TRUE)

# Crop the buildings with the circle
buildings_within_circle <- st_intersection(building, circle)%>%
  mutate(MAX_HGT = ifelse(is.na(MAX_HGT) | MAX_HGT == 0, -1, MAX_HGT))

# # 查看五分位数分隔值
# #   3   27   34   38   43  982 
# quantiles <- quantile(buildings_within_circle$MAX_HGT[buildings_within_circle$MAX_HGT > 0], probs = seq(0, 1, by = 0.2), na.rm = TRUE)
# print(quantiles)

# 使用 Jenks 分级法将 MAX_HGT 分为五个区间
custom_breaks <- c(-1, 0, 27, 35, 40, 50, Inf)

# jenks_breaks <- classIntervals(buildings_within_circle$MAX_HGT, n = 5, style = "jenks")$brks

# 创建自定义复古色渐变，从低饱和度蓝到红
colors <- c("gray90", "#89A1B6", "#8AA98A", "#F7DD8E", "#EA8F68", "#B24E4E") # "#89A1B6", "#A1B7A1", "#E6CEAC", "#E9A38B", "#D27F68"

# 调整亮度来加深颜色
darker_colors <- darken(colors, amount = 0.3)  # `amount` 越大颜色越深


# Philadelphia landuse
# Data Source: https://opendataphilly.org/datasets/land-use/
phlLanduse <- st_read("data/Land_Use/Land_Use.shp")%>%
  st_transform('EPSG:2272')

phlWater <- phlLanduse %>%
  filter(C_DIG1 == 8) %>%
  select(geometry)%>%
  mutate(landuse = "water")

# Delaware water
# Data Source: https://www.nj.gov/drbc/basin/map/GIS.html
delaware_river <- st_read("Data/Delawarewater/delawareriver.shp")%>%
  st_transform('EPSG:2272')

phlWater_circle <- st_intersection(phlWater, circle)
delaware_river_circle <- st_intersection(delaware_river, circle)

## Philadelphia neighborhood
## Data Source: https://opendataphilly.org/datasets/philadelphia-neighborhoods/
phlNeighbhd <- st_read("data/philadelphia-neighborhoods/philadelphia-neighborhoods.shp")%>%
  st_transform('EPSG:2272')%>%
  st_union()

# *********************************************************************
# plot circle
p <- ggplot()+
  geom_sf(data = circle, fill = "#ede5da", color = "#BFAE9E")+
  geom_sf(data = buildings_within_circle, aes(fill = cut(MAX_HGT, breaks = custom_breaks, include.lowest = TRUE),color = "transparent")) +
  # 应用复古色调的分级填充颜色
  scale_fill_manual(values = colors) +
  # 隐藏边框颜色
  scale_color_manual(values = "transparent") +
  # geom_sf(data = buildings_within_circle%>%filter(is.na(MAX_HGT)), fill = "gray80", color = "transparent")+

  geom_sf(data = delaware_river_circle, fill = "#4e7f84", color = "transparent")+
  geom_sf(data = phlWater_circle, fill = "#4e7f84", color = "transparent")+

  # coord_sf(xlim = c(2655000, 2755000), ylim = c(202000, 312000), expand = FALSE) +
  theme(
  panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background = element_rect(fill = "transparent", color = NA))

# save image
ggsave(filename = "Day3.png", plot = p, bg = "transparent",
       width = 6, height = 6, units = "in",
       dpi = 600)


# 
img <- readPNG("Day3.png")  # 读取circle buildings图像
# *********************************************************

# *******************************************************************************

# 创建 ggplot 图形

# circle center: 2693651 237087
Xmax = 2693651 + (2693651 - 2655000)
Ymax = 237087 + (237087 - 202000)
# custom_breaks_allbd <- c(0, 27, 31)
# colors_allbd = c("#7A6056", "#b79f87", "#BFAE9E")

p2 <- ggplot()+
  #   annotation_custom(colored_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  # 
  # # Add a semi-transparent overlay with #ede5da color
  #   annotate("rect",  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#ede5da", alpha = 0.6)+
    geom_sf(data = phlNeighbhd, fill = "#ede5da", color = "#BFAE9E")+
    # geom_sf(data = building, aes(fill = cut(MAX_HGT, breaks = custom_breaks_allbd, include.lowest = TRUE), color = "transparent", alpha = 0.7)) +
    # scale_fill_manual(values = colors_allbd) +
    # scale_color_manual(values = "transparent") +
    geom_sf(data = building, aes(fill = MAX_HGT), color = "transparent", alpha = 0.7)+
    scale_fill_gradient(low = "#b79f87", high = "#ede5da") +
    # geom_sf(data = building, fill = "#BFAE9E",color = "transparent") +
    # 隐藏边框颜色
    geom_sf(data = building%>%filter(is.na(MAX_HGT)), fill = "gray80", color = "transparent")+

    geom_sf(data = delaware_river, fill = "#BFAE9E", color = "transparent")+ # "#D6C9BC"
    geom_sf(data = phlWater, fill = "#BFAE9E", color = "transparent")+

    coord_sf(xlim = c(2655000, 2755000), ylim = c(189000, 321000), expand = FALSE) +
    annotation_custom(rasterGrob(img, width = unit(5.5, "in"), height = unit(5.5, "in")), 
                    xmin = 2655000, ymin = 202000, xmax = Xmax, ymax = Ymax)+  # coordinate of the center of circle

    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = "#ede5da", color = NA),
          )


ggsave(filename = "Day3-2.png", plot = p2, 
       width = 8.5, height = 11, units = "in",  
       dpi = 600) 
system2("open", "Day3-2.png")


# # 原始颜色
# original_color <- "#ede5da"
# 
# # 饱和度降低 50%
# less_bright_color <- desaturate(original_color, amount = 2)

