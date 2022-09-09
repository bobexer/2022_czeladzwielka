# Title: Middle Bronze Age settlement practices in Czeladź Wielka.
# Contribution towards settlement archaeology, chronology and production of the Silesian-Greater Poland Tumulus Culture. Script.
# Authors: Jan Romaniszyn, Robert Staniuk, Patrycja Silska, Weronika Skrzyniecka
# Date: 2022-09-09

# 1. Packages:
library(tidyverse)
library(sf)
library(tmap)
library(ggspatial)

# 2. Figures:
# 2.1. Figure 1
# 2.2. Fiture 2
czeladztrench <- st_read("czeladz_trench.shp")
czeladzlayer <- st_read("czeladz_layer.shp")
czeladzfeature <- st_read("czeladz_feature.shp")
czeladzfeature = st_transform(czeladzfeature, "EPSG:2180")
ggplot(data = czeladzlayer) +
  geom_sf(fill = "grey") +
  geom_sf(data = czeladzfeature, aes(fill = phase)) +
  geom_sf(data = czeladztrench, fill = "white", alpha = 0.01) + 
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Czeladź Wielka", subtitle = "Feature distribution") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "white"))
