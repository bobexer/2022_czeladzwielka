# Title: Middle Bronze Age settlement practices in Czeladź Wielka.
# Contribution towards settlement archaeology, chronology and production of the Silesian-Greater Poland Tumulus Culture. Script.
# Authors: Jan Romaniszyn, Robert Staniuk, Patrycja Silska, Weronika Skrzyniecka
# Date: 2022-09-09

# 1. Packages:
library(tidyverse)
library(sf)
library(rnaturalearth)
library(elevatr)
library(raster)
library(cowplot)
library(ggspatial)
library(grImport)
library(gridExtra)
library(data.table)
library(ggridges)

# 2. Figures:
## 2.1. Figure 1
czeladz_region <- read.csv("czeladz_region.csv", encoding="UTF-8", dec=".")
czeladz_region <- st_as_sf(czeladz_region, coords = c("xcoord", "ycoord"), crs = 2180)
czeladz_region_box <- st_as_sfc(st_bbox(czeladz_region))
Europe <- (ne_countries(scale = "medium", type = "map_units", returnclass = "sf"))
map1 = ggplot() +
  geom_sf(data = Europe) +
  geom_sf(data = czeladz_region_box, fill = NA, color = "red") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  coord_sf(xlim = c(-15, 55), ylim = c(35, 73), expand = TRUE)

elevation_data <- get_elev_raster(czeladz_region, z = 9, expand = 2) #extract a raster base from the selected data
elevation_data_df <- as.data.frame(elevation_data, xy = TRUE)
colnames(elevation_data_df)[3] <- "elevation" # remove rows of data frame with one or more NA's, using complete.cases
elevation_data_df <- elevation_data_df[complete.cases(elevation_data_df), ]
rivers <- st_read("eurriver.shp")
rivers = st_transform(rivers, "EPSG:2180")
map2 = ggplot() +
  geom_raster(data = elevation_data_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradientn(colours=c("#91cf60","#ffffbf","#fc8d59")) +
  geom_sf(data = rivers, col = "lightblue", lwd = 1.1) +
  geom_sf(data = czeladz_region, aes(col = C14, shape = type), size = 4) +
  geom_sf_text(data = czeladz_region, aes(label = Figure1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("MBA sites in the Silesian-Greater Poland borderland") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "white")) +
  coord_sf(xlim = st_bbox(czeladz_region$geometry)[c('xmin','xmax')], ylim = st_bbox(czeladz_region$geometry)[c('ymin','ymax')])

gg_Figure1 = ggdraw() +
  draw_plot(map2) +
  draw_plot(map1, x = 0.8, y = 0.7, width = 0.2, height = 0.2)
ggsave(filename = "Figure 1.jpeg", 
       plot = gg_Figure1,
       width = 14, 
       height = 10,
       dpi = 300)

## 2.2. Figure 2
czeladztrench <- st_read("czeladz_trench.shp")
czeladzlayer <- st_read("czeladz_layer.shp")
czeladzfeature <- st_read("czeladz_feature.shp")
czeladzfeature = st_transform(czeladzfeature, "EPSG:2180")
ggplot(data = czeladzlayer) +
  geom_sf(fill = "grey") +
  geom_sf(data = czeladzfeature, aes(fill = phase)) +
  geom_sf(data = czeladztrench, fill = "white", alpha = 0.01) + 
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Czeladź Wielka: feature distribution") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "white"))

## 2.3. Figure 4
czeladz_dataset <- read.csv("czeladz_dataset.csv", encoding="UTF-8", dec=".")
LipTypes<-sort(unique(czeladz_dataset$TypeLip))
czeladz_dataset$TypeLip<-factor(czeladz_dataset$TypeLip,levels=LipTypes)
Fig4_1<-ggplot(subset(czeladz_dataset, !is.na(TypeLip)), aes(x=TypeLip))+
  geom_bar(stat="count", position="dodge")+
  geom_text(stat="count", aes(label=..count..), vjust=-0.5)+
  ggtitle("Czeladź Wielka: lip type [n=369]")+
  ylab("frequency")+ 
  xlab("lip type")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
Sys.setenv(R_GSCMD = normalizePath("C:/Program Files/gs/gs9.56.1/bin/gswin64.exe"))
PostScriptTrace("czeladz_lips.eps", "czeladz_lips.xml")
Fig4_2_1 <- readPicture("czeladz_lips.xml")
Fig4_2 <- pictureGrob(Fig4_2_1)
Fig4 <- grid.arrange(Fig4_1, Fig4_2, nrow=2)

## 2.4. Figure 5

## 2.5. Figure 6

## 2.6. Figure 7

## 2.7. Figure 8

## 2.8. Figure 9

## 2.9. Figure 10
czeladz_c14 <- read.csv("czeladz_c14.csv")
end <- c(czeladz_c14$estimation-czeladz_c14$std)
start <-c(czeladz_c14$estimation+czeladz_c14$std)
czeladz_c14 <- cbind(czeladz_c14, start)
czeladz_c14 <- cbind(czeladz_c14, end)
head(czeladz_c14)
czeladz_c14_ranges <- Map(`:`, czeladz_c14$start, czeladz_c14$end)
head(czeladz_c14_ranges)
czeladz_c14 <- transform(czeladz_c14[rep(seq_len(nrow(czeladz_c14)), lengths(czeladz_c14_ranges)), c('labcode', 'site')],
          start = unlist(czeladz_c14_ranges))
Figure10<-ggplot(czeladz_c14, aes(x = start, y = fct_reorder(site, start), fill = site)) + 
  geom_density_ridges(jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|', point_size = 0.5, point_alpha = 1, alpha = 0.7,) +
  labs(y = "Sites", x = "Age BP") +
  theme_ridges() + 
  theme(legend.position = "none")
ggsave(filename = "Figure 10.jpeg", 
       plot = Figure10,
       width = 15, 
       height = 15,
       dpi = 300)
