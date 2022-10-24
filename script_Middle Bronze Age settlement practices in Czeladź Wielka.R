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
library(grid)
library(gridExtra)
library(magick)
library(data.table)
library(ggridges)
library(oxcAAR)
library(data.table)

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
ortho1 <- stack(paste0("75106_1053101_M-33-22-A-d-1-1.tif"))
ortho_extent <- shapefile("site_extent.shp")
ortho1 <- crop(ortho1, extent(ortho_extent))
ortho1 <- mask(x= ortho1, mask=ortho_extent)
ortho1 <- as.data.frame(ortho1, xy = TRUE)
ortho1 <- ortho1 %>% rename(Red = X75106_1053101_M.33.22.A.d.1.1.1,
                            Green = X75106_1053101_M.33.22.A.d.1.1.2,
                            Blue = X75106_1053101_M.33.22.A.d.1.1.3)
ortho1 <- ortho1 %>% filter(Red !=0)
czeladzlayer <- st_read("czeladz_layer.shp")
czeladzfeature <- st_read("czeladz_feature.shp")
czeladztrench <- st_read("czeladz_trench.shp")
map3 = ggplot() +
  geom_raster(data = ortho1, aes(x = x, y = y, fill = rgb(r = Red, g = Green, b = Blue, maxColorValue = 255)), show.legend = FALSE) +
  geom_sf(data = czeladztrench, alpha = 0.2) +
  geom_sf(data = czeladzlayer, alpha = 0.2) +
  geom_sf(data = czeladzfeature, col = "black") +
  scale_fill_identity() +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Excavated area") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "white"))
czeladzfeature$phase <- factor(czeladzfeature$phase, levels = c("MN", "MBA", "MIA", "modern", "uncertain"))
map4 = ggplot() +
  geom_sf(data = czeladzlayer, fill = "gray") +
  geom_sf(data = czeladzfeature, aes(fill = phase)) +
  geom_sf(data = czeladztrench, fill = "white", alpha = 0.01) + 
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Feature distribution") +
  theme(legend.position="bottom") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                        size = 0.5), panel.background = element_rect(fill = "white"))
Figure2<-grid.arrange(map3, map4, nrow = 2, top = textGrob("Czeladź Wielka", gp = gpar(fontsize = 30, font = 1)))
ggsave(filename = "Figure 2.jpeg",
       plot = Figure2,
       width = 20,
       height = 30,
       dpi = 300)

## 2.3. Figure 4
czeladz_dataset <- read.csv("czeladz_dataset.csv", encoding = "UTF-8", dec = ".")
LipTypes <- sort(unique(czeladz_dataset$TypeLip))
czeladz_dataset$TypeLip <- factor(czeladz_dataset$TypeLip,levels = LipTypes)
graph1 <- ggplot(subset(czeladz_dataset, !is.na(TypeLip)), aes(x = TypeLip)) +
  geom_bar(stat = "count", position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  ggtitle("Czeladź Wielka: lip type [n=369]") +
  ylab("Frequency") + 
  xlab("Lip type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(5, 5, 0, 5), "pt"))

image1 <- image_read("czeladz_lips.png")
graph1_image1 <- image1 %>%
  image_scale("2000") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "100")
graph1_image1 <- rasterGrob(graph1_image1)
Figure4 <- grid.arrange(graph1, graph1_image1, nrow = 2, widths = c(1))
Figure4

ggsave(filename = "Figure 4.png",
       plot = Figure4,
       width = 20,
       height = 10,
       dpi = 300)

## 2.4. Figure 5

## 2.5. Figure 6

## 2.6. Figure 7

## 2.7. Figure 8

## 2.8. Figure 9

## 2.9. Figure 10
czeladz_c14 <- read.csv("czeladz_c14.csv")
quickSetupOxcal()
czeladz_c14_cal <- oxcalCalibrate(czeladz_c14$estimation, czeladz_c14$std, czeladz_c14$labcode)
czeladz_c14_comparison <- rbindlist(czeladz_c14_cal)
czeladz_c14_comparison

czeladz_c14_comparison <- c("name", "sigma_ranges", "two_sigma", "start", "end")
czeladz_c14_comparison <- lapply(czeladz_c14_cal, function(x) x[["sigma_ranges"]])
test <- as.data.frame(do.call(rbind, czeladz_c14_comparison))
View(test)
test <- rm(one_sigma)

test1<-as.data.frame(test$V1)
test1

czeladz_c14_comparison <- lapply(czeladz_c14_cal, function(x) x[["start"]])
View(czeladz_c14_comparison)
test2 <- as.data.frame(do.call(rbind, czeladz_c14_comparison))
test2

ggplot(czeladz_c14_cal, aes(x = start, y = fct_reorder(site, start), fill = site)) + 
  geom_density_ridges(jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|', point_size = 0.5, point_alpha = 1, alpha = 0.7,) +
  labs(y = "Sites", x = "Age BP") +
  theme_ridges() + 
  theme(legend.position = "none")


end <- c(czeladz_c14$estimation-czeladz_c14$std)
start <-c(czeladz_c14$estimation+czeladz_c14$std)
czeladz_c14 <- cbind(czeladz_c14, start)
czeladz_c14 <- cbind(czeladz_c14, end)
head(czeladz_c14)
czeladz_c14_ranges <- Map(`:`, czeladz_c14$start, czeladz_c14$end)
head(czeladz_c14_ranges)
czeladz_c14 <- transform(czeladz_c14[rep(seq_len(nrow(czeladz_c14)), lengths(czeladz_c14_ranges)), c('labcode', 'site')],
          start = unlist(czeladz_c14_ranges))
Figure10 <- ggplot(czeladz_c14, aes(x = start, y = fct_reorder(site, start), fill = site)) + 
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