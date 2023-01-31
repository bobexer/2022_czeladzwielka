# Title: Middle Bronze Age settlement practices in Czeladź Wielka.
# Contribution towards settlement archaeology, chronology and production of the Silesian-Greater Poland Tumulus Culture. Script.
# Authors: Jan Romaniszyn, Robert Staniuk, Patrycja Silska, Weronika Skrzyniecka
# For question regarding the code contact R. Staniuk (rstaniuk@gmail.com)
# Date: 2022.12.31

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
library(classInt)
library(reshape2)

# 2. Figures:
## 2.1. Figure 1
czeladz_comparison <- st_read("czeladz_comparison.shp")
czeladz_region_box <- st_as_sfc(st_bbox(czeladz_comparison))
czeladz_comparison <- st_zm(czeladz_comparison)
Europe <- (ne_countries(scale = "medium", type = "map_units", returnclass = "sf"))
map1 = ggplot() +
  geom_sf(data = Europe) +
  geom_sf(data = czeladz_region_box, fill = NA, color = "red") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  coord_sf(xlim = c(-15, 55), ylim = c(35, 73), expand = TRUE)
elevation_data <- get_elev_raster(czeladz_comparison, z = 9, expand = 2) #extract a raster base from the selected data
elevation_data_df <- as.data.frame(elevation_data, xy = TRUE)
colnames(elevation_data_df)[3] <- "elevation" # remove rows of data frame with one or more NA's, using complete.cases
elevation_data_df <- elevation_data_df[complete.cases(elevation_data_df), ]
lakes10 <- st_read("ne_10m_lakes.shp")
rivers10 <- st_read("ne_10m_rivers_lake_centerlines.shp")
map2 = ggplot() +
  geom_raster(data = elevation_data_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradientn(colours=c("#91cf60","#ffffbf","#fc8d59")) +
  geom_sf(data = rivers10, col = "lightblue") +
  geom_sf(data = lakes10, col = "lightblue") +
  geom_sf_text(data = rivers10, aes(label = name), col = "lightblue") +
  geom_sf(data = czeladz_comparison, aes(col = type), size = 4) +
  geom_sf_text(data = czeladz_comparison, aes(label = Figure1),
               nudge_x = c(-0.025, 0.025, 0.025),
               nudge_y = c(-0.025, 0.025, 0.025)) +
  coord_sf(xlim = st_bbox(czeladz_comparison$geometry)[c('xmin','xmax')], ylim = st_bbox(czeladz_comparison$geometry)[c('ymin','ymax')]) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("MBA sites in the Silesian-Greater Poland borderland") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "white"))
Figure1 = ggdraw() +
  draw_plot(map2) +
  draw_plot(map1, x = 0.75, y = 0.7, width = 0.25, height = 0.25)
ggsave(filename = "Figure 1.pdf",
       plot = Figure1,
       width = 14,
       height = 10,
       dpi = 300)

## 2.2. Figure 2
ortho1 <- stack(paste0("ortho1.tiff"))
ortho_extent <- shapefile("site_extent.shp")
ortho1 <- crop(ortho1, extent(ortho_extent))
ortho1 <- mask(x = ortho1, mask = ortho_extent)
ortho1 <- as.data.frame(ortho1, xy = TRUE)
ortho1 <- ortho1 %>% rename(Red = ortho1_1,
                            Green = ortho1_2,
                            Blue = ortho1_3)
ortho1 <- ortho1 %>% filter(Red !=0)
czeladzlayer <- st_read("czeladz_layer.shp")
czeladzfeature <- st_read("czeladz_feature.shp")
czeladzfeature <- st_transform(czeladzfeature, crs = 2180)
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
Figure2 <- grid.arrange(map3, map4, ncol = 2, top = textGrob("Czeladź Wielka", gp = gpar(fontsize = 30)))
ggsave(filename = "Figure 2.pdf",
       plot = Figure2,
       width = 20,
       height = 10,
       dpi = 300)

## 2.3. Figure 4
czeladz_dataset <- read.csv("czeladz_dataset.csv", encoding = "UTF-8", dec = ".")
LipTypes <- sort(unique(czeladz_dataset$TypeLip))
czeladz_dataset$TypeLip <- factor(czeladz_dataset$TypeLip,levels = LipTypes)
graph1 <- ggplot(subset(czeladz_dataset, !is.na(czeladz_dataset$TypeLip)), aes(x = TypeLip)) +
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

ggsave(filename = "Figure 4.pdf",
       plot = Figure4,
       width = 20,
       height = 11,
       dpi = 300)

## 2.4. Figure 5
fabric <- czeladz_dataset %>%
  count(Inclusions, InclusionsSize, InclusionsVisible) %>%
  na.omit() %>%
  rowid_to_column(var='FabricID')

Figure5_1 <- ggplot(fabric, aes(x = as.factor(FabricID), y = n)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("a) Fabrics [n=939]") +
  ylab("frequency") + 
  xlab("fabric") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

WallThickness <- czeladz_dataset[c(25:32)]
WallThickness$row_mean <- rowMeans(WallThickness, na.rm = TRUE)
Figure5_2 <- ggplot(WallThickness, aes(row_mean))+
  geom_density(aes(y=..density..), binwidth = 1, position="identity", colour="black", fill="grey") +
  geom_vline(xintercept = 7.894, linetype = "dashed", size = 1) +
  labs(title = "b) Wall thickness [n=927]", x = "thickness [mm]", y = "density") +
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom", legend.title = element_blank())

Figure5_3 <- ggplot(subset(czeladz_dataset, !is.na(SurfaceExterior)),aes(x = SurfaceExterior))+
  geom_bar(stat = "count", position = "dodge")+
  ggtitle("c) Exterior surface [n = 941]") +
  ylab("frequency") + 
  xlab("surface treatment") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

Figure5_4 <- ggplot(subset(czeladz_dataset, !is.na(SurfaceInterior)),aes(x = SurfaceInterior))+
  geom_bar(stat = "count", position = "dodge")+
  ggtitle("d) Interior surface [n = 941]") +
  ylab("frequency") + 
  xlab("surface treatment") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

Figure5_5 <- ggplot(subset(czeladz_dataset, !is.na(Firing)),aes(x = Firing))+
  geom_bar(stat = "count", position = "dodge")+
  ggtitle("e) Firing [n = 941]") +
  ylab("frequency") + 
  xlab("firing") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

Figure5 <- grid.arrange(Figure5_1, Figure5_2, Figure5_3, Figure5_4, Figure5_5, nrow = 5)
ggsave(filename = "Figure 5.pdf",
       plot = Figure5,
       width = 10,
       height = 11,
       dpi = 300)

## 2.5. Figure 6
decorativetechnique <- melt(czeladz_dataset, id=c("ID"), measure.vars = c("DecoratedTechA", "DecoratedTechB", "DecoratedTechC"))
Figure6_1 <- ggplot(subset(decorativetechnique, !is.na(decorativetechnique$value)), aes(x = value))+
  geom_bar(stat = "count", position = "stack")+
  ggtitle("a) Decorative technique [n = 412]") +
  ylab("frequency")+ 
  xlab("decorative technique")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

decorativeelement <- melt(czeladz_dataset, id=c("ID"), measure.vars = c("DecoratedTechEleA", "DecoratedTechEleB", "DecoratedTechEleC"), na.rm = TRUE)
Figure6_2 <-ggplot(subset(decorativeelement, !is.na(value)), aes(x = value)) +
  geom_bar(stat = "count", position = "stack") +
  ggtitle("b) Decorative elements [n=339]") +
  ylab("frequency") + 
  xlab("decorative element") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

decorativemotif <- melt(czeladz_dataset, id=c("ID"), measure.vars = c("DecoratedMotifGroup"))
Figure6_3 <- ggplot(subset(decorativemotif, !is.na(value)), aes(x = value))+
  geom_bar(stat = "count", position = "dodge")+
  ggtitle("c) Motif groups [n=314]")+
  ylab("frequency")+ 
  xlab("size group")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

Figure6 <- grid.arrange(Figure6_1, Figure6_2, Figure6_3, nrow = 3)
ggsave(filename = "Figure 6.pdf",
       plot = Figure6,
       width = 13,
       height = 11,
       dpi = 300)

## 2.6. Figure 8
czeladzfeaturesanalysis <- merge(czeladzfeature, czeladz_dataset, by.x = "Name", by.y = "OrgID")
classes <- classIntervals(czeladzfeaturesanalysis$SherdNumber, n = 4, style = "fisher")
classes$brks
czeladzfeaturesanalysis <- czeladzfeaturesanalysis %>%
  mutate(SherdNumber_class = cut(SherdNumber, classes$brks, include.lowest = T))

Figure8 <- ggplot(data = czeladzlayer) +
  geom_sf(fill = "grey") +
  geom_sf(data = czeladzfeaturesanalysis, aes(fill = SherdNumber_class)) +
  geom_sf(data = czeladztrench, fill = "white", alpha = 0.01) +
  guides(fill=guide_legend(title="Sherds per feature")) + 
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Sherds per feature") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "white"))
ggsave(filename = "Figure 8.pdf",
       plot = Figure8,
       width = 8,
       height = 5,
       dpi = 300)

## 2.7. Figure 9
czeladzlayergridanalysis <- merge(czeladztrench, czeladz_dataset, by.x = "name", by.y = "GridID")
classeslayergridanalysis <- classIntervals(czeladzlayergridanalysis$SherdNumber, n = 4, style = "fisher")
classeslayergridanalysis$brks
czeladzlayergridanalysis <- czeladzlayergridanalysis %>%
  mutate(SherdNumber_class = cut(SherdNumber, classeslayergridanalysis$brks, include.lowest = T))
czeladzlayergridanalysis

Figure9 <- ggplot(data = czeladzlayergridanalysis) +
  geom_sf(aes(fill = SherdNumber_class)) +
  geom_sf(data = czeladztrench, fill = "white", alpha = 0.01) +
  geom_sf(data = czeladzlayer, fill = "grey", alpha = 0.01) +
  xlab("Longitude") +
  ylab("Latitude") +
  guides(fill=guide_legend(title="Sherds per grid")) + 
  ggtitle("Sherds per grid") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "white"))
ggsave(filename = "Figure 9.pdf",
       plot = Figure9,
       width = 8,
       height = 5,
       dpi = 300)
