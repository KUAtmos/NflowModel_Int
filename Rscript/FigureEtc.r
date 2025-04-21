library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(patchwork)
library(cowplot)
#Cropland distribution
#Cropdistribution <- VY_load2[, c(2,3,7,10,11)] %>%
# filter(i2 == 2010) %>%
#  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A")) %>%
#  group_by (i2, lat, lon) %>%
#  summarise(LUAREA = sum(LUAREA, na.rm = TRUE))

Totaldistribution <- VY_load2[, c(2,3,7,10,11)] %>%
  filter(i2 == 2010) %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
  group_by (i2, lat, lon) %>%
  summarise(LUAREA = sum(LUAREA, na.rm = TRUE))


Cropdistribution2 <- Totaldistribution %>%
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25) %>% #ha
  mutate(LUAREA = LUAREA/1000) #kha
Cropdistribution2 <- Cropdistribution2[, c(1,4,5,6)]

custom_colors <- c("#d4e6f1", "#7fb3d5", "#2e86c1", "#f5b041", "#d35400", "#c0392b")

plot1 <- ggplot() +
  geom_tile(data = Cropdistribution2, aes(x = x, y = y, fill = LUAREA)) + 
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.075, 0.15, 0.25, 0.625, 1),
    limits = c(0, 60), 
    breaks = c(0, 15, 30, 45, 60),  
    labels = c("0", "15", "30", "45", "60+"), 
    oob = scales::squish,
    name = "cropland area\n(kha grid^-1 yr^-1)",  
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Cropland Area"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/agrland_2010.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#INfer Map, OUTgasMap
Totaldistribution <- VY_load2[, c(2,3,7,10,11)] %>%
  filter(i2 == 2005) %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
  group_by (i2, lat, lon) %>%
  summarise(LUAREA = sum(LUAREA, na.rm = TRUE))

INferdismap <- INferdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2005)  #Substitute variable
INferdismap <- INferdismap[, c(2,3,4,5)] %>%
  left_join(Totaldistribution, by = c("lat", "lon")) %>% #ha
  mutate(Fert_Use_Flux = Fert_Use_Grid*100000000/LUAREA) %>% #gN/m2/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
INferdismap <- INferdismap[, c(1,7,8,9)] 

OUTgasdismap <- OUTgasdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2005)  #Substitute variable
OUTgasdismap <- OUTgasdismap[, c(2,3,4,5)] %>%
  left_join(gridarea2, by = c("lat", "lon")) %>% #kha
  mutate(OUTgasgrid = OUTgasgrid*1000000/area) %>% #kg/ha/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
OUTgasdismap <- OUTgasdismap[, c(1,2,6,7)]

world <- ne_countries(scale = "medium", returnclass = "sf")

custom_colors <- c("#d4e6f1", "#7fb3d5", "#2e86c1", "#f5b041", "#d35400", "#c0392b")

plot1 <- ggplot() +
  geom_tile(data = INferdismap, aes(x = x, y = y, fill = Fert_Use_Flux)) + 
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.075, 0.15, 0.25, 0.625, 1),
    limits = c(0, 20), 
    breaks = c(0, 5, 10, 15, 20),  
    labels = c("0", "5", "10", "15", "20+"), 
    oob = scales::squish,
    name = "Fertilizer application\n(g N m^-2 agricultural land yr^-1)",  
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Fertilizer Application"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/fertilizer_application_2005.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

plot2 <- ggplot() +
  geom_tile(data = OUTgasdismap, aes(x = x, y = y, fill = OUTgasgrid)) +  
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.025, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1), 
    limits = c(0, 20), 
    breaks = c(0, 5, 10, 15, 20),  
    labels = c("0", "5", "10", "15", "20+"),  
    oob = scales::squish,
    name = "Gaseous Losses\n(kg ha^-1 yr^-1)", 
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Gaseous Losses"
  ) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 15), 
    legend.text = element_text(size = 12),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/outgas_2005.png",  
  plot = plot2,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#INfercropMap
INfercropdismap <- INfercropdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2005)  #Substitute variable
INfercropdismap <- INfercropdismap[, c(2,3,4,5)] %>%
  left_join(Cropdistribution, by = c("lat", "lon")) %>% #ha
  mutate(Fert_Use_Flux = Fert_Use_Grid*100000000/LUAREA) %>% #gN/m2/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
INfercropdismap <- INfercropdismap[, c(1,7,8,9)] 

custom_colors <- c("#d4e6f1", "#7fb3d5", "#2e86c1", "#f5b041", "#d35400", "#c0392b")

plot1 <- ggplot() +
  geom_tile(data = INfercropdismap, aes(x = x, y = y, fill = Fert_Use_Flux)) + 
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.04, 0.07, 0.2, 0.75, 0.9, 1),
    limits = c(0, 25), 
    breaks = c(0, 5, 10, 15, 20, 25),  
    labels = c("0", "5", "10", "15", "20", "25+"), 
    oob = scales::squish,
    name = "Fertilizer application\n(g N m^-2 cropland yr^-1)",  
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Fertilizer Application"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 15),  
    legend.text = element_text(size = 12),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/fertilizer_application_cropland_2005_C_B.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#INfercroptotalMap
INfercroptotaldismap <- INfercropdis %>% #Tg/yr
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2005) %>%  #Substitute variable
  mutate(Fert_Use_Grid = Fert_Use_Grid*1000) #Gg/yr
INfercroptotaldismap <- INfercroptotaldismap[, c(2,3,4,5)] %>%
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
INfercroptotaldismap <- INfercroptotaldismap[, c(1,2,5,6)] 

custom_colors <- c("#d4e6f1", "#7fb3d5", "#2e86c1", "#f5b041", "#d35400", "#c0392b")

plot1 <- ggplot() +
  geom_tile(data = INfercroptotaldismap, aes(x = x, y = y, fill = Fert_Use_Grid)) + 
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.075, 0.15, 0.25, 0.625, 1),
    limits = c(0, 4), 
    breaks = c(0, 1, 2, 3, 4),  
    labels = c("0", "1.0", "2.0", "3.0", "4.0+"), 
    oob = scales::squish,
    name = "Fertilizer application\n(GgN grid^-1 yr^-1)",  
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Fertilizer Application"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/fertilizer_totalapplication_cropland_2005.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#INmancropMap
INmandismap <- INmandis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2005)  #Substitute variable
INmandismap <- INmandismap[, c(1,3,4,5)] %>%
  left_join(Cropdistribution, by = c("lat", "lon")) %>% #ha
  mutate(ManureGrid = ManureGrid*100000000/LUAREA) %>% #gN/m2/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
INmandismap <- INmandismap[, c(1,2,7,8)] 

custom_colors <- c("#d4e6f1", "#7fb3d5", "#2e86c1", "#f5b041", "#d35400", "#c0392b")

plot1 <- ggplot() +
  geom_tile(data = INmandismap, aes(x = x, y = y, fill = ManureGrid)) + 
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.075, 0.15, 0.25, 0.625, 1),
    limits = c(0, 2), 
    breaks = c(0, 0.5, 1, 1.5, 2),  
    labels = c("0", "0.5", "1.0", "1.5", "2.0+"), 
    oob = scales::squish,
    name = "Manure input\n(g N m^-2 agricultural land yr^-1)",  
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Manure Application"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/manure_application_2005.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#OUTgascropMap
OUTgascropdismap <- OUTgascropdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2005)  #Substitute variable
OUTgascropdismap <- OUTgascropdismap[, c(2,3,4,5)] %>%
  left_join(Cropdistribution, by = c("lat", "lon")) %>% #ha
  mutate(OUTgascropgrid = OUTgascropgrid*1000000000/LUAREA) %>% #kg/ha/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
OUTgascropdismap <- OUTgascropdismap[, c(1,2,7,8)]

custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

plot2 <- ggplot() +
  geom_tile(data = OUTgascropdismap, aes(x = x, y = y, fill = OUTgascropgrid)) +  
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.15, 0.30, 0.4, 0.45, 0.65, 0.85, 0.9, 1), 
    limits = c(0, 80), 
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80),  
    labels = c("0", "10", "20", "30", "40", "50", "60", "70", "80+"),  
    oob = scales::squish,
    name = "Gaseous Losses\n(kg ha^-1 cropland yr^-1)", 
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Gaseous Losses"
  ) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/outgas_cropland_2005.png",  
  plot = plot2,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)




#Input Map 2100_500C
Totaldistribution <- VY_load2[, c(2,3,7,10,11)] %>%
  filter(i2 == 2100) %>% #year
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
  group_by (i2, lat, lon) %>%
  summarise(LUAREA = sum(LUAREA, na.rm = TRUE))

Inputdismap <- Inputdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2100)  #Substitute variable
Inputdismap <- Inputdismap[, c(2,3,4,5)] %>%
  left_join(Totaldistribution, by = c("lat", "lon")) %>% #ha
  mutate(Input = Input*100000000/LUAREA) %>% #gN/m2/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
Inputdismap <- Inputdismap[, c(2,7,8)] 

world <- ne_countries(scale = "medium", returnclass = "sf")

breaks_vals <- seq(0, 25, by = 2.5)  # 0, 2.5, 5, ..., 25
labels_vals <- c("0", "2.5", "5", "7.5", "10", "12.5", "15", "17.5", "20", "22.5", "25+")

custom_colors <- c(
  "#ffffff",  # white
  "#deebf7",  # light blue
  "#c7e9c0",  # light green
  "#74c476",  # medium green
  "#ffffb2",  # pale yellow
  "#fecc5c",  # soft orange
  "#fd8d3c",  # orange
  "#f16913",  # moderate orange-red (↓調整)
  "#d94801",  # soft reddish-brown (↓代替)
  "#a63603"   # medium brown (←旧最濃色より薄い)
)

Inputdismap_2100500C <- Inputdismap

plot_input <- ggplot() +
  geom_tile(data = Inputdismap_2100500C, aes(x = x, y = y, fill = Input)) +
  scale_fill_stepsn(
    colors = custom_colors,
    breaks = breaks_vals,
    limits = c(0, 25),
    labels = labels_vals,
    na.value = "white",
    name = "Total N input in 2100\n(g N m⁻² yr⁻¹)",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(12, "cm"),
      barheight = unit(0.2, "cm")  # ← 横棒を細く！
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) +
    labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/Input_500C_2100.png",
  plot = plot_input,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

#Input Map 2100_Base
Totaldistribution <- VY_load2[, c(2,3,7,10,11)] %>%
  filter(i2 == 2100) %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
  group_by (i2, lat, lon) %>%
  summarise(LUAREA = sum(LUAREA, na.rm = TRUE))

Inputdismap <- Inputdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2100)  #Substitute variable
Inputdismap <- Inputdismap[, c(2,3,4,5)] %>%
  left_join(Totaldistribution, by = c("lat", "lon")) %>% #ha
  mutate(Input = Input*100000000/LUAREA) %>% #gN/m2/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
Inputdismap <- Inputdismap[, c(2,7,8)] 

world <- ne_countries(scale = "medium", returnclass = "sf")

breaks_vals <- seq(0, 25, by = 2.5)  # 0, 2.5, 5, ..., 25
labels_vals <- c("0", "2.5", "5", "7.5", "10", "12.5", "15", "17.5", "20", "22.5", "25+")

custom_colors <- c(
  "#ffffff",  # white
  "#deebf7",  # light blue
  "#c7e9c0",  # light green
  "#74c476",  # medium green
  "#ffffb2",  # pale yellow
  "#fecc5c",  # soft orange
  "#fd8d3c",  # orange
  "#f16913",  # moderate orange-red (↓調整)
  "#d94801",  # soft reddish-brown (↓代替)
  "#a63603"   # medium brown (←旧最濃色より薄い)
)

Inputdismap_2100Base <- Inputdismap

plot_input2 <- ggplot() +
  geom_tile(data = Inputdismap_2100Base, aes(x = x, y = y, fill = Input)) +
  scale_fill_stepsn(
    colors = custom_colors,
    breaks = breaks_vals,
    limits = c(0, 25),
    labels = labels_vals,
    na.value = "white",
    name = "Total N input in 2100\n(g N m⁻² yr⁻¹)",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(12, "cm"),
      barheight = unit(0.2, "cm")  # ← 横棒を細く！
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/Input_Base_2100_ver2.png",
  plot = plot_input2,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

#Input Map 2010
Totaldistribution <- VY_load2[, c(2,3,7,10,11)] %>%
  filter(i2 == 2010) %>%
  filter(i3 %in% c("PDRIR", "WHTIR", "GROIR", "OSDIR", "C_BIR", "PDRRF", "WHTRF", "GRORF", "OSDRF", "C_BRF", "OTH_A", "PAS")) %>%
  group_by (i2, lat, lon) %>%
  summarise(LUAREA = sum(LUAREA, na.rm = TRUE))

Inputdismap <- Inputdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2010)  #Substitute variable
Inputdismap <- Inputdismap[, c(2,3,4,5)] %>%
  left_join(Totaldistribution, by = c("lat", "lon")) %>% #ha
  mutate(Input = Input*100000000/LUAREA) %>% #gN/m2/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
Inputdismap <- Inputdismap[, c(2,7,8)] 

world <- ne_countries(scale = "medium", returnclass = "sf")

breaks_vals <- seq(0, 25, by = 2.5)  # 0, 2.5, 5, ..., 25
labels_vals <- c("0", "2.5", "5", "7.5", "10", "12.5", "15", "17.5", "20", "22.5", "25+")

custom_colors <- c(
  "#ffffff",  # white
  "#deebf7",  # light blue
  "#c7e9c0",  # light green
  "#74c476",  # medium green
  "#ffffb2",  # pale yellow
  "#fecc5c",  # soft orange
  "#fd8d3c",  # orange
  "#f16913",  # moderate orange-red (↓調整)
  "#d94801",  # soft reddish-brown (↓代替)
  "#a63603"   # medium brown (←旧最濃色より薄い)
)

Inputdismap_2010 <- Inputdismap

plot_input3 <- ggplot() +
  geom_tile(data = Inputdismap_2010, aes(x = x, y = y, fill = Input)) +
  scale_fill_stepsn(
    colors = custom_colors,
    breaks = breaks_vals,
    limits = c(0, 25),
    labels = labels_vals,
    na.value = "white",
    name = "Total N input in 2100\n(g N m⁻² yr⁻¹)",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(12, "cm"),
      barheight = unit(0.2, "cm")  # ← 横棒を細く！
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/Input_2010.png",
  plot = plot_input3,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


#Aggregate Input Map
plot1_clean <- plot_input + theme(legend.position = "none")
plot2_clean <- plot_input2 + theme(legend.position = "none")
plot3_clean <- plot_input3 + theme(legend.position = "none")
plot1_clean <- plot1_clean +
  annotate("text", x = 0, y = -55, label = "500C 2100", size = 5, fontface = "bold") +
  labs(x = "Longitude", y = "Latitude")

plot2_clean <- plot2_clean +
  annotate("text", x = 0, y = -55, label = "Baseline 2100", size = 5, fontface = "bold") +
  labs(x = "Longitude", y = "Latitude")

plot3_clean <- plot3_clean +
  annotate("text", x = 0, y = -55, label = "2010", size = 5, fontface = "bold") +
  labs(x = "Longitude", y = "Latitude")


legend_shared <- get_legend(plot_input +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))
)

# 各地図に黒枠を追加する関数（大きさはそのまま）
add_black_border_with_axes <- function(p) {
  p +
    theme_bw() +  # ← 軸表示のため minimal から変更
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      panel.grid = element_blank(),  # グリッドは消す
      axis.title = element_text(size = 10),  # 軸タイトル
      axis.text = element_text(size = 8),    # 軸目盛り
      axis.ticks = element_line(size = 0.3), # 軸目盛線
      plot.title = element_blank(),          # 地図タイトル非表示
      plot.margin = margin(1, 1, 1, 1, "mm")
    )
}

# 地図を読み込み済みのオブジェクトに黒枠追加
plot1_bordered <- add_black_border(plot1_clean)
plot2_bordered <- add_black_border(plot2_clean)
plot3_bordered <- add_black_border(plot3_clean)

breaks_vals <- seq(0, 25, by = 2.5)
labels_vals <- c("0", "2.5", "5", "7.5", "10", "12.5", "15", "17.5", "20", "22.5", "25+")

custom_colors <- c(
  "#ffffff",  # white
  "#deebf7",  # light blue
  "#c7e9c0",  # light green
  "#74c476",  # medium green
  "#ffffb2",  # pale yellow
  "#fecc5c",  # soft orange
  "#fd8d3c",  # orange
  "#f16913",  # moderate orange-red
  "#d94801",  # soft reddish-brown
  "#a63603"   # medium brown
)

# 凡例をplot_input3と同様に作成
legend_shared <- get_legend(
  ggplot() +
    geom_tile(data = Inputdismap_2010, aes(x = x, y = y, fill = Input)) +
    scale_fill_stepsn(
      colors = custom_colors,
      breaks = breaks_vals,
      limits = c(0, 25),
      labels = labels_vals,
      na.value = "white",
      name = "Total N input\n(g N m⁻² yr⁻¹)",
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(12, "cm"),
        barheight = unit(0.2, "cm")
      )
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 10)
    )
)
# 上段：2010、下段：Baselineと500C横並び
top_row <- plot3_bordered
bottom_row <- plot_grid(plot2_bordered, plot1_bordered, ncol = 2, rel_widths = c(1, 1))

# 全体を上下に結合
map_grid <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  rel_heights = c(1, 1),
  align = "v"
)

# 最終的に凡例を下に追加
final_plot <- plot_grid(
  map_grid,
  legend_shared,
  ncol = 1,
  rel_heights = c(1, 0.13)
)

# 保存
ggsave("../output/Input_combined_rev.png", plot = final_plot, width = 12, height = 7.5, dpi = 300)






# Gasmap 2010
OUTgasdismap <- OUTgasdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2010)  #Substitute variable
OUTgasdismap <- OUTgasdismap[, c(2,3,4,5)] %>%
  left_join(gridarea2, by = c("lat", "lon")) %>% #kha
  mutate(OUTgasgrid = OUTgasgrid*1000000/area) %>% #kg/ha/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
OUTgasdismap <- OUTgasdismap[, c(1,2,6,7)]
custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

OUTgasdismap_2010 <- OUTgasdismap

plot1 <- ggplot() +
  geom_tile(data = OUTgasdismap_2010, aes(x = x, y = y, fill = OUTgasgrid)) +  
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.025, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1), 
    limits = c(0, 20), 
    breaks = c(0, 5, 10, 15, 20),  
    labels = c("0", "5", "10", "15", "20+"),  
    oob = scales::squish,
    name = "Gaseous Losses\n(kg ha^-1 yr^-1)", 
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Gaseous Losses"
  ) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 15), 
    legend.text = element_text(size = 12),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/outgas_2010.png",  
  plot = plot1,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#outgas 2100 base
OUTgasdismap <- OUTgasdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2100)  #Substitute variable
OUTgasdismap <- OUTgasdismap[, c(2,3,4,5)] %>%
  left_join(gridarea2, by = c("lat", "lon")) %>% #kha
  mutate(OUTgasgrid = OUTgasgrid*1000000/area) %>% #kg/ha/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
OUTgasdismap <- OUTgasdismap[, c(1,2,6,7)]
custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

OUTgasdismap_2100base <- OUTgasdismap

plot2 <- ggplot() +
  geom_tile(data = OUTgasdismap_2100base, aes(x = x, y = y, fill = OUTgasgrid)) +  
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.025, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1), 
    limits = c(0, 20), 
    breaks = c(0, 5, 10, 15, 20),  
    labels = c("0", "5", "10", "15", "20+"),  
    oob = scales::squish,
    name = "Gaseous Losses\n(kg ha^-1 yr^-1)", 
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Gaseous Losses"
  ) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 15), 
    legend.text = element_text(size = 12),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/outgas_Base_2100_ver2.png",  
  plot = plot2,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)

#Outgas 2100 500C
OUTgasdismap <- OUTgasdis %>%
  left_join(MAP_GIJ, by = c("G")) %>%
  filter(YEMF == 2100)  #Substitute variable
OUTgasdismap <- OUTgasdismap[, c(2,3,4,5)] %>%
  left_join(gridarea2, by = c("lat", "lon")) %>% #kha
  mutate(OUTgasgrid = OUTgasgrid*1000000/area) %>% #kg/ha/yr
  mutate(x = 0.5*lon - 180.25, y = -0.5*lat + 90.25)
OUTgasdismap <- OUTgasdismap[, c(1,2,6,7)]
custom_colors <- c("#f7fbff", "#fdae61", "#d7191c")

OUTgasdismap_2100500C <- OUTgasdismap

plot3 <- ggplot() +
  geom_tile(data = OUTgasdismap_2100500C, aes(x = x, y = y, fill = OUTgasgrid)) +  
  scale_fill_gradientn(
    colors = custom_colors,
    values = c(0, 0.025, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1), 
    limits = c(0, 20), 
    breaks = c(0, 5, 10, 15, 20),  
    labels = c("0", "5", "10", "15", "20+"),  
    oob = scales::squish,
    name = "Gaseous Losses\n(kg ha^-1 yr^-1)", 
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = unit(12, "cm"),  
      barheight = unit(0.3, "cm")  
    )
  ) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) + 
  theme_minimal() +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Gaseous Losses"
  ) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 15), 
    legend.text = element_text(size = 12),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
    panel.grid = element_blank()
  )

ggsave(
  filename = "../output/outgas_500C_2100.png",  
  plot = plot3,  
  width = 10,   
  height = 6,   
  dpi = 300,     
  bg = "white"
)


#Aggregate Outgas Map
plot1_clean <- plot1 + theme(legend.position = "none")
plot2_clean <- plot2 + theme(legend.position = "none")
plot3_clean <- plot3 + theme(legend.position = "none")
plot1_clean <- plot1_clean +  # 既存の地図オブジェクト
  annotate("text",
           x = 0, y = -55,  # 緯度経度を調整（x=経度, y=緯度）
           label = "2010",
           size = 5, fontface = "bold")+
  labs(title = NULL)
plot2_clean <- plot2_clean +  # 既存の地図オブジェクト
  annotate("text",
           x = 0, y = -55,  # 緯度経度を調整（x=経度, y=緯度）
           label = "Baseline 2100",
           size = 5, fontface = "bold")+
  labs(title = NULL)
plot3_clean <- plot3_clean +  # 既存の地図オブジェクト
  annotate("text",
           x = 0, y = -55,  # 緯度経度を調整（x=経度, y=緯度）
           label = "500C 2100",
           size = 5, fontface = "bold")+
  labs(title = NULL)



# 各地図に黒枠を追加する関数（大きさはそのまま）
add_black_border <- function(p) {
  p + theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.margin = margin(1, 1, 1, 1, "mm")  # 少しマージン
  )
}

# 地図を読み込み済みのオブジェクトに黒枠追加
plot1_bordered <- add_black_border(plot1_clean)
plot2_bordered <- add_black_border(plot2_clean)
plot3_bordered <- add_black_border(plot3_clean)

legend_shared <- get_legend(
  plot1 +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 12)
    )
)
# 上段：2010、下段：Baselineと500C横並び
top_row <- plot1_bordered
bottom_row <- plot_grid(plot2_bordered, plot3_bordered, ncol = 2, rel_widths = c(1, 1))

# 全体を上下に結合
map_grid <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  rel_heights = c(1, 1),
  align = "v"
)

# 最終的に凡例を下に追加
final_plot <- plot_grid(
  map_grid,
  legend_shared,
  ncol = 1,
  rel_heights = c(1, 0.131)
)

# 保存
ggsave("../output/Outgas_combined.png", plot = final_plot, width = 12, height = 7.5, dpi = 300)



#Agricultural land area(VY_load2 is 500C's)
AIMPLUM_BaU <- "../data/results_SSP2_BaU_NoCC.gdx" #input data
AIMPLUM_BaU_data <- gdxrrw::rgdx(AIMPLUM_BaU)
VY_load <- rgdx.param(AIMPLUM_BaU, 'VY_load') %>%
  rename(G = i4)

data_prep <- "../data/data_prep.gdx"
data_prep_data <- gdxrrw::rgdx(data_prep)
GA <- rgdx.param(data_prep, 'GA') #kha
MAP_GIJ <- rgdx.set(data_prep, 'MAP_GIJ') %>%
  rename(lat = I) %>%
  rename(lon = J)

VY_load2_BaU <- VY_load %>%
  left_join(GA, by = "G") %>%
  mutate(LUAREA = value*GA*1000) #ha


LUAREA_Region_BaU <- VY_load2_BaU %>%
  group_by(i1, i2, i3) %>%
  summarise(sum_LUAREA = sum(LUAREA))

VY_load2_BaU <- VY_load2_BaU %>%
  left_join(LUAREA_Region, by = c("i1", "i2", "i3")) %>%
  mutate(LURATIO = LUAREA/sum_LUAREA) %>%
  left_join(MAP_GIJ, by = c("G"))

VY_load2_BaU$lat <- as.numeric(as.character(VY_load2_BaU$lat))
VY_load2_BaU$lon <- as.numeric(as.character(VY_load2_BaU$lon))
VY_load2_BaU$i2 <- as.numeric(as.character(VY_load2_BaU$i2))



df_area_500C <- VY_load2 %>%
  mutate(
    i2 = as.numeric(i2),  # ← 型を揃える
    LandType = case_when(
      i3 %in% cropland_codes ~ "Cropland",
      i3 == "PAS" ~ "Pasture",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(LandType)) %>%
  group_by(i2, LandType) %>%
  summarise(Area = sum(LUAREA, na.rm = TRUE), .groups = "drop") %>%
  mutate(Scenario = "500C")

df_area_BaU <- VY_load2_BaU %>%
  mutate(
    i2 = as.numeric(i2),  # ← 型を揃える
    LandType = case_when(
      i3 %in% cropland_codes ~ "Cropland",
      i3 == "PAS" ~ "Pasture",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(LandType)) %>%
  group_by(i2, LandType) %>%
  summarise(Area = sum(LUAREA, na.rm = TRUE), .groups = "drop") %>%
  mutate(Scenario = "Baseline")

df_area_all <- bind_rows(df_area_500C, df_area_BaU)

Areaplot <- ggplot(df_area_all, aes(x = as.factor(i2), y = Area / 1e6, fill = LandType)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Scenario) +
  labs(x = "Year", y = "Area (Mha)", fill = "Land Type",
       title = "Global Cropland and Pasture Area Over Time") +
  scale_fill_manual(values = c("Cropland" = "#66c2a5", "Pasture" = "#a6761d")) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("../output/Agrland_area.png", plot = Areaplot, width = 12, height = 7.5, dpi = 300)



VY_load2_BaU <- VY_load2
# 土地利用タイプ
cropland_codes <- c("CL")

# 各シナリオでCropland面積を年ごとに集計
df_cropland_500C <- VY_load2 %>%
  mutate(i2 = as.numeric(i2)) %>%
  filter(i3 %in% cropland_codes) %>%
  group_by(i2) %>%
  summarise(Area = sum(LUAREA, na.rm = TRUE), .groups = "drop") %>%
  mutate(Scenario = "500C")

df_cropland_BaU <- VY_load2_BaU %>%
  mutate(i2 = as.numeric(i2)) %>%
  filter(i3 %in% cropland_codes) %>%
  group_by(i2) %>%
  summarise(Area = sum(LUAREA, na.rm = TRUE), .groups = "drop") %>%
  mutate(Scenario = "Baseline")


df_cropland_all <- bind_rows(df_cropland_500C, df_cropland_BaU)


Cropland_area <- ggplot(df_cropland_all, aes(x = i2, y = Area / 1e6, color = Scenario)) +
  geom_line(size = 1.2) +
  labs(
    x = "Year",
    y = "Cropland Area (million ha)",
    color = "Scenario"
  ) +
  scale_y_continuous(
  expand = c(0, 0),
  limits = c(0, NA)  
) +
  scale_x_continuous(
    breaks = c(2010, 2030, 2050, 2100),
    limits = c(2010, 2100)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25)
  )

ggsave("../output/Cropland_area_rev.png", plot = Cropland_area, width = 12, height = 6, dpi = 300)







breaks_vals <- seq(0, 25, by = 2.5)
labels_vals <- c("0", "2.5", "5", "7.5", "10", "12.5", "15", "17.5", "20", "22.5", "25+")

custom_colors <- c(
  "#ffffff",  # white
  "#deebf7",  # light blue
  "#c7e9c0",  # light green
  "#74c476",  # medium green
  "#ffffb2",  # pale yellow
  "#fecc5c",  # soft orange
  "#fd8d3c",  # orange
  "#f16913",  # moderate orange-red
  "#d94801",  # soft reddish-brown
  "#a63603"   # medium brown
)

legend_shared <- get_legend(
  ggplot() +
    geom_tile(data = Inputdismap_2010, aes(x = x, y = y, fill = Input)) +
    scale_fill_stepsn(
      colors = custom_colors,
      breaks = breaks_vals,
      limits = c(0, 25),
      labels = labels_vals,
      na.value = "white",
      name = "Total N input in 2100\n(g N m⁻² yr⁻¹)",
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(12, "cm"),
        barheight = unit(0.2, "cm")
      )
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    )
)


top_row <- plot3_bordered
bottom_row <- plot_grid(plot2_bordered, plot1_bordered, ncol = 2, rel_widths = c(1, 1))

map_grid <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  rel_heights = c(1, 1),
  align = "v"
)

final_plot <- plot_grid(
  map_grid,
  legend_shared,
  ncol = 1,
  rel_heights = c(1, 0.11)
)


ggsave("../output/Input_combined_rev.png", plot = final_plot, width = 12, height = 7.5, dpi = 300)




cropland_codes <- c("PAS")


df_cropland_500C <- VY_load2 %>%
  mutate(i2 = as.numeric(i2)) %>%
  filter(i3 %in% cropland_codes) %>%
  group_by(i2) %>%
  summarise(Area = sum(LUAREA, na.rm = TRUE), .groups = "drop") %>%
  mutate(Scenario = "500C")

df_cropland_BaU <- VY_load2_BaU %>%
  mutate(i2 = as.numeric(i2)) %>%
  filter(i3 %in% cropland_codes) %>%
  group_by(i2) %>%
  summarise(Area = sum(LUAREA, na.rm = TRUE), .groups = "drop") %>%
  mutate(Scenario = "Baseline")


df_cropland_all <- bind_rows(df_cropland_500C, df_cropland_BaU)

Cropland_area <- ggplot(df_cropland_all, aes(x = i2, y = Area / 1e6, color = Scenario)) +
  geom_line(size = 1.2) +
  labs(
    x = "Year",
    y = "Pasture Area (million ha)",
    color = "Scenario"
  ) +
  scale_y_continuous(
  expand = c(0, 0),
  limits = c(0, NA)  
) +
  scale_x_continuous(
    breaks = c(2010, 2030, 2050, 2100),
    limits = c(2010, 2100)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25)
  )

ggsave("../output/Pasture_area.png", plot = Cropland_area, width = 12, height = 6, dpi = 300)




df_plot <- IAMC_Template_liv %>%
  filter(
    RegionCode == "World",
    SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_CACN_NoCC")
  ) %>%
  mutate(
    YEMF = as.numeric(as.character(YEMF)),
    Change_Rate = as.numeric(as.character(Change_Rate)),
     ScenarioLabel = case_when(
      SCENARIO == "SSP2_BaU_NoCC" ~ "Baseline",
      SCENARIO == "SSP2_500C_CACN_NoCC" ~ "500C"
    )) %>%
  group_by(SCENARIO) %>%
  mutate(Change_Rate_norm = Change_Rate / Change_Rate[YEMF == 2010]) %>%
  ungroup()


livnum <- ggplot(df_plot, aes(x = YEMF, y = Change_Rate_norm, color = ScenarioLabel)) +
  geom_line(size = 1.2) +
  labs(
    x = "Year",
    y = "Livestock Population Change Rate",
    color = "Scenario"
  ) +
  scale_x_continuous(
    breaks = c(2010, 2030, 2050, 2100),
    limits = c(2010, 2100)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25)
  )

ggsave("../output/livnum.png", plot = livnum, width = 12, height = 7, dpi = 300)


#Region Aggregate
OUTgasNH3_R <- OUTgasdis_NH3 %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(OUTgasNH3 = sum(OUTgasgrid)) %>%
  filter(YEMF == 2010)
OUTgasNH3_R <- OUTgasNH3_R[, c(2,3)]

OUTgasNH3_AIMHub <- IAMC_Template %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  filter(VEMF == "Emi_NH3_AFO_Agr_Man_Soi") %>%
  filter(YEMF == 2010)
OUTgasNH3_AIMHub <- OUTgasNH3_AIMHub[, c(2,5)]
OUTgasNH3_AIMHub <- OUTgasNH3_AIMHub %>%
  rename(OUTgasNH3_AIMHub = IAMC_Template)  %>%
  rename(R = REMF)

NH3_df <- read_csv("../../../Others/Data/CEDS/CEDS2025_3/v_2025_03_11_aggregate/v_2025_03_11_aggregate/NH3_CEDS_emissions_by_country_sector_v_2025_03_11.csv")
ISO17<-read.csv("../data/ISO17.csv")
NH3_df <- NH3_df %>% mutate(country = toupper(country))
OUTgasNH3_EDGAR <- NH3_df %>%
  left_join(ISO17, by = "country") %>%
  filter(sector == "3D_Soil-emissions") %>%
  select(country, region, X2010) %>%
  group_by(region) %>%
  summarise(OUTgasNH3_EDGAR = sum(X2010)) %>%
  filter(!is.na(region)) %>%
  rename(R = region)

OUTgasNH3_17 <- OUTgasNH3_R %>%
  left_join(OUTgasNH3_AIMHub, by = "R") %>%
  left_join(OUTgasNH3_EDGAR, by = "R") %>%
  mutate(OUTgasNH3_EDGAR = OUTgasNH3_EDGAR/1000)


df_long <- OUTgasNH3_17 %>%
  pivot_longer(
    cols = c(OUTgasNH3, OUTgasNH3_AIMHub, OUTgasNH3_EDGAR),
    names_to = "Inventory",
    values_to = "Emission"
  )
library(ggplot2)
library(cowplot)


p_legend <- ggplot(df_long, aes(x = Inventory, y = Emission, fill = Inventory)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(
    "OUTgasNH3" = "#1b9e77",
    "OUTgasNH3_AIMHub" = "#d95f02",
    "OUTgasNH3_EDGAR" = "#7570b3"
  ),
  labels = c(
    "OUTgasNH3" = "本研究",
    "OUTgasNH3_AIMHub" = "AIM-Hub",
    "OUTgasNH3_EDGAR" = "EDGAR"
  )) +
  theme(legend.position = "bottom")  


legend <- get_legend(p_legend+
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),         
      legend.text = element_text(size = 20),
      legend.spacing.x = unit(0.65, "cm")    
    )
)


NH3_main <- ggplot(df_long, aes(x = Inventory, y = Emission, fill = Inventory)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  facet_wrap(~ R, scales = "free_y", ncol = 5) +  
  labs(
    x = "Inventory",
    y = "NH₃ Emissions (Tg N/yr)"
  ) +
  scale_fill_manual(values = c(
    "OUTgasNH3" = "#1b9e77",
    "OUTgasNH3_AIMHub" = "#d95f02",
    "OUTgasNH3_EDGAR" = "#7570b3"
  ),
   labels = c(
    "OUTgasNH3" = "本研究",
    "OUTgasNH3_AIMHub" = "AIM-Hub",
    "OUTgasNH3_EDGAR" = "EDGAR"
  )) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "gray85", color = NA),
    strip.text = element_text(size = 25, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 25),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


final_plot <- plot_grid(
  NH3_main, legend,
  ncol = 1,
  rel_heights = c(1, 0.1),  
  align = "v"
)


ggsave("../output/NH3_17_revised.png", plot = final_plot, width = 12, height = 10, dpi = 300)


#OUTlea Region Aggregate
OUTlea_R <- OUTleadis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(OUTleagrid = sum(OUTleagrid)) %>%
  filter(YEMF == 2010) %>%
  mutate(OUTleagrid = OUTleagrid*(77/71))

region_map <- c(
  "BRA" = "South America",
  "CAN" = "North America",
  "CHN" = "Asia",
  "CIS" = "Europe",
  "IND" = "Asia",
  "JPN" = "Asia",
  "TUR" = "Europe",
  "USA" = "North America",
  "XAF" = "Africa",
  "XER" = "Europe",
  "XE25" = "Europe",
  "XLM" = "South America",
  "XME" = "Asia",
  "XNF" = "Africa",
  "XOC" = "Oceania",
  "XSA" = "Asia",
  "XSE" = "Asia"
)

OUTlea_summary <- OUTlea_R %>%
  mutate(R = as.character(R),                    
         RegionGroup = region_map[R])  %>%  
  group_by(RegionGroup) %>%
  summarise(
    OUTleagrid = sum(OUTleagrid, na.rm = TRUE)
  )


#INput Region Aggregate
Input_R_2010 <- Inputdis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(Input = sum(Input)*(238/243)) %>%
  filter(YEMF == 2010)
Input_R_2030 <- Inputdis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(Input = sum(Input)) %>%
  filter(YEMF == 2030)
Input_R_2050 <- Inputdis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(Input = sum(Input)) %>%
  filter(YEMF == 2050)
Input_R_2100 <- Inputdis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(Input = sum(Input)) %>%
  filter(YEMF == 2100)
Input_R_2010 <- Input_R_2010[, c(2,3)] %>%
  rename(Input2010 = Input)
Input_R_2030 <- Input_R_2030[, c(2,3)] %>%
  rename(Input2030 = Input)
Input_R_2050 <- Input_R_2050[, c(2,3)] %>%
  rename(Input2050 = Input)
Input_R_2100 <- Input_R_2100[, c(2,3)] %>%
  rename(Input2100 = Input)

Input_R <- Input_R_2010 %>%
  left_join(Input_R_2030, by = "R") %>%
  left_join(Input_R_2050, by = "R") %>%
  left_join(Input_R_2100, by = "R") %>%

df_input_long <- Input_R %>%  
  pivot_longer(
    cols = starts_with("Input"), 
    names_to = "Year", 
    values_to = "Value"
  ) %>%
  mutate(
    Year = str_remove(Year, "Input") %>% as.numeric()
  )

Input_main <- ggplot(df_input_long, aes(x = factor(Year), y = Value, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.7) +
  facet_wrap(~ R, scales = "free_y", ncol = 5) +
  labs(
    x = "Year",
    y = "Nitrogen Input (Tg N/yr)"
  ) +
  scale_fill_brewer(palette = "Set2", guide = "none") +  
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "gray85", color = NA),
    strip.text = element_text(size = 25, face = "bold"),
     axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 24),
    axis.title = element_text(size = 25),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("../output/Input_17_revised.png", plot = Input_main, width = 14, height = 10, dpi = 300)




INman_R_2010 <- INmandis %>%
  left_join(MAP_RG, by = "G") %>%
  group_by(YEMF, R) %>%
  summarise(ManureGrid = sum(ManureGrid)) %>%
  filter(YEMF == 2010)

