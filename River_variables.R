################################################################################
# VA RIVER VARIABLES
# Author: Hana Thurman
# December 17, 2024

# Description: 
# Testing the impact of different variables on flow wave lengths in Virginia.
################################################################################


################################################################################
# Load libraries
################################################################################

if (!"ggplot2" %in% rownames(installed.packages())){
  install.packages("ggplot2")}; require(ggplot2)
if (!"scales" %in% rownames(installed.packages())){
  install.packages("scales")}; require(scales)
if (!"sf" %in% rownames(installed.packages())){
  install.packages("sf")}; require(sf)
if (!"tidyverse" %in% rownames(installed.packages())){
  install.packages("tidyverse")}; require(tidyverse)
if (!"lubridate" %in% rownames(installed.packages())){
  install.packages("lubridate")}; require(lubridate)


################################################################################
# Lengths of all Virginia flow waves
################################################################################
# Load lengths (shapefile created in VA_flow_wave_lengths.R)
all_lengths <- "VA_flow_wave_lengths_all.shp"
all_lengths <- st_read(all_lengths)

# Plot
all_lengths |>
  ggplot() +
  stat_ecdf(aes(mn_fw_l)) +
  labs(x = "Mean flow wave length (km)", y = "ECDF") +
  theme(axis.text = element_text(size = 13), axis.title = element_text(size = 13))


################################################################################
# Lengths versus landcover
################################################################################
# Load lengths
all_lengths <- "VA_flow_wave_lengths_all.shp"
all_lengths <- st_read(all_lengths)

# Load landcover
# CSV created using GEE code at 
# https://code.earthengine.google.com/8e1c140d2b272c7ab7f7cfe817ed925f?accept_repo=projects%2Fgee-edu%2Fbook 
# Substitute for local directory
landcover <- read.csv("C:/...NLCD_with_histogram.csv")

# Join lengths and landcover
lengths_landcover <-
  left_join(all_lengths, landcover, by = c("reach_d" = "reach_id"))

# Boxplot
lengths_landcover |>
  ggplot() +
  geom_boxplot(aes(Landcover_binned, mn_fw_l, fill = Landcover_binned)) +
  geom_text(aes(x = 1, 
                y = 1, 
                label = paste0("n = ", nrow(lengths_landcover[Landcover_binned == "Cultivated Crops", ]))), 
            vjust = 0) +
  geom_text(aes(x = 2, 
                y = 1, 
                label = paste0("n = ", nrow(lengths_landcover[Landcover_binned == "Deciduous Forest", ]))), 
            vjust = 0) +
  geom_text(aes(x = 3, 
                y = 1, 
                label = paste0("n = ", nrow(lengths_landcover[Landcover_binned == "Developed", ]))), 
            vjust = 0) +
  geom_text(aes(x = 4, 
                y = 1, 
                label = paste0("n = ", nrow(lengths_landcover[Landcover_binned == "Mixed or Evergreen Forest", ]))), 
            vjust = 0) +
  geom_text(aes(x = 5, 
                y = 1, 
                label = paste0("n = ", nrow(lengths_landcover[Landcover_binned == "Pasture/Hay", ]))), 
            vjust = 0) +
  geom_text(aes(x = 6, 
                y = 1, 
                label = paste0("n = ", nrow(lengths_landcover[Landcover_binned == "Woody Wetlands", ]))), 
            vjust = 0) +
  labs(x = "Landcover type", y = "Mean flow wave length (km)") +
  theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 13), 
        legend.position = "none") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = c("#F39B7FFF", "#91D1C299", "#E64B35FF", "#00A087FF", "#B09C85FF", "#4DBBD5FF"))


################################################################################
# Lengths versus number of obstructions
################################################################################
# Load lengths
all_lengths <- "VA_flow_wave_lengths_all.shp"
all_lengths <- st_read(all_lengths)

# Load obstructions
# Used spatial join in ArcGIS Pro to get obstructions per reach
# Substitute for local folder
obstructions <- read.csv("C:/...Obstructions.csv")

# Join
lengths_obstructions <-
  left_join(all_lengths, obstructions, by = c("reach_d" = "reach_id"))

lengths_obstructions <- lengths_obstructions |> as.data.frame()

# Combine reaches with 2+ obstructions
lengths_obstructions <-
  lengths_obstructions |>
  mutate(Count_all_obstructions_binned = 
           case_when(Count_all_obstructions == 0 ~ "0",
                      Count_all_obstructions == 1 ~ "1",
                      Count_all_obstructions == 2 ~ "2+",
                      Count_all_obstructions == 3 ~ "2+",))

# Plot
lengths_obstructions |>
  ggplot() +
  geom_boxplot(aes(Count_all_obstructions_binned, mn_fw_l, 
                   group = Count_all_obstructions_binned, 
                   fill = Count_all_obstructions_binned)) +
  geom_text(aes(x = 1, 
                y = 1, 
                label = paste0("n = ", nrow(lengths_obstructions[Count_all_obstructions_binned == "0", ]))), 
            vjust = 0) +
  geom_text(aes(x = 2, 
                y = 1, 
                label = paste0("n = ", nrow(lengths_obstructions[Count_all_obstructions_binned == "1", ]))), 
            vjust = 0) +
  geom_text(aes(x = 3, y = 1, 
                label = 
                  paste0("n = ", nrow(lengths_obstructions[Count_all_obstructions_binned == "2+", ]))), 
            vjust = 0) +
  labs(x = "Number of obstructions", y = "Mean flow wave length (km)") +
  theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 13), 
        legend.position = "none") +
  scale_fill_manual(values = c("#91D1C299", "#00A08799", "#00A087FF"))


################################################################################
# Lengths versus RiverATLAS variables
# Terrain slope and population density
################################################################################
# Load lengths
all_lengths <- "VA_flow_wave_lengths_all.shp"
all_lengths <- st_read(all_lengths)

# Load RiverAtlas
# Used spatial join in ArcGIS Pro to get values per reach
# Substitute for local folder
riveratlas <- read.csv("C:/...RiverAtlas.csv")

# Join lengths and river atlas
lengths_river_atlas <-
  left_join(all_lengths, riveratlas, by = "reach_d")

# Compute slope in degrees by dividing by 10
lengths_river_atlas <-
  lengths_river_atlas |>
  mutate(slp_dg_cav_degrees = slp_dg_cav / 10)

# Linear model for terrain slope
lm.1 <- lm(mn_fw_l ~ slp_dg_cav_degrees, data = lengths_river_atlas)
summary(lm.1)

# Plot terrain slope
lengths_river_atlas |>
  ggplot() +
  geom_point(aes(slp_dg_cav_degrees, mn_fw_l)) +
  geom_smooth(aes(slp_dg_cav_degrees, mn_fw_l), method='lm', se=F, color = "#E64B35FF") +
  theme(axis.text = element_text(size = 11), 
        axis.title = element_text(size = 11), 
        legend.position = "none") +
  labs(x = "Terrain slope (degrees)", y = "Mean flow wave length (km)")

# Linear model for population density
lm.2 <- lm(mn_fw_l ~ ppd_pk_cav, data = lengths_river_atlas)
summary(lm.2)

# Plot pop density
lengths_river_atlas |>
  ggplot() +
  geom_point(aes(ppd_pk_cav, mn_fw_l)) +
  geom_smooth(aes(ppd_pk_cav, mn_fw_l), method='lm', se=F, color = "#E64B35FF") +
  theme(axis.text = element_text(size = 11), 
        axis.title = element_text(size = 11), 
        legend.position = "none") +
  labs(x = "Population density (people per km²)", y = "Mean flow wave length (km)")


