################################################################################
# VA FLOW WAVE LENGTHS
# Author: Hana Thurman
# December 17, 2024

# Description: 
# General workflow for finding and measuring flow waves in Virginia
# using a wavelet transform approach.
################################################################################


################################################################################
# Load libraries
################################################################################

if (!"ggplot2" %in% rownames(installed.packages())){
  install.packages("ggplot2")}; require(ggplot2)
if (!"biwavelet" %in% rownames(installed.packages())){
  install.packages("biwavelet")}; require(biwavelet)
if (!"sf" %in% rownames(installed.packages())){
  install.packages("sf")}; require(sf)
if (!"tidyverse" %in% rownames(installed.packages())){
  install.packages("tidyverse")}; require(tidyverse)
if (!"lubridate" %in% rownames(installed.packages())){
  install.packages("lubridate")}; require(lubridate)


################################################################################
# Datasets
################################################################################
# SWOT data available at: https://podaac.jpl.nasa.gov/SWOT 
# SWORD v16 shapefiles available at: https://www.swordexplorer.com/

################################################################################
# Step 1: Wavelet transform for each pass and river
# NOTE: Run this for all passes on a single river before moving to Step 2!
################################################################################
# Load the nodes that I manually defined in ArcGIS Pro as being part of the river.
# Substitute for your local path.
SWOT_data <- read.csv("C:/...SWOT_pass_076_Shenandoah_to_Potomac.csv")

# Set pass number
pass_no <- "076"

# Set river name
river <- "Shenandoah_to_Potomac"

# Clean data
SWOT_data <- SWOT_data[-which(SWOT_data$wse <= -1000000),]
SWOT_data$ymd <- ymd_hms(SWOT_data$time_str)
SWOT_data$Date <- format(as.Date(SWOT_data$ymd), "%m-%d-%Y")
SWOT_data$Date <- as.factor(SWOT_data$Date)

# Filter node quality
SWOT_data <- 
  SWOT_data |>
  filter(node_q < 3)

# Find max dist and calculate diff_km field
max_dist_SWOT_data <- max(SWOT_data$p_dist_out)
min_dist_SWOT_data <- min(SWOT_data$p_dist_out)
max_scale <- (max_dist_SWOT_data - min_dist_SWOT_data)/1000
SWOT_data$Diff_km <- ((max_dist_SWOT_data - SWOT_data$p_dist_out))/1000

# Plot SWOT water surface elevations for manual inspection
SWOT_data |>
  ggplot() +
  geom_line(aes(x = Diff_km, y = wse, color = Date)) +
  labs(x = "Distance downstream (km)", y = "Water surface elevation (m)")

# Calculate low flow profile (10th percentile wse)
Low_flow <-
  SWOT_data |>
  group_by(node_id) |>
  summarize(quantile(wse, probs = 0.1, na.rm = TRUE))
names(Low_flow)[2] <- "wse_10th_perc"

# Subtract low flow threshold from SWOT measurements to get relative wse
SWOT_data <-
  full_join(SWOT_data, Low_flow, by = "node_id")

SWOT_data <-
  SWOT_data |>
  mutate(relative_wse = wse - wse_10th_perc)

# Remove profiles where wse is below low flow threshold (non-flow waves)
SWOT_data <-
  SWOT_data |>
  filter(relative_wse > 0)

#Plot again with relative wse
SWOT_data |>
  ggplot() +
  geom_line(aes(x = Diff_km, y = relative_wse, color = Date))

# Get list of dates for each river and pass
date_list <- levels(unique(SWOT_data$Date))

# Set empty list
datalist = list()

# Loop through list and get wavelet power for every date
for (i in 1:length(date_list)) {
  # Interpolate and smooth with cubic splines
  interp_1 <- SWOT_data |> filter(Date == date_list[i])
  interp_1 <- smooth.spline(interp_1$Diff_km,
                            interp_1$relative_wse,
                            spar = 1,
                            cv=TRUE)
  interp_1 <- data.frame(interp_1$x, interp_1$y)
  interp_1 <- approx(x = interp_1$interp_1.x, 
                     y = interp_1$interp_1.y, 
                     method = "linear") 
  
  # Wavelet transform
  t1 = cbind(interp_1$x, interp_1$y) # Define variable with "space stamp"
  wt.t1 = wt(d = t1,
             dj = 1/100, #spacing between scales
             max.scale = max_scale, #set max scale as max downstream distance
             mother = "morlet",
             sig.level = 0.95,
             sig.test = 0) 
  
  # Calculate averaged wavelet power over length scales
  power <- data.frame(wt.t1$period, wt.t1$power)
  names(power)[1] <- "length_scale"
  power$scale_avg_power <- rowMeans(power[-1])
  
  # Get flow wave length
  flow_wave_length <- 
    power$length_scale[power$scale_avg_power >= max(power$scale_avg_power)]
  dat <- data.frame(river = river, pass_no = pass_no)
  dat$date <- date_list[i]
  dat$flow_wave_length <- flow_wave_length
  datalist[[i]] <- dat # add it to your list
  
}

# Save flow wave length
flow_wave_lengths_by_pass = do.call(rbind, datalist)

# Remove events that are longer than the pass itself
flow_wave_lengths_by_pass <- flow_wave_lengths_by_pass |>
  filter(flow_wave_length < max(SWOT_data$Diff_km)) 

# Create new data frame
assign(paste0("SWOT_pass_", pass_no, "_", river), flow_wave_lengths_by_pass)

# Plot power spectrum
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
plot(wt.t1, plot.cb=TRUE, plot.phase=FALSE)


################################################################################
# Step 2: Merge all passes for each river and find mean reach-level lengths
# NOTE: Do not run this until Step 1 is completed!
################################################################################
# Set a reach shapefile
# These SWORD reaches were manually defined as being part of the river
# Substitute for your local path.
reaches <- "C:/...Shenandoah_to_Potomac_River.shp"
reaches <- st_read(reaches)

# Merge lengths from all passes and dates for each river
# Note: This is an example...change the names to the appropriate river/passes.
flow_wave_lengths_all <-
  Reduce(function(x, y) merge(x, y, all=TRUE),
         list(SWOT_pass_076_Shenandoah_to_Potomac,
              SWOT_pass_091_Shenandoah_to_Potomac,
              SWOT_pass_354_Shenandoah_to_Potomac,
              SWOT_pass_382_Shenandoah_to_Potomac,
              SWOT_pass_397_Shenandoah_to_Potomac))

# Find mean flow wave lengths by river and pass
flow_wave_lengths_mean <-
  flow_wave_lengths_all |>
  group_by(river, pass_no) |>
  summarize(mean_length = mean(flow_wave_length))

# Modify the shapefile to calculate mean lengths for each reach
# Separate out pass numbers in the "SWOT orbit" column
reaches$swot_orbit_keep <- reaches$swot_orbit
reaches <- separate(reaches, col = swot_orbit,
                    into = c("pass_1", "pass_2", "pass_3", "pass_4"), sep = " ")

# Convert reach numbers to integers to match between datasets
reaches$pass_1 <- as.numeric(reaches$pass_1)
reaches$pass_2 <- as.numeric(reaches$pass_2)
reaches$pass_3 <- as.numeric(reaches$pass_3)
reaches$pass_4 <- as.numeric(reaches$pass_4)
flow_wave_lengths_mean$pass_no <- as.numeric(flow_wave_lengths_mean$pass_no)

# Join mean lengths to reach shapefile
# First pass
lengths_by_reach <-
  left_join(reaches, flow_wave_lengths_mean, by = c("pass_1" = "pass_no"))
lengths_by_reach$length_1 <- lengths_by_reach$mean_length
lengths_by_reach <- subset(lengths_by_reach, select = -mean_length)
lengths_by_reach <- subset(lengths_by_reach, select = -river)

# Second pass (if applicable)
lengths_by_reach <-
  left_join(lengths_by_reach, flow_wave_lengths_mean, by = c("pass_2" = "pass_no"))
lengths_by_reach$length_2 <- lengths_by_reach$mean_length
lengths_by_reach <- subset(lengths_by_reach, select = -mean_length)
lengths_by_reach <- subset(lengths_by_reach, select = -river)

# Third pass (if applicable)
lengths_by_reach <-
  left_join(lengths_by_reach, flow_wave_lengths_mean, by = c("pass_3" = "pass_no"))
lengths_by_reach$length_3 <- lengths_by_reach$mean_length
lengths_by_reach <- subset(lengths_by_reach, select = -mean_length)
lengths_by_reach <- subset(lengths_by_reach, select = -river)

# Fourth pass (if applicable)
lengths_by_reach <-
  left_join(lengths_by_reach, flow_wave_lengths_mean, by = c("pass_4" = "pass_no"))
lengths_by_reach$length_4 <- lengths_by_reach$mean_length
lengths_by_reach <- subset(lengths_by_reach, select = -mean_length)
lengths_by_reach <- subset(lengths_by_reach, select = -river)


# Find mean length by reach
lengths_by_reach <-
  lengths_by_reach |>
  st_drop_geometry() 

lengths_by_reach <-
  lengths_by_reach |>
  mutate(mean_length_by_pass = 
           rowMeans(lengths_by_reach[,c("length_1", "length_2", "length_3", "length_4")], 
                    na.rm = TRUE))


# Write shapefile
reaches$mean_length_by_pass <- lengths_by_reach$mean_length_by_pass
write_sf(reaches, paste0("Output_reach_shp/", river, "_mean_lengths_by_reach.shp"), overwrite = TRUE)


################################################################################
# Step 3: Combine mean flow wave lengths for all rivers and create final shapefile
# of mean flow wave lengths by reach for the state of Virginia.
################################################################################
# Get shapefiles from folder (created during Step 2)
# Substitute for your local directory.
file_list <- list.files("C:/.../Output_reach_shp/", 
                        pattern = "*shp", full.names = TRUE)
shapefile_list <- lapply(file_list, read_sf)

# Get rid of old column
shapefile_list[[2]] <- shapefile_list[[2]] |> select(-fw_lngt)

# Bind all (nine rivers...may add or substract numbers as applicable)
all_shapefiles <- rbind(shapefile_list[[1]], 
                        shapefile_list[[2]], 
                        shapefile_list[[3]], 
                        shapefile_list[[4]],
                        shapefile_list[[5]],
                        shapefile_list[[6]],
                        shapefile_list[[7]],
                        shapefile_list[[8]],
                        shapefile_list[[9]])

# Remove reaches with no mean length (no observation)
all_shapefiles <- all_shapefiles[!is.na(all_shapefiles$mn_ln__),]

# Summarize mean flow wave length by reach
# This includes reaches impacted by multiple rivers
all_lengths <-
  all_shapefiles |>
  group_by(reach_d) |>
  summarize(mean_fw_length = mean(mn_ln__))

# Write final shapefile for the state of Virginia
write_sf(all_lengths, paste0("VA_flow_wave_lengths_all.shp"), overwrite = TRUE)