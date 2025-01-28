#install.packages("readxl")

# Load required libraries
library(readxl)  # For reading Excel files
library(sf)      # For handling spatial data
library(dplyr)   # For data manipulation
library(tidyr)   # For tidying data
library(stringr) # For string operations
library(ggplot2) # For plotting

# ---- ACLED Data Processing ----
# Read ACLED data for Africa from a CSV file
acleddf_afr <- read.csv2('data/ACLED_afr_to_export.csv')

# Read the shapefile for Africa
africa.shp <- sf::st_read("DATA/.shp/afr_g2014_2013_0.shp")

# Convert ACLED data to a spatial dataframe with specified longitude and latitude
acleddf_afr.shp <- st_as_sf(acleddf_afr, coords = c("longitude", "latitude"), remove = FALSE)

# Set the coordinate reference system (CRS) to WGS 84 (EPSG:4326)
st_crs(acleddf_afr.shp) <- 4326

# Transform ACLED spatial data to match the CRS of the Africa shapefile
acleddf_afr.shp <- st_transform(acleddf_afr.shp, st_crs(africa.shp))

# Plot ACLED events on the map of Africa
gg1 <- ggplot() +
  geom_sf(data = africa.shp, fill = "gray90", color = "gray60") +
  geom_sf(data = acleddf_afr.shp, aes(color = event_type), size = 0.5) +
  scale_color_viridis_d(name = "Event Type") +
  theme_minimal() +
  labs(title = "ACLED Events in Africa") +
  theme(legend.position = "bottom")

# Display the plot
gg1

# ---- Mining Data Processing ----
# Read mining data from an Excel file
minesdf <- read_excel('data/detailed_data_mining.xlsx')

# Remove rows with missing longitude values
minesdf <- minesdf %>% drop_na(longitude)

# Convert mining data to a spatial dataframe
minesdf.shp = st_as_sf(minesdf, coords = c("longitude", "latitude"), remove = FALSE)

# Set the CRS to WGS 84 and transform mining data to match the Africa shapefile CRS
st_crs(minesdf.shp) <- 4326
minesdf.shp <- st_transform(minesdf.shp, st_crs(africa.shp))

# Clean the commodities/products column by keeping only the first commodity
minesdf.shp$commodities <- sapply(minesdf.shp$commodities_products, function(x) sub(",.*", "", x))
minesdf.shp$commodities <- str_replace(minesdf.shp$commodities, ",.*", "")

# Intersect mining data with the Africa shapefile to ensure correct geographic scope
africa_mines.shp <- st_intersection(minesdf.shp, africa.shp)

# Plot mines in Africa categorized by type
gg2 <- ggplot() +
  geom_sf(data = africa.shp, fill = "gray90", color = "gray60") +
  geom_sf(data = africa_mines.shp, aes(color = commodities), size = 1) +
  scale_color_viridis_d(name = "Mine Type") +
  theme_minimal() +
  labs(title = "Mines in Africa by Type") +
  theme(legend.position = "bottom")

# Display the plot
gg2
