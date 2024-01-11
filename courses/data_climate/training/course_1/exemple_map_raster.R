


# Exemple d'une carte raster


library(sf)
library(raster)
library(ggplot2)

# Le fichier raster
raster_tmax5 <- raster('ex_map_raster/tmax5.bil')


# Le fichier GADM (https://gadm.org/download_country.html)
gadm_ecuador <- st_read('ex_map_raster/gadm41_ECU.gpkg')


raster_tmax5_ecuador = crop(raster_tmax5,gadm_ecuador)
raster_tmax5_ecuador = mask(raster_tmax5_ecuador,gadm_ecuador)
par(mar = c(1, 1, 1, 1))
plot(raster_tmax5_ecuador)


gain(raster_tmax5_ecuador) <- 0.1 #must be multiplied by 0.1 to convert back to degrees Celsius
raster_tmax5_ecuador$tmax5

plot(raster_tmax5_ecuador$tmax5) # température maximum en mai

# Converting the raster object into a dataframe
tmax_data_may_df <- as.data.frame(raster_tmax5_ecuador$tmax5, xy = TRUE, na.rm = TRUE)
rownames(tmax_data_may_df) <- c()

ggplot(
  data = tmax_data_may_df,
  aes(x = x, y = y)
) +
  geom_raster(aes(fill = tmax5)) +
  labs(
    title = "Maximum temperature in May",
    subtitle = "For the years 1970-2000"
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradientn(
    name = "Temperature (°C)",
    colours = c("#0094D1", "#68C1E6", "#FEED99", "#AF3301"),
    breaks = c(-20, 0, 20, 40)
  )
