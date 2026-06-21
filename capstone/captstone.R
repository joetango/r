library(dplyr)
library(ggplot2)
library(sf)
library(randomForest)
library(spatialRF) ## https://cloud.r-project.org/web/packages/spatialRF/index.html
                    ## https://blasbenito.github.io/spatialRF/
library(arrow)


data <- read_parquet('yellow_tripdata_2025-08.parquet')
zones <- st_read("taxi_zones/taxi_zones.shp")


zones_4326 <- st_transform(zones, 4326)

centroids <- st_centroid(zones_4326)

coords <- st_coordinates(centroids)

zone_centroids <- data.frame(
  LocationID = zones_4326$LocationID,
  lon = coords[,1],
  lat = coords[,2]
)
# 
# zone_centroids <- zones %>%
#   st_centroid() %>%
#   mutate(
#     lon = st_coordinates(.)[,1],
#     lat = st_coordinates(.)[,2]
#   ) %>%
#   st_drop_geometry() %>%
#   select(LocationID, lon, lat)

# taxi_spatial <- data %>%
#   left_join(zone_centroids, by = c("PULocationID" = "LocationID")) %>%
#   left_join(zone_centroids, by = c("DOLocationID" = "LocationID"),
#             suffix = c("", "_dropoff"))

zone_data <- data %>%
  group_by(PULocationID) %>%
  summarise(
    total_count = n(),
    fare = mean(fare_amount, na.rm = TRUE),
    distance = mean(trip_distance, na.rm = TRUE),
    .groups = "drop"
  )

zone_data <- zone_data %>%
  left_join(zone_centroids, by = c("PULocationID" = "LocationID"))

coords <- zone_data[, c("lon", "lat")]

coords <- as.matrix(coords)

dist_matrix <- as.matrix(dist(coords))

model <- rf_spatial(
  data = zone_data,
  dependent.variable.name = "total_count",
  predictor.variable.names = c("fare", "distance"),
  xy = c("lon", "lat"),
  distance.matrix = dist_matrix
)
