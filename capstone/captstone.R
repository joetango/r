library(dplyr)
library(sf)
library(randomForest)
library(spatialRF) ## https://cloud.r-project.org/web/packages/spatialRF/index.html
                    ## https://blasbenito.github.io/spatialRF/
library(arrow)


data <- read_parquet('yellow_tripdata_2025-08.parquet')
zones <- st_read("taxi_zones/taxi_zones.shp")

# taxi_spatial <- data %>%
#   left_join(zones, by = c("PULocationID" = "LocationID")) %>%
#   st_as_sf()

zone_centroids <- zones %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(LocationID, lon, lat)

taxi_spatial <- data %>%
  left_join(zone_centroids, by = c("PULocationID" = "LocationID")) %>%
  left_join(zone_centroids, by = c("DOLocationID" = "LocationID"),
            suffix = c("", "_dropoff"))

model <- rf_spatial(
  data = taxi_spatial,
  dependent.variable.name = "trip_count",
  predictor.variable.names = c("fare", "distance", "hour"),
  xy = c("lon", "lat")
)