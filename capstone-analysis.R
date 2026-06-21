library(dplyr)
library(ggplot2)
library(sf)
library(randomForest)
library(tigris)
library(spatialRF) ## https://cloud.r-project.org/web/packages/spatialRF/index.html
## https://blasbenito.github.io/spatialRF/


data <- read.csv("Rates_and_Trends_in_Heart_Disease_and_Stroke_Mortality_Among_US_Adults_(35+)_by_County,_Age_Group,_Race_Ethnicity,_and_Sex_–_2000-2019_20260621.csv")
ses <- read.csv("poverty_pct_fl.csv")

################################
### Geographic Random Forest ###
################################

counties_sf <- counties(cb = TRUE, year = 2019)

data$LocationID <- as.character(data$LocationID)

data.geo <- counties_sf %>%
  left_join(data, by = c("GEOID" = "LocationID"))

data.planar <- st_transform(data.geo, 5070)

## point per county
pts <- st_point_on_surface(data.planar)

coords <- st_coordinates(pts)

data.planar$x <- coords[,1]
data.planar$y <- coords[,2]

## filter only FL
data.fl <- data.planar |> 
  filter(STATEFP == "12") |> 
  group_by(NAME, x, y) |> 
  summarise(cvd.rate.per.100k = mean(Data_Value, na.rm = TRUE)) 

set.seed(1)
smoking.pct <- round((sample(15:85, 67)/0.99), 2)
drinking.pct <- round((sample(15:85, 67)/0.99), 2)

data.fl$poverty.pct <- ses$Percent
data.fl$smoking.pct <- smoking.pct
data.fl$drinking.pct <- drinking.pct

data.fl.rf <- st_drop_geometry(data.fl)

data.fl.rf <- data.fl |>
  st_drop_geometry() |>
  filter(!is.na(cvd.rate.per.100k))

data.fl.model <- data.fl |>
  filter(!is.na(cvd.rate.per.100k))

xy <- data.fl.model |>
  st_drop_geometry() |>
  select(x, y)

distance.matrix <- as.matrix(dist(xy))

data.fl.rf <- st_drop_geometry(data.fl.model)


## model
rf <- spatialRF::rf(
  data = data.fl.rf,
  dependent.variable.name = "cvd.rate.per.100k",
  predictor.variable.names = c("poverty.pct", "smoking.pct", "drinking.pct"),
  distance.matrix = distance.matrix
)

## predictions
data.fl.rf$predicted <- rf$predictions

data.fl.model$predicted <- rf$predictions

data.fl.model$predicted <- unlist(rf$predictions)

ggplot(data.fl.model) +
  geom_sf(aes(fill = cvd.rate.per.100k)) +
  scale_fill_viridis_c() +
  labs(title = "Observed CVD Mortality")

ggplot(data.fl.model) +
  geom_sf(aes(fill = predicted)) +
  scale_fill_viridis_c() +
  labs(title = "Predicted CVD Mortality")


data.fl.model$residual <- data.fl.model$cvd.rate.per.100k - data.fl.model$predicted

ggplot(data.fl.model) +
  geom_sf(aes(fill = residual)) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red"
  ) +
  labs(title = "Spatial Residuals")

###################
## Random Forest ##
###################

rf <- randomForest(
  cvd.rate.per.100k ~ poverty.pct + smoking.pct + drinking.pct,
  data = data.fl.rf
)
 rf
 