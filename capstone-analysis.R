library(dplyr)
library(ggplot2)
library(sf)
library(randomForest)
library(tigris)
library(gridExtra)
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

### Generating some variables ###

set.seed(1)

data.fl$smoking.pct <-
  scale(data.fl$cvd.rate.per.100k) * 5 +
  rnorm(nrow(data.fl), 25, 3)

data.fl$drinking.pct <-
  -scale(data.fl$cvd.rate.per.100k) * 2 +
  rnorm(nrow(data.fl), 30, 4)

### spacially correlated predicotrs
# center.x <- mean(data.fl$x)
# center.y <- mean(data.fl$y)
# 
# dist.center <- sqrt(
#   (data.fl$x - center.x)^2 +
#     (data.fl$y - center.y)^2
# )
# 
# data.fl$smoking.pct <- scale(-dist.center) * 10 +
#   rnorm(nrow(data.fl), 25, 3)
# 
# data.fl$drinking.pct <- scale(data.fl$y) * 8 +
#   rnorm(nrow(data.fl), 30, 3)

## from poverty data
data.fl$poverty.pct <- ses$Percent

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

p1 <- ggplot(data.fl.model) +
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

rf2 <- randomForest(
  cvd.rate.per.100k ~ poverty.pct + smoking.pct + drinking.pct,
  data = data.fl.rf
)
data.fl.model$rf2.preds <- rf2$predicted
 
 
 ggplot(data.fl.model) +
   geom_sf(aes(fill = rf2.preds)) +
   scale_fill_viridis_c() +
   labs(title = "Predicted CVD Mortality")
 
 data.fl.model$residual2 <- data.fl.model$cvd.rate.per.100k - data.fl.model$rf2.preds
 
 p2 <- ggplot(data.fl.model) +
   geom_sf(aes(fill = residual2)) +
   scale_fill_gradient2(
     low = "blue",
     mid = "white",
     high = "red"
   ) +
   labs(title = "Spatial Residuals")
 
grid.arrange(p1, p2) ## very similar, which probably means the spacial element 
## does not explain outcome as much as predictors alone do (because small area?)
 
 ##RSME
 
 rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
 
 rmse_rf  <- rmse(data.fl.model$cvd.rate.per.100k, data.fl.model$rf2.preds)
 rmse_srf <- rmse(data.fl.model$cvd.rate.per.100k, data.fl.model$predicted)
 data.frame(
   model = c("RF", "Spatial RF"),
   RMSE = c(rmse_rf, rmse_srf)
 ) 
 