library(dplyr)
library(ggplot2)
library(sf)
library(randomForest)
library(tigris)
library(gridExtra)
library(spatialRF) 
## https://cloud.r-project.org/web/packages/spatialRF/index.html
## https://blasbenito.github.io/spatialRF/


## Data ##
data <- read.csv("Rates_and_Trends_in_Heart_Disease_and_Stroke_Mortality_Among_US_Adults_(35+)_by_County,_Age_Group,_Race_Ethnicity,_and_Sex_–_2000-2019_20260621.csv")
ses <- read.csv("poverty_pct_fl.csv")


#################
## Coordinates ##
#################

counties.sf <- counties(cb = TRUE, year = 2019)

data$LocationID <- as.character(data$LocationID)

data.geo <- counties.sf %>%
  left_join(data, by = c("GEOID" = "LocationID"))

data.planar <- st_transform(data.geo, 5070)

## point per county ##
points <- st_point_on_surface(data.planar)

coordinates <- st_coordinates(points)

data.planar$x <- coordinates[,1]
data.planar$y <- coordinates[,2]

## filter only FL (could do this earlier) ##
data.fl <- data.planar |> 
  filter(STATEFP == "12") |> 
  group_by(NAME, x, y) |> 
  summarise(cvd.rate.per.100k = mean(Data_Value, na.rm = TRUE)) 

## Generating additional variables ##

set.seed(1)

data.fl$smoking.pct <- scale(data.fl$cvd.rate.per.100k) * 5 +
  rnorm(nrow(data.fl), 24, 3)

data.fl$drinking.pct <- -scale(data.fl$cvd.rate.per.100k) * 2 +
  rnorm(nrow(data.fl), 32, 4)

data.fl$education.pct <- rnorm(nrow(data.fl), mean = 50, sd = 10)

data.fl$noise.pct <- abs(rnorm(nrow(data.fl), mean = 0, sd = 1))

## from poverty data ##
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


## GRF model ##
rf <- spatialRF::rf_spatial(
  data = data.fl.rf,
  dependent.variable.name = "cvd.rate.per.100k",
  predictor.variable.names = c("poverty.pct", "smoking.pct", "drinking.pct",
                               "education.pct", "noise.pct"),
  distance.matrix = distance.matrix
)

## predictions ##
data.fl.model$predicted <- rf$predictions
data.fl.model$predicted <- unlist(rf$predictions)

## for limits ##

preds.and.actual <- c(data.fl.model$cvd.rate.per.100k, data.fl.model$predicted,
                      rf2$predicted2) ## need to run RF model below before this line
max.rates <- max(abs(preds.and.actual))

p1 <- ggplot(data.fl.model) +
  geom_sf(aes(fill = cvd.rate.per.100k)) +
  scale_fill_gradient(low = "pink",
                      high = "blue",
                      limit = c(0, max.rates)) +
  labs(title = "Observed CVD Mortality",
       fill = "Deaths per 100,000")

p2 <- ggplot(data.fl.model) +
  geom_sf(aes(fill = predicted)) +
  scale_fill_gradient(low = "pink",
                      high = "blue",
                      limit = c(0, max.rates)) +
  labs(title = "GRF Predicted CVD Mortality",
       fill = "Deaths per 100,000")

grid.arrange(p1, p2, ncol = 1)

## residuals ##
data.fl.model$residual <- data.fl.model$cvd.rate.per.100k - data.fl.model$predicted

## for plotting range ##
all.residuals <- c(data.fl.model$residual, data.fl.model$residual2)

max.abs <- max(abs(all.residuals))

p3 <- ggplot(data.fl.model) +
  geom_sf(aes(fill = residual)) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "yellow",
    limits = c(-max.abs, max.abs)) +
  labs(title = "GRF Spatial Residuals",
       fill = "")

###################
## Random Forest ##
###################

# # old RF
# rf2 <- randomForest(
#   cvd.rate.per.100k ~ poverty.pct + smoking.pct + drinking.pct + education.pct +
#     noise.pct,
#   data = data.fl.rf
# )
# data.fl.model$rf2.preds <- rf2$predicted


rf2 <- spatialRF::rf(
  data = data.fl.rf,
  dependent.variable.name = "cvd.rate.per.100k",
  predictor.variable.names = c("poverty.pct", "smoking.pct", "drinking.pct",
                               "education.pct", "noise.pct")
) 

xy2 <- xy[,2:3]
model.comparison <- rf_compare(models = list(a=rf, b=rf2), xy = xy2)
 
p4 <- ggplot(data.fl.model) +
  geom_sf(aes(fill = rf2.preds)) +
  scale_fill_gradient(low = "pink",
                      high = "blue",
                      limit = c(0, max.rates)) +
  labs(title = "RF Predicted CVD Mortality",
       fill = "Deaths per 100,000")

grid.arrange(p1, p4, ncol = 1)
 
data.fl.model$residual2 <- data.fl.model$cvd.rate.per.100k - data.fl.model$rf2.preds
 
p5 <- ggplot(data.fl.model) +
  geom_sf(aes(fill = residual2)) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "yellow",
    limits = c(-max.abs, max.abs)) +
  labs(title = "RF Spatial Residuals", fill = "")
 
grid.arrange(p3, p5, nrow = 1) 

 
##RSME
 
rmse <- function(obs, pred){
  sqrt(mean((obs - pred)^2))
}
 
rmse.rf  <- rmse(data.fl.model$cvd.rate.per.100k, data.fl.model$rf2.preds)
rmse.srf <- rmse(data.fl.model$cvd.rate.per.100k, data.fl.model$predicted)

data.frame(
  model = c("RF", "GRF"),
  RMSE = c(rmse.rf, rmse.srf)
) 
 
 