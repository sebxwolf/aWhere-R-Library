## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      eval=TRUE,
                      message = FALSE,
                      warning = FALSE,
                      results='hide')

## ----load_packages-------------------------------------------------------
# load required packages
library(aWhereAPI)
library(dplyr)

# load credentials to access aWhere API
aWhereAPI::load_credentials("credentials_trial.txt")

## ----detectCores, results=TRUE-------------------------------------------
parallel::detectCores()

## ----observedArea--------------------------------------------------------
polygon_wkt <- "POLYGON((34.3 -5.3,34.3 -5.9,35 -5.9,35 -5.3,34.3 -5.3))"
obs_startdate <- as.character(Sys.Date() - 30)
obs_enddate <- as.character(Sys.Date() - 1)

obs_area <- aWhereAPI::daily_observed_area(polygon = polygon_wkt, 
                                           day_start = obs_startdate, 
                                           day_end = obs_enddate, 
                                           numcores = 4, 
                                           bypassNumCallCheck = TRUE)

## ----createKey-----------------------------------------------------------
obs_area$key <- paste(obs_area$gridx, obs_area$gridy, sep = "_")

## ----checkData-----------------------------------------------------------
## This will return the number of aWhere grid cells within the polygon
length(unique(obs_area$key))

## All values in the freq column should equal the number of days requested
plyr::count(obs_area$key)

## ----aggregateObsPrecip--------------------------------------------------
obs_precip <- obs_area %>% group_by(latitude, longitude, gridy, gridx) %>%
                summarise(obs_precip = sum(precipitation.amount)) %>% data.frame()

## ----ltnArea-------------------------------------------------------------
ltn_startday <- format(Sys.Date() - 30, "%m-%d")
ltn_endday <- format(Sys.Date() - 1, "%m-%d")

ltn_area <- aWhereAPI::weather_norms_area(polygon = polygon_wkt, 
                                          monthday_start = ltn_startday,
                                          monthday_end = ltn_endday, 
                                          year_start = 2006,
                                          year_end = 2017, 
                                          numcores = 4, 
                                          bypassNumCallCheck = TRUE)

## ----headLTN, results=TRUE-----------------------------------------------
# take a look at the first 6 rows of the location, day, and long-term normal 
# maximum/minumum temperature data
head(ltn_area[,c('latitude','longitude','gridy','gridx','day',
                 'maxTemp.average','minTemp.average')])

## ----aggregateLTN--------------------------------------------------------
ltn_precip <- ltn_area %>% group_by(latitude, longitude, gridy, gridx) %>%
                summarise(ltn_precip = sum(precipitation.average)) %>% data.frame()

## ----mergeData-----------------------------------------------------------
precip <- merge(obs_precip, ltn_precip, by = c("gridy", "gridx"))
precip$diff <- precip$obs_precip - precip$ltn_precip
precip$perc_diff <- precip$obs_precip/precip$ltn_precip * 100

## ----selectColumns-------------------------------------------------------
precip[,c("latitude.y", "longitude.y")] <- NULL
colnames(precip) <- gsub("\\.x", "", colnames(precip))

## ---- results=TRUE-------------------------------------------------------
# take a look at the first six rows of observed precip, long-term normal precip,
# the difference between them, and the percent difference between them per grid
head(precip[,c('gridy','gridx','obs_precip','ltn_precip','diff','perc_diff')])

## ----createWKT, results=TRUE---------------------------------------------
precip$shapewkt <- mapply(aWhereAPI::create_shapewkt, precip$longitude, precip$latitude)

# take a look at the polygon geometry that was added to the precip data frame
head(precip$shapewkt)

## ----polyID--------------------------------------------------------------
precip$id = c(1:nrow(precip))

## ----customFortify, results=TRUE-----------------------------------------
poly <- setNames(data.frame(rep(precip$id, 
                                each=5), 
                            matrix(unlist(lapply(precip$shapewkt, 
                                                 aWhereAPI::custom_fortify)), 
                                   ncol=2, 
                                   byrow=TRUE)), 
                 c('id', 'longitude', 'latitude'))
head(poly, 10)

## ----plot_daata, results=TRUE--------------------------------------------
plot_data <- merge(poly, precip %>% 
                     select(id, obs_precip, ltn_precip, diff, perc_diff), by = "id")
head(plot_data, 10)

## ----normalPlot, results=TRUE--------------------------------------------
ggplot2::ggplot() + ggplot2::geom_polygon(data = plot_data, 
                                          ggplot2::aes(x = longitude, 
                                                       y = latitude, 
                                                       group = id, 
                                                       fill = diff)) + 
  ggplot2::scale_fill_gradient(low = "red", high = "green")

## ----ggmapPlot, results=TRUE---------------------------------------------
## The ggmap function for stamen maps uses a bounding box to request the image.
## You can then plot over the top off it with any ggplot function as normally used.
ggmap::ggmap(ggmap::get_stamenmap(bbox = c(left = min(plot_data$longitude)
                                           ,right = max(plot_data$longitude)
                                           ,bottom = min(plot_data$latitude)
                                           ,top = max(plot_data$latitude))
                                  ,type = 'toner-hybrid')) + 
  ggplot2::geom_polygon(data = plot_data, ggplot2::aes(x = longitude, 
                                                       y = latitude, 
                                                       group = id, 
                                                       fill = diff)
                        ,alpha = .2) + 
  ggplot2::scale_fill_gradient(low = "red", high = "green")



