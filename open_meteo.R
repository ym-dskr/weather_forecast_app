library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)


url1 <- "https://api.open-meteo.com/v1/forecast?latitude="
url2 <- "&longitude="
# url3 <- "&hourly=temperature_2m,relativehumidity_2m,dewpoint_2m,pressure_msl,precipitation,shortwave_radiation,direct_radiation,diffuse_radiation,vapor_pressure_deficit,windspeed_10m&windspeed_unit=ms&timezone=Asia%2FTokyo"
url3 <- "&hourly=temperature_2m,relativehumidity_2m,pressure_msl,precipitation,direct_radiation,cloudcover,weathercode,snow_depth,windspeed_10m&windspeed_unit=ms&timezone=Asia%2FTokyo"

