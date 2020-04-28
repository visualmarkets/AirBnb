library(purrr)
library(dplyr)
library(readr)
library(fst)

#------------#
# Covid Data #
#------------#

flightFiles <- list.files("cache/flights", pattern = ".gz", full.names = TRUE)

newFlights <-
  read_csv(
    "cache/flights/flightlist_20200401_20200407.csv", 
    col_types = cols_only(
     callsign            = col_character(),
     icao24              = col_character(),
     estDepartureAirport = col_character(),
     estArrivalAirport   = col_character(),
     firstSeen           = col_datetime(),
     lastSeen            = col_datetime(),
     day                 = col_date()
    )
  ) %>% 
  rename(destination = estArrivalAirport,
         origin = estDepartureAirport)

flightData <-
  map_df(
    flightFiles,
    function(x){
      read_csv(x)
    }
  ) %>% 
  mutate(day = as.Date(day))

airportData <- read_csv("airportMetaData.csv") # jonlite::read_json("airports.json") %>% map_df(~unlist(.x) %>% map_df(~.x))

# write_csv(airportData, "airportMetaData.csv")

flightData %>% 
  bind_rows(newFlights) %>% 
  left_join(airportData %>% select(icao, country, lat, lon), by = c("destination" = "icao")) %>% 
  write.fst("cache/allFlight.fst", compress = 100)
