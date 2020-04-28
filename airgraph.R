#install.packages("remotes")

library(readr)
library(echarts4r)
library(echarts4r.assets)

airportData <- read_csv("airportMetaData.csv") # jonlite::read_json("airports.json") %>% map_df(~unlist(.x) %>% map_df(~.x))

data <- 
  allFlights %>% 
  group_by(lat, lon, country) %>% 
  summarize(value = n()) %>% na.omit() %>% 
  ungroup() %>% 
  mutate(lat = round(lat, 2),
         lon = round(lon, 2))

data %>% 
  e_charts(lon) %>% 
  e_globe(
    environment = ea_asset("starfield"),
    base_texture = ea_asset("world topo"), 
    height_texture = ea_asset("world topo"),
    displacementScale = 0.04
  ) %>% 
  e_bar_3d(lat, value, coord_system = "globe") %>% 
  e_visual_map(show = TRUE)

data <- 
  allFlights %>% 
  filter(day < "2020-04-01"& day > "2020-03-30") %>% 
  select(origin, destination) %>% 
  group_by(origin, destination) %>% 
  summarize(cnt = n()) %>% 
  ungroup() %>% 
  left_join(airportData %>% select(icao, dest_country = country, end_lat = lat, end_lon = lon), by = c("destination" = "icao")) %>% 
  left_join(airportData %>% select(icao, origin_country = country, start_lat = lat, start_lon = lon), by = c("origin" = "icao")) %>% 
  mutate(start_lat = round(start_lat, 5),
         start_lon = round(start_lon, 5),
         end_lat = round(end_lat, 5),
         end_lon = round(end_lon, 5)
         )
    
data %>% 
  filter((origin_country != "GB" & dest_country == "GB")) %>%
  e_charts() %>% 
  e_globe(
    environment = ea_asset("starfield"),
    base_texture = ea_asset("world topo"), 
    height_texture = ea_asset("world topo"),
    displacementScale = 0.05
  ) %>% 
  e_lines_3d(
    start_lon, 
    start_lat, 
    end_lon, 
    end_lat,
    name = "flights",
    effect = list(show = TRUE)
  ) %>% 
  e_legend(FALSE)

# 
# test <- data %>% group_by(dest_country, origin_country) %>% summarize(cnt = n()) %>% ungroup()
# 
# value <- rnorm(10, 10, 2)
# 
# nodes <- data.frame(
#   name = sample(LETTERS, 10),
#   value = test$cnt,
#   size = test$cnt,
#   symbol = sample(c("circle", "rect", "triangle"), 10, replace = TRUE),
#   grp = rep(c("grp1", "grp2"), 5),
#   stringsAsFactors = FALSE
# )
# 
# 
# e_charts() %>%
#   e_graph() %>%
#   e_graph_nodes(nodes, name, value, size, grp) # %>%
#   # e_graph_edges(edges, source, target)
