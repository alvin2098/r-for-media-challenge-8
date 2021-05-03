### --- R Challenge 8, Alvin Aziz, 02.05.2021 --- ###

# Challenge #8: Inzidenzzahlen der Hamburger Bezirke

# Lade den Datensatz stadtteile_wsg84.RDS
# Recherchiere die Fallzahlen der letzten sieben Tage für die Hamburger Bezirke
# https://www.hamburg.de/corona-zahlen/
# Erstelle eine leaflet Map und visualisiere die Inzidenzzahlen (Achtung: Nicht die Fallzahlen)
# Nutze dafür Shapes, Legende, Hovereffekte und Labels
# Exportiere die Map als HTML file

### --- Load libraries --- ###
library(dplyr)
library(tidyverse)
library(plotly)
library(leaflet)
library(sf)


### --- Create Dataframes and add GPS coords to districts dataframe --- ###
hamburg_districts <- readRDS("data/hamburg_districts.rds")

districts_gps <- readRDS("data/bezirke_wsg84.RDS") %>% 
  st_as_sf() %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  left_join(hamburg_districts, by = c("Bezirk_Name" = "bezirk"))


### --- Add corona data --- ###
cases_last_week <- c(298,141,238,571,248,237,455)
district_names <- c("Altona", "Bergedorf", "Eimsbüttel", "Hamburg-Mitte", "Hamburg-Nord", "Harburg", "Wandsbek")

inzidenz <- data.frame(cases_last_week, district_names)


### --- Join both dataframes, calculate Inzidenzwert --- ###
districts_gps <- districts_gps %>% 
  left_join(inzidenz, by = c("Bezirk_Name" = "district_names")) %>% 
  mutate(inzidenzwert = round(cases_last_week / einwohner * 100000))


### --- Plot map --- ###
cuts <- c(0, 25, 50, 75, 100, 125, 150, 200)
pal <- colorBin("Oranges", domain = districts_gps$inzidenzwert, bins = cuts)

labels <- sprintf("<strong>%s</strong><br>Inzidenzwert: %g<br>Neuinfektionen: %g",
                  districts_gps$Bezirk_Name, 
                  districts_gps$inzidenzwert, 
                  districts_gps$cases_last_week) %>% 
  map(htmltools::HTML)

HH_corona_map <- 
  leaflet(data = districts_gps) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = 9.993682, lat = 53.551086, zoom = 10) %>% 
  addPolygons(fillColor = ~pal(inzidenzwert),
              weight = 1,
              opacity = 0.75,
              color = "white",
              fillOpacity = 0.5,
              highlight = highlightOptions(
                weight = 3,
                color = "grey",
                bringToFront = T,
                fillOpacity = 0.5),
              label = labels) %>%
  addLegend(pal = pal, 
            values = ~inzidenzwert, 
            title = "Inzidenzwert",
            opacity = 0.5,
            position = "bottomright")


### --- Save map --- ###
htmlwidgets::saveWidget(as_widget(HH_corona_map), "coronamap_hamburg.html")
