library(httr)
library(ggplot2)
library(stringr)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(zoo)
library(dlnm)
library(splines)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(leaflet.minicharts)
library(leaflet.providers)
library(htmlwidgets)
library(leaftime)
library(sf)
library(geojsonio)
library(mapview)
library(leafpop)
library(rnaturalearth)
library(rnaturalearthdata)
library(geodata)
library(manipulateWidget)
library(shiny)
library(ggpubr)
library(Hmisc)
library(mgcv)
library(rnaturalearth)
library(rnaturalearthhires)
library(spdep)
library(lme4)
library(MuMIn)

#Bases de datos 
distritos <- read_csv("data/districts_2017census.csv")
poblacion_distritos <- read_csv("data/population_2017-2022.csv")
temperatura_minima<- read_csv("data/mintemp_20170101-20221231.csv")
casos_dengue<-read_csv("data/datos_abiertos_vigilancia_dengue.csv")


fechas<- temperatura_minima %>%
  pivot_longer(cols = starts_with("mintemp_"), names_to = "fecha", values_to = "temperatura") %>%
  mutate(fecha = str_remove(fecha, "mintemp_")) %>%
  mutate(fecha = as.Date(fecha, format = "%Y%m%d")) %>%
  mutate( ano = year(fecha),
          mes = month(fecha),
          semana = epiweek(fecha))

temperatura_minima_semanal <- fechas %>%
  group_by(ubigeo, ano, mes, semana) %>%
  summarise(temperatura_promedio = mean(temperatura, na.rm = TRUE), .groups = 'drop')

casos_year_semana <- casos_dengue %>%
  group_by(ubigeo, ano, semana) %>%
  summarise(n_casos = n(), .groups = 'drop') %>%
  left_join(temperatura_minima_semanal, by = c("ubigeo", "ano", "semana")) %>%
  mutate(ano_mes = format( make_date(ano, mes), "%b-%Y"))%>%
  left_join(poblacion_distritos, by = c("ubigeo", "ano" = "year")) %>%
  mutate(incidencia = (n_casos / population) * 100000) %>%
  left_join(distritos, by = "ubigeo")

loreto_casos_17_22 <- casos_year_semana %>% filter(departmento=="LORETO" & ano >= 2017) 

get_coordinates <- function(ubigeo, distrito, provincia, departamento) {
  query <- paste(distrito, provincia, departamento, "Peru", sep = ", ")
  url <- paste0("https://nominatim.openstreetmap.org/search?q=", URLencode(query), "&format=json&limit=1")
  res <- GET(url)
  data <- fromJSON(content(res, "text"), flatten = TRUE)
  if (length(data) == 0) {
    return(data.frame(latitud = NA, longitud = NA))
  } else {
    return(data.frame(latitud = as.numeric(data$lat), longitud = as.numeric(data$lon)))
  }
}

distritos_loreto <- distritos %>% filter(departmento == "LORETO") %>%
  rowwise() %>%
  mutate(coords = list(get_coordinates(ubigeo, distrito, provincia, departmento))) %>%
  unnest(cols = c(coords)) %>% select(1, 6, 7)

loreto_casos_17_22 <- loreto_casos_17_22 %>% left_join(distritos_loreto, by = c("ubigeo"))

peru_districts <- geodata::gadm("Peru", level = 3, path = tempdir())

distritos_loreto_sf <- st_as_sf(peru_districts) %>% filter(NAME_1 == "Loreto")  %>%
  mutate(NAME_3 = toupper(NAME_3))

merged_data <- distritos_loreto_sf %>%
  left_join(loreto_casos_17_22, by = c("NAME_3" = "distrito"))  %>%
  drop_na(semana)

pal <- colorNumeric(palette = "plasma", domain = merged_data$temperatura_promedio)

ui1 <- fluidPage(
  fluidRow(
    column(4,
           selectInput("selected_year", "Año:",
                       choices = sort(unique(merged_data$ano)), selected = 2021)
    ),
    column(4,
           sliderInput("selected_week", "Semana:",
                       min = min(merged_data$semana),
                       max = max(merged_data$semana),
                       value = min(merged_data$semana),
                       step = 1,
                       sep = "")
    )
  ),
  leafletOutput("map")
)
# Definir server
server1 <- function(input, output) {
  filtered_data <- reactive({
    merged_data %>%
      filter(ano == input$selected_year, semana == input$selected_week)
  })
  output$map <- renderLeaflet({
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addPolygons(color = 'black',
                  weight = 1,
                  opacity = 1,
                  fillColor = ~pal(temperatura_promedio),
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "red", weight = 3),
                  popup = ~paste(NAME_3, "<br>Tmin (°C):", round(temperatura_promedio, 2),"<br>Incidencia:", round(incidencia,2))) %>%
      addLegend(pal = pal, values = merged_data$temperatura_promedio, title = "T (°C)")
  })
}

shinyApp(ui = ui1, server = server1)