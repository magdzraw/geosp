library(sf)
library(dplyr)
library(readxl)
library(tidyr)
library(dplyr)
library(openxlsx)
library(leaflet)
library(RColorBrewer)
library(data.table)
library(htmlwidgets)

#"shp" es la subcarpeta de su directorio donde se va a almacenar el archivo
#descargado desde mi repositorio de github. Si no tiene la carpeta debe crearla

download.file(
  "https://github.com/magdzraw/geosp/raw/refs/heads/main/shape_mundial.zip",
  destfile = "shp/shape_mundial.zip"
)

#Para descomprimir el archivo. O puede hacerlo manualmente 
#y guardar los 4 archivos en la carpeta shp

system("unzip shp/shape_mundial.zip")


world_sf <- read_sf(paste0(
  getwd(), "/shp/",
  "TM_WORLD_BORDERS_SIMPL-0.3.shp"
))

df <- read.xlsx("lacforgen.xlsx")

df$País.de.origen.del.estudio[df$País.de.origen.del.estudio == "Brazil"] <- "Brasil"
world_sf$NAME[world_sf$NAME == "Brazil"] <- "Brasil"
df2 <- data.frame(
  Investigador = df$Nombre.del.investigador.o.responsable.del.estudio,
  Institucion = df$`Institución/organización`,
  NAME = df$País.de.origen.del.estudio,
  Especie = df$Especie.arborea.investigada,
  Nombre_comun = df$Nombre.común.de.la.especie,
  Estudio = df$Tipo.de.estudio.genético.realizado,
  Marcadores = df$Marcadores.moleculares.utilizados,
  stringsAsFactors = T
  
)

df2 %>% 
  group_by(Especie) %>% 
  summarise(no_rows = length(Especie)) %>% print(n = 10)

dftd <- df2 %>% 
  separate(Especie, c("Genero", "epiteto"), sep = " ") %>% 
  select(-'epiteto') %>% 
  mutate(Especie = df2$Especie) %>% 
  group_by(NAME)

dftd %>% 
  group_by(NAME) %>% 
  summarise(Estudios_por_pais = length(Genero))

epp <- dftd %>% 
  select('NAME', 'Genero') %>% 
  group_by(NAME) %>% 
  summarise(Estudios_por_pais = length(Genero)) %>% 
  arrange(desc(Estudios_por_pais))

tip_est <- dftd %>% 
  select('NAME', 'Estudio') %>% 
  group_by(NAME)
write.xlsx(tip_est, "tipos_estudio_pp.xlsx", colNames = TRUE)

ntepp <- tip_est %>%
  group_by(NAME, Estudio) %>%
  summarise(No_estudios = n(), .groups = "drop") %>%
  pivot_wider(names_from = Estudio, values_from = No_estudios, values_fill = 0)

write.xlsx(ntepp, "conteo_por_pais.xlsx", colNames = TRUE)

est_res <- ntepp %>% 
  left_join(epp, by = 'NAME')
setnames(est_res, "Estudios_por_pais", "Total de estudios")

mapdata <- left_join(world_sf, est_res, by="NAME")

mapdata1 <- mapdata %>% filter(!is.na(mapdata$`Total de estudios`))


dftd <- dftd %>% 
  select('NAME','Especie','Genero',
         'Nombre_comun','Marcadores')

colbins <- c(0, 3, 9, 12, 15, 18, 21, 24, 27, 30)
paletacol <- colorBin(
  palette = "YlOrRd", domain = mapdata1$`Total de estudios`,
  na.color = "transparent", bins = colbins
)
paletacol(27)


etiq <- paste(
  "País: ", mapdata1$NAME, "<br/>",
  "Total de estudios: ", mapdata1$`Total de estudios`, "<br/>",
  "Diversidad genética: ", mapdata1$`Diversidad genética`, "<br/>",
  "Estructura genética: ", mapdata1$`Estructura genética`, "<br/>",
  "Flujo génico: ", mapdata1$`Flujo génico`, "<br/>",
  "Filogenética: ", mapdata1$Filogenética, "<br/>",
  "Filogeografía: ", mapdata1$Filogeografía, "<br/>",
  "Otros: ", mapdata1$Otros, "<br/>",
  
  sep = ""
) %>% 
  lapply(htmltools::HTML)


coromapfin <- leaflet(mapdata1) %>% 
  addTiles() %>% 
  setView(lat = -19.0, lng = -68.0, zoom = 3) %>% 
  addPolygons(
    fillColor = ~paletacol(`Total de estudios`),
    stroke = FALSE, 
    fillOpacity = 0.3,
    color = "",
    weight = "0.2",
    label = etiq,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>% 
  addLegend(
    pal = paletacol,
    values = ~`Total de estudios`, opacity = 0.3,
    title = "Número de estudios", position = "bottomleft"
  )
coromapfin
saveWidget(coromapfin, file = "Mapa_int_LACFORGEN.html", selfcontained = TRUE)


