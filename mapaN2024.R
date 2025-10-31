## El shapefile está en https://pautas.ine.mx/transparencia/mapas/

## Paquetes
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)   # paleta
library(stringr)

shp <- st_read("~/Dropbox/Proyectos en curso/Fragmentacion electoral/DISTRITO_FEDERAL.shp", quiet = TRUE)

#------------------------------------------------------------
#    Leer datos
#------------------------------------------------------------
datos <- resultados2024.distritos %>%
  mutate(
    edo_dto = as.character(edo_dto),
    edo_dto = str_replace_all(edo_dto, "\\s", "")
  )


#------------------------------------------------------------
# Unir el shape con los datos
#------------------------------------------------------------

shp$edo_dto <- paste(shp$ENTIDAD, sep = ".", shp$DISTRITO_F)

mapa <- shp %>%
  left_join(datos %>% select(edo_dto, N), by = "edo_dto")

#------------------------------------------------------------
#  Mapa
#------------------------------------------------------------
MapaN <- ggplot(mapa) +
  geom_sf(aes(fill = N), color = NA) +
  scale_fill_viridis(
    option = "F",
    name = TeX(r"($N_v$)")
  ) +
  coord_sf(datum = NA) +
  labs(
    # title    = "México 2024, elecciones para diputados federales: Número efectivo de partidos",
    # subtitle = "Distritos uninominales por mayoría simple (n = 300)",
    caption  = "Las coaliciones se toman como un sólo partido\nMedia = 2.3\n
    Fuente: Cálculos propios a partir de cómputos distritales del INE (2024)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )

print(MapaN)

#------------------------------------------------------------
# Guardar a archivo
#------------------------------------------------------------
# ggsave("mapa_N_efectivo_distritos.png", MapaN, width = 10, height = 7, dpi = 300)
