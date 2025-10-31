## El shapefile está en https://pautas.ine.mx/transparencia/mapas/

## Paquetes
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)   # paleta
library(stringr)

shp <- st_read("~/Dropbox/Proyectos en curso/Fragmentacion electoral/DISTRITO_FEDERAL.shp", quiet = TRUE)

#------------------------------------------------------------
#    Lee tus datos (ya los tienes en resultados2024.distritos)
#    Asegúrate de que edo_dto sea carácter y esté formateado igual que en el shape
#    Ejemplo de normalización: "01-01", "02-05", etc.
#------------------------------------------------------------
datos <- resultados2024.distritos %>%
  mutate(
    edo_dto = as.character(edo_dto),
    edo_dto = str_replace_all(edo_dto, "\\s", "")
  )

#------------------------------------------------------------
# 2) Lee el shape/geojson de distritos federales (300 polígonos)
#    Cambia la ruta y el archivo a lo que tengas (shp/geojson/gpkg)
#------------------------------------------------------------
# Ejemplos de lectura (usa solo uno):
# shp <- st_read("data/INE_Distritos_2024.shp")
# shp <- st_read("data/INE_Distritos_2024.gpkg")        # capa única
# shp <- st_read("data/INE_Distritos_2024.geojson")


names(shp)
# Mira los nombres para identificar las columnas con entidad y distrito.
# En muchos insumos del INE suelen ser algo como: CVE_ENT (estado) y DIST (distrito).
# Ajusta aquí si tus nombres son distintos:

#------------------------------------------------------------
# 3) Crea una llave en el shape que empate con datos$edo_dto.
#    Ejemplo: edo_dto con formato "01-01" (estado-distrito, con ceros a la izquierda)
#------------------------------------------------------------
# shp <- shp %>%
#  mutate(
    # Ajusta estas dos columnas si tu shape usa otros nombres
#    ent_num = as.integer(as.character(CVE_ENT)),   # p.ej. 1..32
#    dto_num = as.integer(as.character(DIST)),      # p.ej. 1..25
#    edo_dto  = sprintf("%02d-%02d", ent_num, dto_num)
#  )

# Si tu llave en datos NO está en ese formato, homológala:
# datos <- datos %>%
#   separate(edo_dto, into = c("ent","dto"), sep = "[-_/]", remove = FALSE) %>%
#   mutate(edo_dto = sprintf("%02d-%02d", as.integer(ent), as.integer(dto)))

#------------------------------------------------------------
# 4) Une el shape con los datos
#------------------------------------------------------------

# Ajusto
shp$edo_dto <- paste(shp$ENTIDAD, sep = ".", shp$DISTRITO_F)

mapa <- shp %>%
  left_join(datos %>% select(edo_dto, N), by = "edo_dto")

# Checa si hubo distritos sin mach: 
faltantes_shape <- mapa %>% filter(is.na(N)) %>% nrow()
faltantes_datos <- anti_join(datos, shp %>% st_drop_geometry(), by = "edo_dto") %>% nrow()
message("Distritos sin N tras el join (en el shape): ", faltantes_shape)
message("Registros en datos sin polígono (llave no encontrada en shape): ", faltantes_datos)

#------------------------------------------------------------
# 5) Dibuja el mapa (coropleta continua con viridis)
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
# 6) (Opcional) Guardar a archivo
#------------------------------------------------------------
# ggsave("mapa_N_efectivo_distritos.png", MapaN, width = 10, height = 7, dpi = 300)
