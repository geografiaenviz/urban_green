# urban_green
Code to calculate and visualize a vegetation index (NDVI or other multispectral index) applied to urban neighborhoods. The script processes satellite imagery to extract index values per neighborhood, identifying areas with higher vegetation cover and classifying results for spatial analysis.
# --------------------------------------------------------------
# Análisis de Índice de Vegetación Ajustado al Suelo (MSAVI)
# Autor: Jorge Ibares
# Fecha: 2025-08-21
# Descripción: Este script extrae estadísticas de un raster MSAVI
#              para polígonos (manzanas/colonias), calcula densidad
#              de vegetación y clasifica áreas por cuartiles.
# --------------------------------------------------------------

# ===== 1. CARGA DE LIBRERÍAS =====
library(raster)          # Manejo de datos raster
library(sf)              # Manejo de datos vectoriales (shapefiles)
library(exactextractr)   # Extracción exacta de valores de raster por polígono
library(dplyr)           # Manipulación de datos
library(ggplot2)         # Visualización de gráficos

# ===== 2. CARGA DE DATOS =====
# Raster MSAVI
MSAVI <- raster("RUTA")
# Shapefile de manzanas o polígonos de interés
Manzanas <- st_read("RUTA")

# ===== 3. EXTRACCIÓN DE ESTADÍSTICAS POR POLÍGONO =====
# Calcula mínimo, máximo y media del MSAVI dentro de cada polígono
estadisticas <- exact_extract(MSAVI, Manzanas, c("min", "max", "mean"))
estadisticas
# Añade las estadísticas al shapefile
Manzanas <- Manzanas %>%
  mutate(min_MSAVI = estadisticas$min,
         max_MSAVI = estadisticas$max,
         mean_MSAVI = estadisticas$mean)

# Guardar shapefile con estadísticas
# st_write(Manzanas, "F:/GITHUB/Multiespectral/MSAVI_stats_colonias/Colonias_STATSMSAVI.shp")

# ===== 4. CÁLCULO DE DENSIDAD DE VEGETACIÓN =====
# Se considera la densidad como valor medio de MSAVI por unidad de área
Manzanas$Densidad_MSAVI <- Manzanas$mean_MSAVI / Manzanas$AREA

# Guardar shapefile con densidades
st_write(Manzanas, "RUTA", delete_dsn = TRUE)



# Calcular área en m² para cada polígono
Manzanas <- Manzanas %>%
  mutate(AREA = st_area(geometry)) %>%
  mutate(AREA = as.numeric(AREA)) # convertir a numérico (no unidades)

# Ahora sí puedes agrupar y resumir
area_por_cuartil <- Manzanas %>%
  group_by(Cuartil_MSAVI) %>%
  summarise(Area_Total_m2 = sum(AREA, na.rm = TRUE))


# ===== 5. CLASIFICACIÓN POR CUARTILES =====
# Calcular cuartiles de la media de MSAVI
cuartiles <- quantile(Manzanas$mean_MSAVI, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# Asignar cada manzana a un cuartil
Manzanas <- Manzanas %>%
  mutate(
    Cuartil_MSAVI = cut(mean_MSAVI,
                        breaks = cuartiles,
                        include.lowest = TRUE,
                        labels = c("S/V", "Baja", "Media", "Alta"))
  )

# Calcular área total por cuartil
area_por_cuartil <- Manzanas %>%
  group_by(Cuartil_MSAVI) %>%
  summarise(Area_Total_m2 = sum(AREA, na.rm = TRUE))

# ===== 6. VISUALIZACIÓN =====
# Gráfico 

ggplot(Manzanas, aes(x = mean_MSAVI)) +
  geom_density(fill = "#81C784", alpha = 0.6) +
  labs(title = "Densidad de MSAVI en colonias",
       x = "MSAVI medio",
       y = "Densidad") +
  theme_minimal()


ggplot(Manzanas, aes(x = mean_MSAVI, weight = AREA)) +
  geom_histogram(bins = 30, fill = "#4CAF50", color = "white") +
  labs(title = "Distribución de MSAVI ponderada por área de colonia",
       x = "MSAVI medio",
       y = "Área total (m²)") +
  theme_minimal()




# ===== 7. EXPORTAR RESULTADOS PARA REPPLICABILIDAD =====
# Guardar tabla resumen
write.csv(area_por_cuartil, "RUTA", row.names = FALSE)

# ===== 7. TOP 10 COLONIAS CON MÁS VEGETACIÓN =====
# Ordenar colonias por valor medio de MSAVI y quedarnos con las 10 más altas
top10_colonias <- Manzanas %>%
  st_drop_geometry() %>%   # quitamos geometría para trabajar solo con atributos
  select(nom_col, mean_MSAVI, AREA) %>%
  arrange(desc(mean_MSAVI)) %>%
  slice_head(n = 10)

# Gráfico de barras con las colonias
ggplot(top10_colonias, aes(x = reorder(nom_col, mean_MSAVI), y = mean_MSAVI)) +
  geom_bar(stat = "identity", fill = "#2E7D32") +
  coord_flip() +
  labs(title = "Top 10 colonias con mayor índice MSAVI medio",
       x = "Colonia",
       y = "MSAVI medio") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))








