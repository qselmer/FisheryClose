rm(list = ls()); gc(reset = TRUE)

# -----------------------------------------------------------------------------------------------------------------



library(dplyr)
library(ggplot2)
library(purrr)

# Parámetros
lat_min <- 10
lat_max <- 12
lon_min <- -80
lon_max <- -78
resolucion <- 0.1  # grados

# Parámetros de lances
n_centros <- 4
n_puntos_total <- 200
pesos <- runif(n_centros, 10, 20)
puntos_por_centro <- round(pesos / sum(pesos) * n_puntos_total) # número de puntos por centro
desviacion <- 0.1  # cuán "cerrado" está el grupo


# Crear vectores
latitudes <- seq(lat_min + resolucion/2, lat_max - resolucion/2, by = resolucion)  # Centrado
longitudes <- seq(lon_min + resolucion/2, lon_max - resolucion/2, by = resolucion)  # Centrado

# Crear la grilla con ID y coordenadas
grilla <- expand.grid(lat_1 = latitudes, lon_1 = longitudes)
grilla$lat_2 <- grilla$lat_1 + resolucion
grilla$lon_2 <- grilla$lon_1 + resolucion
grilla$area <- (grilla$lat_2 - grilla$lat_1) * (grilla$lon_2 - grilla$lon_1)

# Calcular las coordenadas de los centroides
grilla$lat_centroid <- (grilla$lat_1 + grilla$lat_2) / 2
grilla$lon_centroid <- (grilla$lon_1 + grilla$lon_2) / 2

grilla$grilla_id <- paste("G", seq_len(nrow(grilla)), sep = "-")

# Reordenar columnas
grilla <- grilla[, c("grilla_id", "lat_1", "lat_2", "lon_1", "lon_2", "lat_centroid", "lon_centroid", "area")]

# Mostrar
print(grilla)

# Graficamos la grilla con los centroides y sus IDs
g1 <- ggplot(grilla) +
  geom_rect(aes(xmin = lon_1, xmax = lon_2, ymin = lat_1, ymax = lat_2),
            color = "black", alpha = 0.3, fill = "gray") +
  # geom_point(aes(x = lon_centroid, y = lat_centroid), color = "red", size = 2) +  # Agregar centroides
  geom_text(aes(x = lon_centroid, y = lat_centroid, label = grilla_id),
            color = "1", size = 3,  vjust = 0) +  # Etiquetas con grilla_id
  coord_fixed() +
  labs(title = "Grid",
       x = "Longitud", y = "Latitud", fill = "ID Grilla") +
  theme_minimal() +
  theme(legend.position = "none")

print(g1)


# -----------------------------------------------------------------------------------------------------------------


diferencia <- n_puntos_total - sum(puntos_por_centro)
if (diferencia != 0) {
  # Encuentra índices a los que se le puede sumar o restar 1
  idx <- sample(1:n_centros, abs(diferencia), replace = TRUE)
  for (i in idx) {
    puntos_por_centro[i] <- puntos_por_centro[i] + sign(diferencia)
  }
}

centros <- data.frame(
  centro_id = 1:n_centros,
  lat_centro = runif(n_centros, lat_min, lat_max),
  lon_centro = runif(n_centros, lon_min, lon_max),
  puntos_por_centro = puntos_por_centro
)

# Generamos los puntos alrededor de los centros
puntos <- pmap_df(centros, function(centro_id, lat_centro, lon_centro, puntos_por_centro) {
  data.frame(
    lat = rnorm(puntos_por_centro, mean = lat_centro, sd = desviacion),
    lon = rnorm(puntos_por_centro, mean = lon_centro, sd = desviacion)
  )
}) %>%
  filter(lat >= lat_min, lat <= lat_max, lon >= lon_min, lon <= lon_max) %>%
  mutate(punto_id = row_number())

g1 +
  geom_point(data = puntos, aes(x = lon, y = lat), color = "red", size = 0.5) +
  labs(title = "") +
  theme(legend.position = "none")


# Paso 2: Asociar cada punto con su celda
# Hacemos un join cruzado y filtramos por los límites de cada celda
puntos_grilla <- puntos %>%
  mutate(punto_id = row_number()) %>%
  # Usamos una combinación cruzada entre los puntos y la grilla
  tidyr::crossing(grilla) %>%
  filter(lat >= lat_1 & lat < lat_2 & lon >= lon_1 & lon < lon_2) %>%
  select(punto_id, lat, lon, grilla_id)

# Verificamos la tabla resultante
head(puntos_grilla)

g1 +
  geom_point(data = puntos_grilla, aes(x = lon, y = lat, color = grilla_id), size = 2.5) +
  # geom_text(data = puntos_grilla, aes(x = lon, y = lat, label = grilla_id),
  #           color = "black", size = 3)+
  labs(title = "") +
  theme(legend.position = "none")

puntos_grilla$lance_posi <- rbinom(nrow(puntos_grilla), 1, 0.5)  # Generar variable binaria
puntos_grilla$lance_posi <- factor(puntos_grilla$lance_posi)
# -----------------------------------------------------------------------------------------------------------------

g1 +
  geom_point(data = puntos_grilla, aes(x = lon, y = lat,
                                       color = lance_posi, shape = lance_posi),
             size = 2.5) +
  scale_shape_manual(values = c("0" = 16, "1" = 16)) +
  scale_color_manual(values = c("0" = "gray30", "1" = "red"))+
  labs(title = "") +
  theme(legend.position = "none")


# -----------------------------------------------------------------------------------------------------------------
sp <- "atun"
marks <- seq(100, 300, 5)
juv_mark <- 180

# tres criterios de cierre
bycatch <- TRUE
juveniles <- TRUE
desove <- TRUE


generar_estructura_tallas <- function() {
  # Simula una distribución normal centrada alrededor de una talla media aleatoria
  talla_media <- sample(seq(120, 200, 5), 1)
  sd_talla <- sample(seq(10, 30, 5), 1)

  # Frecuencia aleatoria con distribución normal sobre 'marks'
  freq <- dnorm(marks, mean = talla_media, sd = sd_talla)
  freq <- round((freq / sum(freq)) * sample(100:500, 1))  # total de individuos por lance
  names(freq) <- marks
  return(freq)
}

calcular_porcentaje_juveniles <- function(estructura_tallas, juv_marks = juv_mark) {
  total <- sum(estructura_tallas, na.rm = TRUE)
  if (total == 0) return(NA)

  juveniles <- sum(estructura_tallas[as.numeric(names(estructura_tallas)) < juv_marks], na.rm = TRUE)
  return(round(juveniles / total, 3))
}

lances <- puntos_grilla %>%
  filter(lance_posi == 1) %>%
  mutate(
    especie_objetivo = sp,
    captura_total_kg = rgamma(n(), shape = 3, rate = 0.5),
    esfuerzo_minutos = sample(10:60, n(), replace = TRUE),
   )

lances <- lances %>%
  rowwise() %>%
  mutate(
    estructura_tallas_mm = list(generar_estructura_tallas())
  ) %>%
  ungroup()


if (bycatch) {
  lances <- lances %>%
    mutate(
      bycatch_presente = rbinom(n(), 1, 0.3) == 1
    ) %>%
    rowwise() %>%
    mutate(
      bycatch_sp = ifelse(bycatch_presente, sample(c("Jurel", "Caballa", "Tortuga", "Delfín"), 1), NA)
    ) %>%
    ungroup()
}


lances <- lances %>%
  mutate(
    porcentaje_juveniles = map_dbl(estructura_tallas_mm, calcular_porcentaje_juveniles, juv_marks = juv_marks)
  )


if (desove) {
  lances <- lances %>%
    mutate(
      desove_presente = rbinom(n(), 1, 0.2) == 1
    )
}

# estadisiticas
resumen <- puntos_grilla %>%
  group_by(grilla_id) %>%
  summarise(prop_positivos = mean(as.numeric(as.character(lance_posi))),
            n = n()) %>%
  left_join(grilla, by = "grilla_id")

summary(resumen)
