#### ============================================================
### Punto 3 - Construcción de variables espaciales
#### ============================================================

# === Establecer el directorio de trabajo ===
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

## 0) Paquetes y opciones generales -------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  stringi,
  rio,
  leaflet,
  here,
  osmdata,
  sf,
  lwgeom,
  doParallel,
  foreach,
  httr2,
  curl
)

# Se desactiva s2 para evitar problemas en joins espaciales
sf::sf_use_s2(FALSE)


#### ============================================================
### 1) Funciones auxiliares
#### ============================================================

## 1.1) Función para extraer datos desde OpenStreetMap -------------------------

obtener_osmdata <- function(llave, valor, tipo_dato) {
  
  # Se arma la consulta para el bounding box de Bogotá
  consulta_osm <- osmdata::opq(bbox = osmdata::getbb("Bogotá Colombia")) |>
    osmdata::add_osm_feature(key = llave, value = valor)
  
  # Se descarga el objeto completo en formato sf
  objeto_sf <- osmdata::osmdata_sf(consulta_osm)
  
  # Según el tipo de geometría, se escoge la capa correspondiente
  sf_filtrado <- dplyr::case_when(
    tipo_dato == "linea"   ~ objeto_sf$osm_lines,
    tipo_dato == "puntos"  ~ objeto_sf$osm_points,
    tipo_dato == "poligono" ~ objeto_sf$osm_polygons,
    TRUE                   ~ stop("tipo_dato debe ser 'linea', 'puntos' o 'poligono'")
  ) |>
    dplyr::select(osm_id, name) |>
    sf::st_as_sf(crs = 4326) |>
    sf::st_transform(crs = 4326) |>
    sf::st_make_valid()
  
  return(sf_filtrado)
}

## 1.2) Distancia mínima a un conjunto de features -----------------------------

distneastfeat <- function(data_original, data_feat, n_variable, tipo_dato) {
  
  # Si los features son polígonos, se trabaja con sus centroides
  if (identical(tipo_dato, "poligono")) {
    data_feat <- sf::st_centroid(data_feat, byid = TRUE)
  }
  
  # Matriz de distancias entre cada punto de data_original y cada feature
  matriz_dist <- sf::st_distance(x = data_original, y = data_feat)
  
  # Se toma la distancia mínima por observación
  dist_min   <- apply(matriz_dist, 1L, min)
  data_original[[n_variable]] <- dist_min
  
  return(data_original)
}

## 1.3) Distancia mínima punto-a-conjunto (loop fila a fila) -------------------

distminpoints <- function(data_original, ext_points) {
  
  n_obs   <- nrow(data_original)
  dist_min <- vector(mode = "numeric", length = n_obs)
  
  for (i in seq_len(n_obs)) {
    dist_vec    <- sf::st_distance(data_original[i, ], ext_points)
    dist_min[i] <- min(dist_vec)
  }
  
  return(dist_min)
}


#### ============================================================
### 2) Datos base de apartamentos
#### ============================================================

## 2.1) Lectura de train y test, y construcción de objeto sf -------------------

train_texto <- readRDS("stores/train_texto.rds")
test_texto  <- readRDS("stores/test_texto.rds")
datos <- bind_rows(test_texto, train_texto)

# Se convierten las coordenadas lon/lat a objeto espacial
datos <- sf::st_as_sf(
  datos,
  coords = c("lon", "lat"),
  crs    = 4326
)


#### ============================================================
### 3) Geografía de Bogotá: localidades y sectores
#### ============================================================

## 3.1) Localidades ------------------------------------------------------------

localidades <- sf::st_read("stores/Localidad/Loca.shp", quiet = TRUE) |>
  dplyr::select(LocNombre) |>
  sf::st_transform(crs = 4326)

datos <- sf::st_join(datos, localidades, join = sf::st_within)

## 3.2) Sectores ---------------------------------------------------------------

sector <- sf::st_read("stores/Sector/SECTOR1.shp", quiet = TRUE) |>
  sf::st_transform(crs = 4326) |>
  dplyr::select(SCANOMBRE) |>
  sf::st_make_valid()

datos <- sf::st_join(datos, sector, join = sf::st_within)


#### ============================================================
### 4) Datos abiertos de Bogotá
#### ============================================================

## 4.1) Lectura de shapefiles temáticos ---------------------------------------

indicador_loc    <- sf::st_read("stores/Indicador Localidad/IndLocalidad.shp", quiet = TRUE)
indicador_upz    <- sf::st_read("stores/Indicador UPZ/IndUPZ.shp", quiet = TRUE)
luminarias_upz   <- sf::st_read("stores/Luminaria/Luminarias_UPZ.shp", quiet = TRUE)
seguridad_nocturna <- sf::st_read("stores/Seguridad Nocturna/PuntosSeguridadNocturna.shp", quiet = TRUE)
bibliotecas      <- sf::st_read("stores/Bibliotecas/RedBibliotecAcademica.shp", quiet = TRUE)
museo            <- sf::st_read("stores/Museos/Museo.shp", quiet = TRUE)
colegios         <- sf::st_read("stores/Colegios/colegios06_2025.shp", quiet = TRUE)
recaudo_predial  <- sf::st_read("stores/Recaudo Predial/RPREDIAL.shp", quiet = TRUE)
manzanas_estr    <- sf::st_read("stores/Estratificacion/ManzanaEstratificacion.shp", quiet = TRUE)
sitp             <- sf::st_read("stores/SITP/PSITP.shp", quiet = TRUE)
delitos          <- sf::st_read("stores/Delitos/DAILoc.shp", quiet = TRUE)
restbar          <- sf::st_read("stores/Bares/EGBa.shp", quiet = TRUE)

## 4.2) Corrección de CRS y coordenadas de colegios ---------------------------

cat("Ajustando coordenadas y CRS de COLEGIOS...\n")

coords_col  <- sf::st_coordinates(colegios)

mascara_ok <- coords_col[, "X"] > -80 & coords_col[, "X"] < -70 &
  coords_col[, "Y"] >   3 & coords_col[, "Y"] <   6

colegios <- colegios[mascara_ok, , drop = FALSE]
sf::st_crs(colegios) <- 4326

cat("Bounding box corregido de colegios:\n")
print(sf::st_bbox(colegios))

## 4.3) Estaciones de Transmilenio (WFS) --------------------------------------

transmi_raw <- sf::st_read(
  "https://gis.transmilenio.gov.co/arcgis/services/Troncal/consulta_estaciones_troncales/MapServer/WFSServer?request=GetCapabilities&service=WFS",
  quiet = TRUE
)

transmi <- transmi_raw |>
  as.data.frame() |>
  dplyr::select(-Shape) |>
  sf::st_as_sf(coords = c("LONGITUD", "LATITUD"), crs = 4326) |>
  sf::st_make_valid()

## 4.4) Homogeneización de CRS a 4326 -----------------------------------------

lista_capas <- c(
  "colegios", "indicador_loc", "indicador_upz", "luminarias_upz",
  "seguridad_nocturna", "bibliotecas", "museo", "recaudo_predial",
  "manzanas_estr", "sitp", "delitos", "restbar"
)

for (nm in lista_capas) {
  cat("Transformando CRS (si aplica) para:", nm, "...\n")
  
  capa <- get(nm)
  epsg_actual <- try(sf::st_crs(capa)$epsg, silent = TRUE)
  
  if (!inherits(epsg_actual, "try-error") && !is.na(epsg_actual) && epsg_actual != 4326) {
    capa <- sf::st_transform(capa, crs = 4326)
  }
  
  assign(nm, capa)
}


#### ============================================================
### 5) Unión de capas y construcción de variables espaciales
#### ============================================================

## 5.1) Indicadores de espacio público (localidad y UPZ) -----------------------

indicador_loc_sel <- indicador_loc |>
  dplyr::select(EPE, EPT, EPCC, geometry)

datos <- sf::st_join(datos, indicador_loc_sel, join = sf::st_within)

indicador_upz_sel <- indicador_upz |>
  dplyr::select(EPE__m2_ha, geometry, CODIGO_UPZ, NOMBRE) |>
  dplyr::rename(
    EPE_UPZ   = EPE__m2_ha,
    NOMBRE_UPZ = NOMBRE
  )

datos <- sf::st_join(datos, indicador_upz_sel, join = sf::st_within)

## 5.2) Luminarias por UPZ -----------------------------------------------------

luminarias_upz_sel <- luminarias_upz |>
  dplyr::select(geometry, TOTAL) |>
  dplyr::rename(luminarias = TOTAL)

datos <- sf::st_join(datos, luminarias_upz_sel, join = sf::st_within)

## 5.3) Distancias a equipamientos puntuales ----------------------------------

# Bibliotecas
datos <- distneastfeat(datos, bibliotecas, "distnearestlibrary", "puntos")

# Colegios
datos <- distneastfeat(datos, colegios, "distnearestschool", "puntos")

# Museos
datos <- distneastfeat(datos, museo, "distnearestmuseum", "puntos")

# Estaciones de Transmilenio
datos <- distneastfeat(datos, transmi, "distnearesttransmi", "puntos")

## 5.4) Recaudo predial por polígono más cercano --------------------------------

recaudo_predial_sel <- recaudo_predial |>
  dplyr::select(geometry, Sum_VALOR_) |>
  dplyr::rename(recaudo_predial = Sum_VALOR_)

datos <- sf::st_join(datos, recaudo_predial_sel, join = sf::st_within)

## 5.5) Estrato de la manzana más cercana -------------------------------------

manzanas_estr_sel <- manzanas_estr |>
  dplyr::select(geometry, ESTRATO)

datos <- sf::st_join(datos, manzanas_estr_sel, join = sf::st_nearest_feature)

# Se usa vecindad más cercana para asignar estrato
tmp_idx <- sf::st_nearest_feature(datos, manzanas_estr_sel)

datos <- sf::st_join(datos, manzanas_estr_sel, join = sf::st_nearest_feature)

## 5.6) Distancia a paraderos SITP --------------------------------------------

datos <- distneastfeat(datos, sitp, "distnearestsitp", "puntos")


#### ============================================================
### 6) Construcción de agregados de delitos por localidad
#### ============================================================

## 6.1) Limpieza de columnas y renombramiento ----------------------------------

idx_quitar <- grep("TOT|VAR|22|18|24|23", names(delitos))
delitos    <- delitos[, -idx_quitar]

nombres_anteriores <- c(
  "CMLP", "CMHP", "CMHR", "CMHA", "CMHB", "CMHCE", "CMHM", "CMDS", "CMVI"
)

reemplazos <- c(
  "lesionesperson", "hurtopersonas", "hurtosresidencias",
  "hurtosautos", "hurtosbicis", "hurtoscel", "hurtosmotos",
  "delitos_sexual", "violencia_intra"
)

nuevos_nombres <- stringr::str_replace_all(
  names(delitos),
  setNames(reemplazos, nombres_anteriores)
)

nuevos_nombres <- gsub("CMHC", "hurtoscomercio", nuevos_nombres)
nuevos_nombres <- gsub("CMH",  "homicidios",     nuevos_nombres)

colnames(delitos) <- nuevos_nombres

## 6.2) Promedios de delitos (2019, 2020, 2022) -------------------------------

delitos_df <- as.data.frame(delitos)

delitos <- delitos %>%
  mutate(
    n_homicidios      = rowMeans(across(starts_with("homicidios")), na.rm = TRUE),
    n_lesiones        = rowMeans(across(starts_with("lesionesperson")), na.rm = TRUE),
    n_hurtopersonas   = rowMeans(across(starts_with("hurtopersonas")), na.rm = TRUE),
    n_hurtosautos     = rowMeans(across(starts_with("hurtosautos")), na.rm = TRUE),
    n_hurtosresidencias = rowMeans(across(starts_with("hurtosresidencias")), na.rm = TRUE),
    n_hurtosbicis     = rowMeans(across(starts_with("hurtosbicis")), na.rm = TRUE),
    n_hurtosmotos     = rowMeans(across(starts_with("hurtosmotos")), na.rm = TRUE),
    n_hurtoscomercio  = rowMeans(across(starts_with("hurtoscomercio")), na.rm = TRUE),
    n_hurtoscelular   = rowMeans(across(starts_with("hurtoscel")), na.rm = TRUE),
    n_delitossexales  = rowMeans(across(starts_with("delitos_sexual")), na.rm = TRUE),
    n_violenciaintra  = rowMeans(across(starts_with("violencia_intra")), na.rm = TRUE)
  )


delitos_loc <- delitos_df |>
  dplyr::rename(
    n_localidad = CMIULOCAL,
    localidad   = CMNOMLOCAL
  ) |>
  dplyr::select(localidad, dplyr::starts_with("n_")) |>
  dplyr::mutate(
    localidad = chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", localidad),
    localidad = tolower(localidad)
  )

## 6.3) Unión con la base principal por nombre de localidad -------------------

datos <- datos |>
  dplyr::rename(localidad = LocNombre) |>
  dplyr::mutate(localidad = tolower(localidad))

datos <- dplyr::left_join(datos, delitos_loc, by = "localidad")


#### ============================================================
### 7) Restaurantes / bares por estrato y distancia
#### ============================================================

## 7.1) Asignar estrato a cada establecimiento y contar -----------------------

manzanas_rest <- manzanas_estr |>
  dplyr::select(ESTRATO, geometry)

restbar_join <- sf::st_join(restbar, manzanas_rest, join = sf::st_nearest_feature)

num_restbar <- restbar_join |>
  dplyr::group_by(ESTRATO) |>
  dplyr::summarise(
    num_restaurantes_manz = dplyr::n_distinct(RegNTurism),
    .groups = "drop"
  )

datos <- sf::st_join(datos, num_restbar, join = sf::st_nearest_feature)

## 7.2) Distancia al restaurante/bar más cercano ------------------------------

datos <- distneastfeat(datos, restbar, "distrestaurantebar", "puntos")

## 7.2) Sacando la variables geometry ------------------------------


datos <- datos %>% mutate(
  lon = st_coordinates(geometry)[,1],
  lat = st_coordinates(geometry)[,2]
)  %>% 
  st_drop_geometry()

## 7.3) Sacando variables que no usaremos ------------------------------


datos <- datos %>% select(-c(ESTRATO.x,ESTRATO.y,operation_type,
                             title,description,text_raw))


#### ============================================================
### 8) Exportar base final enriquecida
#### ============================================================
## 8.2) retomando el id de train y test ------------------------------
data_train <- read.csv("stores/train.csv")

data_test <- read.csv("stores/test.csv")


data_train_final <- datos %>% filter(property_id %in% data_train$property_id)

data_test_final <- datos %>% filter(property_id %in% data_test$property_id)


rio::export(datos, "stores/datos_unidos.rds")
write.csv(datos, "stores/datos_unides.csv")
write.csv(data_train_final, "stores/train_final.csv")
write.csv(data_test_final, "stores/test_final.csv")