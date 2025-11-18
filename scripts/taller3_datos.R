#### ============================================================
### Punto 1 - Paquetes y carga de bases
#### ============================================================

# === Establecer el directorio de trabajo ===
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

## 1) Se cargan o instalan los paquetes necesarios

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

pacman::p_load(
  tidyverse,
  stringr,
  stringi,
  purrr,
  here,
  tidytext
)

## 2) Se leen las bases de train y test y se unifican en un solo objeto

train <- readr::read_csv("stores/train.csv", show_col_types = FALSE) |>
  dplyr::mutate(dataset = "train")

test  <- readr::read_csv("stores/test.csv",  show_col_types = FALSE) |>
  dplyr::mutate(dataset = "test")

datos <- dplyr::bind_rows(train, test)


#### ============================================================
### Punto 2 - Texto unificado y limpieza básica
#### ============================================================

## 1) Se construye un solo campo de texto a partir de título y descripción
## 2) Se pasa todo a minúsculas, se quitan tildes, signos de puntuación
##    y espacios múltiples para facilitar las búsquedas

datos <- datos |>
  dplyr::mutate(
    text_raw = paste(
      dplyr::coalesce(title, ""),
      dplyr::coalesce(description, ""),
      sep = " "
    ),
    text_raw = stringr::str_to_lower(text_raw),
    text_raw = stringi::stri_trans_general(text_raw, "Latin-ASCII"),
    text_raw = stringr::str_replace_all(text_raw, "[[:punct:]]", " "),
    text_raw = stringr::str_squish(text_raw)
  )

datos <- datos |>
  dplyr::mutate(
    text_raw = paste(
      dplyr::coalesce(title, ""),
      dplyr::coalesce(description, ""),
      sep = " "
    ),
    text_raw = stringr::str_to_lower(text_raw),
    text_raw = stringi::stri_trans_general(text_raw, "Latin-ASCII"),
    text_raw = stringr::str_replace_all(text_raw, "[[:punct:]]", " "),
    text_raw = stringr::str_squish(text_raw)
  )

#### ============================================================
### Punto 3 - Amenidades (dummies 0/1 desde el texto)
#### ============================================================

## 1) Se define un diccionario de amenidades con patrones de búsqueda
## 2) Para cada amenidad se crea una dummy:
##    - 1 si el patrón aparece en el texto
##    - 0 en caso contrario (no se modifica luego en la imputación)

amenities_dicc <- c(
  cocina_americana       = "cocina americana",
  cocina_integral        = "cocina integral",
  gimnasio               = "gimnasio",
  balcon                 = "balcon",
  chimenea               = "chimenea",
  terraza                = "terraza",
  ascensor               = "ascensor",
  sauna                  = "sauna",
  jacuzzi                = "jacuzzi",
  piscina                = "piscina",
  deposito               = "deposito",
  walking_closet         = "walking closet|walk in closet|vestier",
  duplex                 = "duplex",
  zona_verde             = "zona verde|jardin|zona verde comunal",
  bbq                    = "bbq|parrilla|barbecue",
  conjunto_residencial   = "conjunto cerrado|conjunto residencial",
  altillo                = "altillo",
  vigilancia_24h         = "vigilancia 24|24 horas|24h",
  porteria               = "porteria",
  cctv                   = "cctv|circuito cerrado|camara de seguridad|camaras de seguridad",
  parqueadero_cubierto   = "parqueadero cubierto|garaje cubierto",
  parqueadero_comunal    = "parqueadero comunal|parqueadero visitantes|parqueaderos visitantes",
  zona_infantil          = "zona infantil|juegos infantiles|parque infantil|juegos para ninos",
  salon_comunal          = "salon comunal|salon social|salon de eventos",
  zona_humeda            = "zona humeda",
  terraza_comunal        = "terraza comunal|rooftop|terraza mirador",
  pet_friendly           = "pet friendly|mascotas permitidas|se aceptan mascotas",
  remodelado             = "remodelado|remodelacion reciente|totalmente renovado",
  piso_madera            = "piso de madera|madera laminada|laminado madera|piso laminado",
  piso_porcelanato       = "porcelanato|piso en porcelanato"
)

for (var_name in names(amenities_dicc)) {
  patron <- amenities_dicc[[var_name]]
  datos[[var_name]] <- dplyr::if_else(
    stringr::str_detect(datos$text_raw,
                        stringr::regex(patron, ignore_case = TRUE)),
    1L, 0L
  )
}


#### ============================================================
### Punto 4 - Funciones auxiliares para extraer números del texto
#### ============================================================

## 1) Función para sumar números que aparecen antes de una palabra clave
##    (por ejemplo, "2 baños", "3 garajes", etc.)
## 2) Función para sumar números asociados a unidades de área (m2, metros, ...)

extraer_sum_numero_antes <- function(texto, palabra_patron) {
  patron <- stringr::str_c(
    "(\\d+)[[:space:]]*(?:[a-z]{0,15}[[:space:]]*)?(",
    palabra_patron,
    ")"
  )
  
  matches <- stringr::str_match_all(texto, patron)[[1]]
  if (nrow(matches) == 0) return(NA_real_)
  
  nums <- as.numeric(matches[, 2])
  sum(nums, na.rm = TRUE)
}

extraer_area_texto <- function(texto) {
  patron <- "(\\d+\\.?\\d*)[[:space:]]*(m2|mt2|mts2|metros cuadrados|metro cuadrado|metros)"
  
  matches <- stringr::str_match_all(texto, patron)[[1]]
  if (nrow(matches) == 0) return(NA_real_)
  
  nums <- readr::parse_number(matches[, 1])
  sum(nums, na.rm = TRUE)
}


#### ============================================================
### Punto 5 - Variables numéricas desde el texto
#### ============================================================

## 1) Se extrae un área aproximada desde el texto (area_text)
## 2) Se extraen números de baños y parqueaderos si aparecen explícitos
## 3) Se hace una limpieza ligera de outliers extremos en área y conteos

datos <- datos |>
  dplyr::mutate(
    area_text = purrr::map_dbl(text_raw, extraer_area_texto),
    banos_text = purrr::map_dbl(
      text_raw,
      ~ extraer_sum_numero_antes(.x, "bano|banos|banio|banios")
    ),
    parqueaderos_text = purrr::map_dbl(
      text_raw,
      ~ extraer_sum_numero_antes(.x, "parqueadero|parqueaderos|garaje|garajes")
    )
  ) |>
  dplyr::mutate(
    area_text = dplyr::if_else(area_text <= 5 | area_text > 2000,
                               NA_real_, area_text),
    banos_text = dplyr::if_else(banos_text > 10, NA_real_, banos_text),
    parqueaderos_text = dplyr::if_else(parqueaderos_text > 10,
                                       NA_real_, parqueaderos_text)
  )

#### ============================================================
### Punto 6 - Imputación básica sobre la base original
#### ============================================================

## 1) Se definen los nombres de las variables originales:
##    - área construida
##    - número de baños
##    - número total de parqueaderos (nueva variable)
## 2) Para área y baños:
##    - Los valores de la base original se toman como “verdaderos”
##    - Sólo se rellenan los NA con lo extraído del texto
## 3) Para parqueaderos:
##    - Si no se menciona en el texto, se asume que NO tiene parqueaderos (0)

nombre_area         <- "surface_covered"
nombre_banos        <- "bathrooms"
nombre_parqueaderos <- "n_parqueaderos"   # nombre nuevo; cámbialo si quieres

datos <- datos |>
  dplyr::mutate(
    !!nombre_area := dplyr::if_else(
      is.na(.data[[nombre_area]]) & !is.na(area_text),
      area_text,
      .data[[nombre_area]]
    ),
    !!nombre_banos := dplyr::if_else(
      is.na(.data[[nombre_banos]]) & !is.na(banos_text),
      banos_text,
      .data[[nombre_banos]]
    ),
    !!nombre_parqueaderos := dplyr::if_else(
      is.na(parqueaderos_text),
      0,                         # no se menciona -> se asume 0 parqueaderos
      parqueaderos_text
    )
  )


#### ============================================================
### Punto 7 - Imputación adicional por grupos (estilo Referencia)
#### ============================================================

## 1) Se imputan los NA restantes de área agrupando por:
##    barrio, habitaciones, baños, estrato y tipo de propiedad.
## 2) Para observaciones sin barrio pero con UPZ se repite el proceso
##    reemplazando barrio por upz.
## 3) Para baños:
##    - se usa la misma estructura de agrupación, pero añadiendo el área
##    - primero con área continua y luego con área reclasificada en
##      intervalos de 10 m2 (0-10, 10-20, ...).

## 7.1) Nombres de columnas de contexto (ajusta si difieren en tu base)

nombre_barrio        <- "neighborhood"    # barrio
nombre_upz           <- "upz"             # upz
nombre_habitaciones  <- "rooms"           # número de habitaciones
nombre_estrato       <- "stratum"         # estrato
nombre_tipo_prop     <- "property_type"   # tipo de propiedad

area_var  <- nombre_area
banos_var <- nombre_banos

## 7.2) Área: imputación por grupos con barrio -----------------------------

vars_grupo_barrio <- c(
  nombre_barrio,
  nombre_habitaciones,
  banos_var,
  nombre_estrato,
  nombre_tipo_prop
)
vars_grupo_barrio <- intersect(vars_grupo_barrio, names(datos))

if (length(vars_grupo_barrio) > 0) {
  datos <- datos |>
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_grupo_barrio))) |>
    dplyr::mutate(
      !!area_var := dplyr::if_else(
        is.na(.data[[area_var]]),
        mean(.data[[area_var]], na.rm = TRUE),
        .data[[area_var]]
      )
    ) |>
    dplyr::ungroup()
}

## 7.3) Área: imputación por grupos con UPZ (para los que siguen en NA) ----

if (nombre_upz %in% names(datos)) {
  
  vars_grupo_upz <- c(
    nombre_upz,
    nombre_habitaciones,
    banos_var,
    nombre_estrato,
    nombre_tipo_prop
  )
  vars_grupo_upz <- intersect(vars_grupo_upz, names(datos))
  
  if (length(vars_grupo_upz) > 0) {
    datos <- datos |>
      dplyr::group_by(dplyr::across(dplyr::all_of(vars_grupo_upz))) |>
      dplyr::mutate(
        !!area_var := dplyr::if_else(
          is.na(.data[[area_var]]),
          mean(.data[[area_var]], na.rm = TRUE),
          .data[[area_var]]
        )
      ) |>
      dplyr::ungroup()
  }
}

## 7.4) Baños: se crea un área agrupada de 10 en 10 m2 ---------------------

if (area_var %in% names(datos)) {
  datos <- datos |>
    dplyr::mutate(
      area_10 = floor(.data[[area_var]] / 10) * 10
    )
}

## 7.5) Baños: imputación usando área continua -----------------------------

vars_banos_area <- c(
  nombre_barrio,
  nombre_habitaciones,
  nombre_estrato,
  nombre_tipo_prop,
  area_var
)
vars_banos_area <- intersect(vars_banos_area, names(datos))

if (length(vars_banos_area) > 0) {
  datos <- datos |>
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_banos_area))) |>
    dplyr::mutate(
      !!banos_var := dplyr::if_else(
        is.na(.data[[banos_var]]),
        mean(.data[[banos_var]], na.rm = TRUE),
        .data[[banos_var]]
      )
    ) |>
    dplyr::ungroup()
}

## 7.6) Baños: imputación usando área agrupada (intervalos de 10 m2) ------

vars_banos_area10 <- c(
  nombre_barrio,
  nombre_habitaciones,
  nombre_estrato,
  nombre_tipo_prop,
  "area_10"
)
vars_banos_area10 <- intersect(vars_banos_area10, names(datos))

if (length(vars_banos_area10) > 0) {
  datos <- datos |>
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_banos_area10))) |>
    dplyr::mutate(
      !!banos_var := dplyr::if_else(
        is.na(.data[[banos_var]]),
        mean(.data[[banos_var]], na.rm = TRUE),
        .data[[banos_var]]
      )
    ) |>
    dplyr::ungroup()
}

datos <- datos %>%
  select(
    -surface_total,
    -surface_covered,
    -rooms
  ) %>%
  mutate(
    bathrooms_round_up = floor(bathrooms)
  )

datos <- datos %>%
  select(
    -bathrooms,
    -area_text,
    -banos_text,
    -parqueaderos_text
  )

datos <- datos %>%
  mutate(
    banios = bathrooms_round_up,
    area = area_10,
    habitaciones = bedrooms
  )

datos <- datos %>%
  select(
    -bathrooms_round_up,
    -area_10,
    -bedrooms
  )

datos <- datos %>%
  mutate(
    banios = dplyr::if_else(
      is.na(.data[["banios"]]),
      1,
      .data[["banios"]]
    )
  )

#### ============================================================
### Punto 8 - Separar nuevamente train y test y guardar
#### ============================================================

## 1) Se separan de nuevo los conjuntos de train y test
## 2) Se guardan en formato .rds para usar en los scripts espaciales / de modelo

train_texto <- datos |>
  dplyr::filter(dataset == "train") |>
  dplyr::select(-dataset)

test_texto <- datos |>
  dplyr::filter(dataset == "test") |>
  dplyr::select(-dataset)

readr::write_rds(train_texto, "stores/train_texto.rds")
readr::write_rds(test_texto,  "stores/test_texto.rds")
