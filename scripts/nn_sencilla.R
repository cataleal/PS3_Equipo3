# ============================================
# Neuronal networ 1x20x1
# ============================================

rm(list = ls())
set.seed(123)
library(tidyverse)
library(nnet)
library(caret)
library(scales)

# ============================================
# CARGAR Y LIMPIAR DATOS
train <- read_csv("stores/train_final.csv", show_col_types = FALSE)
test <- read_csv("stores/test_final.csv", show_col_types = FALSE)

# ============================================
# SELECCIÓN DE FEATURES NUMÉRICAS

features <- c(
  # Básicas (4)
  'area', 'habitaciones', 'banios', 'n_parqueaderos',
  
  # Socioeconómicas (4) - IMPORTANTES
  'EPE', 'EPT', 'EPCC', 'recaudo_predial',
  
  # Distancias (4)
  'distnearestlibrary', 'distnearestschool', 
  'distnearestmuseum', 'distnearesttransmi',
  
  # Amenidades interiores (12)
  'cocina_americana', 'cocina_integral', 'balcon', 'terraza',
  'chimenea', 'walking_closet', 'duplex', 'deposito',
  'altillo', 'piso_madera', 'piso_porcelanato', 'remodelado',
  
  # Amenidades edificio (10)
  'gimnasio', 'piscina', 'sauna', 'jacuzzi',
  'ascensor', 'zona_verde', 'bbq', 'conjunto_residencial',
  'vigilancia_24h', 'porteria',
  
  # Amenidades adicionales (6)
  'cctv', 'parqueadero_cubierto', 'parqueadero_comunal',
  'zona_infantil', 'salon_comunal', 'terraza_comunal',
  
  # Otras (2)
  'pet_friendly', 'zona_humeda',
  
  # Ubicación (2) - NUMÉRICAS
  'lat', 'lon',
  
  # Temporal (2)
  'month', 'year'
)

# Verificar features disponibles
available_features <- features[features %in% names(train)]
# ============================================
# PREPARAR DATOS

# Eliminar columna ...1 si existe
train <- train %>% dplyr::select(-matches("^\\.\\.\\.[0-9]+"))
test <- test %>% dplyr::select(-matches("^\\.\\.\\.[0-9]+"))

# Crear datasets limpios
train_clean <- train %>%
  filter(!is.na(price), price > 0) %>%
  mutate(log_price = log(price)) %>%
  dplyr::select(property_id, log_price, all_of(available_features))

test_clean <- test %>%
  dplyr::select(property_id, all_of(available_features))

# Imputar NAs con mediana/0
for (feat in available_features) {
  # Train
  if (all(train_clean[[feat]] %in% c(0, 1, NA), na.rm = TRUE)) {
    # Binaria - usar 0
    train_clean[[feat]][is.na(train_clean[[feat]])] <- 0
    test_clean[[feat]][is.na(test_clean[[feat]])] <- 0
  } else {
    # Numérica - usar mediana
    med_val <- median(train_clean[[feat]], na.rm = TRUE)
    if (is.na(med_val)) med_val <- 0
    train_clean[[feat]][is.na(train_clean[[feat]])] <- med_val
    test_clean[[feat]][is.na(test_clean[[feat]])] <- med_val
  }
}

# ============================================
# Variable calculadas

add_features <- function(data) {
  data %>%
    mutate(
      area_per_room = area / pmax(habitaciones, 1),
      luxury_index = (piscina + gimnasio + sauna + jacuzzi) / 4,
      security_index = (vigilancia_24h + porteria + cctv) / 3,
      socioeconomic_avg = (EPE + EPT + EPCC) / 3
    )
}

train_clean <- add_features(train_clean)
test_clean <- add_features(test_clean)

new_features <- c('area_per_room', 'luxury_index', 'security_index', 'socioeconomic_avg')
all_features <- c(available_features, new_features)

# ============================================
# NORMALIZACIÓN

preprocess_params <- preProcess(
  train_clean %>% dplyr::select(all_of(all_features)),
  method = c("center", "scale")
)

train_scaled_features <- predict(preprocess_params, 
                                 train_clean %>% dplyr::select(all_of(all_features)))
test_scaled_features <- predict(preprocess_params, 
                                test_clean %>% dplyr::select(all_of(all_features)))

val_data <- bind_cols(
  train_clean %>% dplyr::select(property_id, log_price),
  train_scaled_features
)

val_data <- bind_cols(
  test_clean %>% dplyr::select(property_id),
  test_scaled_features
)
# ============================================
# ENTRENAR

formula_nn <- log_price ~ .

nn_model <- nnet(
  formula_nn,
  data = train_data,
  size = 20,
  decay = 0.01,
  linout = TRUE,
  maxit = 500,
  MaxNWts = 2000,
  trace = FALSE
)

# ============================================
# EVALUACIÓN

pred_log_val <- predict(nn_model, newdata = val_data)
pred_val <- exp(pred_log_val)
actual_val <- exp(val_set$log_price)

mae_val <- mean(abs(pred_val - actual_val))
rmse_val <- sqrt(mean((pred_val - actual_val)^2))
mape_val <- mean(abs((actual_val - pred_val) / actual_val)) * 100
r2_val <- cor(pred_val, actual_val)^2

# ============================================
# PREDICCIONES TEST

# Reentrenar con todos los datos
full_train <- train_scaled %>% dplyr::select(-property_id)

nn_full <- nnet(
  formula_nn,
  data = full_train,
  size = 20,
  decay = 0.01,
  linout = TRUE,
  maxit = 500,
  MaxNWts = 2000,
  trace = FALSE
)

# Predicciones
test_data <- test_scaled %>% dplyr::select(-property_id)
pred_log_test <- predict(nn_full, newdata = test_data)
pred_test <- exp(pred_log_test)

# Redondear
pred_test_rounded <- round(pred_test / 100000) * 100000
# ============================================
# GUARDAR

submission <- tibble(
  property_id = test_clean$property_id,
  price = as.vector(pred_test_rounded)
)

write_csv(submission,"stores/models/nn_1hd_20n.csv")