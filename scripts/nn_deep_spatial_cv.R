# ============================================
# Red neuronal profunda - ensable)
# ============================================

rm(list = ls())
gc()

set.seed(123)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(scales)
  library(caret)
  library(nnet)
  library(ggplot2)
})

# ============================================
# CARGAR DATOS
train <- read_csv("stores/train_final.csv", show_col_types = FALSE)
test <- read_csv("stores/test_final.csv", show_col_types = FALSE)


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

available_base <- features[features %in% names(train)]

# ============================================
# PREPARAR DATOS
prepare_data <- function(data, features, include_price = TRUE) {
  
  if (include_price) {
    data_clean <- data %>%
      filter(!is.na(price), price > 0) %>%
      mutate(log_price = log(price))
    
    data_clean <- data_clean[, c("property_id", "log_price", "LocNombre", features)]
  } else {
    data_clean <- data[, c("property_id", "LocNombre", features)]
  }
  
  # Imputar NAs
  for (feat in features) {
    if (all(data_clean[[feat]] %in% c(0, 1, NA), na.rm = TRUE)) {
      data_clean[[feat]][is.na(data_clean[[feat]])] <- 0
    } else {
      med_val <- median(data_clean[[feat]], na.rm = TRUE)
      if (is.na(med_val)) med_val <- 0
      data_clean[[feat]][is.na(data_clean[[feat]])] <- med_val
    }
  }
  
  return(data_clean)
}
train_clean <- prepare_data(train, available_base, include_price = TRUE)
test_clean <- prepare_data(test, available_base, include_price = FALSE)

#Annadir variables calculadas
add_all_features <- function(data) {
  data %>%
    mutate(
      # Ratios básicos
      area_per_room = area / pmax(habitaciones, 1),
      area_per_bathroom = area / pmax(banios, 1),
      rooms_per_bathroom = habitaciones / pmax(banios, 1),
      
      # Índices compuestos
      luxury_index = (piscina + gimnasio + sauna + jacuzzi + 
                        walking_closet + remodelado + chimenea) / 7,
      security_index = (vigilancia_24h + porteria + cctv + 
                          conjunto_residencial) / 4,
      recreation_index = (zona_verde + bbq + zona_infantil + 
                            salon_comunal + terraza_comunal + terraza) / 6,
      quality_proxy = (cocina_integral + piso_porcelanato + 
                         piso_madera + walking_closet) / 4,
      
      # Parking
      total_parking = n_parqueaderos + 
        coalesce(parqueadero_cubierto, 0) + 
        coalesce(parqueadero_comunal, 0),
      
      # Distancias
      avg_dist_services = (distnearestlibrary + distnearestschool + 
                             distnearestmuseum + distnearesttransmi) / 4,
      min_dist_service = pmin(distnearestlibrary, distnearestschool,
                              distnearestmuseum, distnearesttransmi),
      max_dist_service = pmax(distnearestlibrary, distnearestschool,
                              distnearestmuseum, distnearesttransmi),
      
      # Socioeconómico
      socioeconomic_avg = (EPE + EPT + EPCC) / 3,
      socioeconomic_weighted = EPE * 0.4 + EPT * 0.3 + EPCC * 0.3,
      
      # Espaciales
      lat_lon_interaction = lat * lon,
      lat_squared = lat^2,
      lon_squared = lon^2
    )
}

train_set <- add_all_features(train_clean)
test_set <- add_all_features(test_clean)

engineered_features <- c(
  'area_per_room', 'area_per_bathroom', 'rooms_per_bathroom',
  'luxury_index', 'security_index', 'recreation_index', 'quality_proxy',
  'total_parking', 'avg_dist_services', 'min_dist_service', 'max_dist_service',
  'socioeconomic_avg', 'socioeconomic_weighted',
  'lat_lon_interaction', 'lat_squared', 'lon_squared'
)

all_numeric_features <- c(available_base, engineered_features)

# ============================================

train_A <- bind_cols(
  train_set %>% dplyr::select(property_id, log_price),
  train_set %>% dplyr::select(all_of(all_numeric_features)))
test_A <- bind_cols(
  test_clean %>% dplyr::select(property_id),
  test_clean %>% dplyr::select(all_of(all_numeric_features)))

# Versión A
preprocess_A <- preProcess(
  train_A %>% dplyr::select(all_of(all_numeric_features)),
  method = c("center", "scale")
)

train_A_scaled_features <- predict(preprocess_A,
                                   train_A %>% dplyr::select(all_of(all_numeric_features)))
                                 val_A %>% dplyr::select(all_of(all_numeric_features))
test_A_scaled_features <- predict(preprocess_A,
                                  test_A %>% dplyr::select(all_of(all_numeric_features)))

train_A_scaled <- bind_cols(
  train_A %>% dplyr::select(property_id, log_price),
  train_A_scaled_features
)

val_A_scaled <- bind_cols(
  val_A %>% dplyr::select(property_id, log_price),
  val_A_scaled_features
)

test_A_scaled <- bind_cols(
  test_A %>% dplyr::select(property_id),
  test_A_scaled_features
)

# ============================================
# ENTRENAR MODELOS 
# ============================================

train_data_A <- train_A_scaled %>% dplyr::select(-property_id)
val_data_A <- val_A_scaled %>% dplyr::select(-property_id)

formula_nn <- log_price ~ .

# Modelo A1: Simple (25 neuronas)
nn_A1 <- nnet(
  formula_nn, data = train_data_A,
  size = 25, decay = 0.010, linout = TRUE,
  maxit = 500, MaxNWts = 2000, trace = FALSE
)

# Modelo A2: Mediana (30 neuronas, 1 led)
nn_A2 <- nnet(
  formula_nn, data = train_data_A,
  size = 30, decay = 0.012, linout = TRUE,
  maxit = 500, MaxNWts = 2500, trace = FALSE
)

# Modelo A3: Amplia (40 neuronas, 1 ledger)
nn_A3 <- nnet(
  formula_nn, data = train_data_A,
  size = 40, decay = 0.015, linout = TRUE,
  maxit = 400, MaxNWts = 3000, trace = FALSE
)

# Modelo A4: Profunda (30→20)

nn_A4 <- nnet(
  formula_nn, data = train_data_A,
  size = 35, decay = 0.015, linout = TRUE,
  maxit = 500, MaxNWts = 2800, trace = FALSE
)

# ============================================
# EVALUACIÓN
evaluate <- function(model, val_data, version_name, model_name) {
  pred_log <- predict(model, newdata = val_data)
  pred_val <- exp(pred_log)
  actual_val <- exp(val_data$log_price)
  
  mae <- mean(abs(pred_val - actual_val))
  rmse <- sqrt(mean((pred_val - actual_val)^2))
  mape <- mean(abs((actual_val - pred_val) / actual_val)) * 100
  r2 <- cor(pred_val, actual_val)^2
  
  list(
    version = version_name,
    name = model_name,
    mae = mae,
    rmse = rmse,
    mape = mape,
    r2 = r2,
    predictions = pred_val,
    model = model
  )
}

actual_val <- exp(val_A_scaled$log_price)

# Evaluar todos
results_A1 <- evaluate(nn_A1, val_data_A, "A", "Simple (25)")
results_A2 <- evaluate(nn_A2, val_data_A, "A", "Mediana (30)")
results_A3 <- evaluate(nn_A3, val_data_A, "A", "Amplia (40)")
results_A4 <- evaluate(nn_A4, val_data_A, "A", "Profunda (35)")

all_results <- list(results_A1, results_A2, results_A3, results_A4)

# Seleccionar mejores
all_results_sorted <- all_results[order(sapply(all_results, function(x) x$mae))]
best_result <- all_results_sorted[[1]]
second_best <- all_results_sorted[[2]]
third_best <- all_results_sorted[[3]]

# ============================================
# ENSEMBLE
# Para ensemble, necesitamos predicciones en la misma escala

best_version <- best_result$version
top_same_version <- Filter(function(x) x$version == best_version, all_results_sorted)[1:3]

if (length(top_same_version) >= 3) {
  ensemble_preds <- (top_same_version[[1]]$predictions +
                       top_same_version[[2]]$predictions +
                       top_same_version[[3]]$predictions) / 3
} else {
  # Usar solo los de la misma versión disponibles
  n_available <- length(top_same_version)
  ensemble_preds <- Reduce(`+`, lapply(top_same_version, function(x) x$predictions)) / n_available
}

# Evaluar ensemble
mae_ensemble <- mean(abs(ensemble_preds - actual_val))
r2_ensemble <- cor(ensemble_preds, actual_val)^2
mape_ensemble <- mean(abs((actual_val - ensemble_preds) / actual_val)) * 100

# Comparar con mejor individual
mejora_ensemble <- ((best_result$mae - mae_ensemble) / best_result$mae) * 100

# ============================================
# REENTRENAR CON TODOS LOS DATOS

final_model <- nnet(
    log_price ~ .,
    data = full_train_data,
    size = best_result$model$n[2],
    decay = best_result$model$decay,
    linout = TRUE,
    maxit = 600,
    MaxNWts = 3500,
    trace = FALSE
  )
  
test_for_pred <- test_A_scaled %>% dplyr::select(-property_id)

# ============================================
# PREDICCIONES FINALES

if (use_ensemble) {
  # Para ensemble, necesitamos los 3 modelos finales
  
  # (Simplificado: usar solo el mejor)
  pred_log_test <- predict(final_model, newdata = test_for_pred)
  
} else {
  pred_log_test <- predict(final_model, newdata = test_for_pred)
}

pred_test <- exp(pred_log_test)
pred_test_rounded <- round(pred_test / 100000) * 100000

# ============================================
# GUARDAR SUBMISSION

submission <- data.frame(
  property_id = if(best_result$version == "A") test_A$property_id else test_B$property_id,
  price = pred_test_rounded
)

write_csv(submission,"stores/models/nn_1hd_prom.csv")

