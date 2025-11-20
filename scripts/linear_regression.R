# ============================================
# LINEAR REGRESSION EN R
# Problem Set 3 - Equipo 03
# ============================================

# Limpiar environment
rm(list = ls())
gc()

# Establecer seed para reproducibilidad
set.seed(123)

# ============================================
# 1. CARGAR LIBRERÍAS
# ============================================
# Verificar e instalar librerías si es necesario
packages <- c("tidyverse", "caret", "car", "MASS", "glmnet", 
              "corrplot", "scales", "knitr", "broom")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Instalando %s...\n", pkg))
    install.packages(pkg, dependencies = TRUE, repos = "https://cran.rstudio.com/")
    library(pkg, character.only = TRUE)
  }
}

# Cargar librerías
library(tidyverse)    # Manipulación de datos
library(caret)        # Machine learning
library(car)          # Diagnósticos de regresión
library(MASS)         # Stepwise selection
library(glmnet)       # Ridge/Lasso
library(corrplot)     # Matriz de correlación
library(scales)       # Formateo de números
library(knitr)        # Tablas
library(broom)        # Tidying model outputs

# ============================================
# 2. CARGAR DATOS
# ============================================

# Cargar datos
train <- read_csv("stores/train_final.csv")
test <- read_csv("stores/test_final.csv")

# ============================================
# 3. DEFINIR FEATURES
# ============================================

# Features estructuradas (las más importantes para regresión lineal)
structural_vars <- c('habitaciones', 'area', 'banios', 'month', 'year')

# Features de texto
text_vars <- c(
  'cocina_americana', 'cocina_integral', 'gimnasio', 'balcon',
  'chimenea', 'terraza', 'ascensor', 'sauna', 'jacuzzi', 'piscina',
  'deposito', 'walking_closet', 'duplex', 'zona_verde', 'bbq',
  'conjunto_residencial', 'altillo', 'vigilancia_24h', 'porteria',
  'cctv', 'parqueadero_cubierto', 'parqueadero_comunal', 
  'zona_infantil', 'salon_comunal', 'zona_humeda', 'terraza_comunal',
  'pet_friendly', 'remodelado', 'piso_madera', 'piso_porcelanato',
  'n_parqueaderos'
)

# Features espaciales
spatial_vars <- c(
  'distnearestlibrary', 'distnearestschool', 'distnearestmuseum',
  'distnearesttransmi', 'distnearestsitp', 'recaudo_predial',
  'ESTRATO', 'num_restaurantes_manz', 'distrestaurantebar',
  'luminarias', 'EPE', 'EPT', 'EPCC', 'EPE_UPZ'
)

all_features <- c(structural_vars, text_vars, spatial_vars)

# Filtrar features disponibles
available_features <- all_features[all_features %in% names(train)]

if (length(available_features) < length(all_features)) {
  missing_vars <- setdiff(all_features, available_features)
  cat(sprintf("\n⚠ Features no encontradas: %d\n", length(missing_vars)))
  cat("Primeras 5:", paste(head(missing_vars, 5), collapse = ", "), "\n")

# ============================================
# 4. PREPARAR DATOS
# ============================================

# Imputar NAs con 0 (común para features de texto que son binarias)
train_clean <- train %>%
  mutate(across(all_of(available_features), ~replace_na(.x, 0)))

test_clean <- test %>%
  mutate(across(all_of(available_features), ~replace_na(.x, 0)))

# TRANSFORMACIÓN LOGARÍTMICA DE PRECIO
train_clean <- train_clean %>%
  mutate(log_price = log(price))

# Verificar que no haya -Inf o NA
if (any(is.infinite(train_clean$log_price)) || any(is.na(train_clean$log_price))) {
  cat("⚠ Ajustando precios <= 0 antes de aplicar log\n")
  train_clean <- train_clean %>%
    mutate(log_price = log(pmax(price, 1)))
}

# Crear matriz de features
X_train <- train_clean %>%
  dplyr::select(all_of(available_features)) %>%
  as.data.frame()

y_train <- train_clean$log_price  # ← Variable objetivo transformada

X_test <- test_clean %>%
  dplyr::select(all_of(available_features)) %>%
  as.data.frame()

# Calcular correlaciones
correlations <- X_train %>%
  bind_cols(log_price = y_train) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  dplyr::select(log_price) %>%
  arrange(desc(abs(log_price)))

cat("Top 10 features más correlacionadas con log(price):\n")
print(head(correlations, 10))
cat("\n")

# Guardar gráfico de correlación
if (ncol(X_train) <= 50) {  # Solo si hay pocas variables
  png("correlation_matrix.png", width = 1200, height = 1000, res = 150)
  correlation_matrix <- cor(X_train, use = "pairwise.complete.obs")
  corrplot(correlation_matrix, method = "color", type = "upper", 
           tl.cex = 0.6, tl.col = "black")
  dev.off()
  cat("✓ Matriz de correlación guardada: correlation_matrix.png\n\n")
}

# ============================================
# 6. SPLIT TRAIN/VALIDATION
# ============================================
# Split 80/20
train_index <- createDataPartition(y_train, p = 0.8, list = FALSE)

X_train_split <- X_train[train_index, ]
y_train_split <- y_train[train_index]

X_val_split <- X_train[-train_index, ]
y_val_split <- y_train[-train_index]

# ============================================
# 7. REGRESIÓN LINEAL SIMPLE
# ============================================

# Crear fórmula
formula_lm <- as.formula(paste("log_price ~", paste(available_features, collapse = " + ")))

# Data frame para modelo
train_data <- X_train_split %>%
  bind_cols(log_price = y_train_split)

# Entrenar modelo
model_lm <- lm(formula_lm, data = train_data)

# ============================================
# 8. PREDICCIONES
# ============================================

cat("9. PREDICCIONES Y EVALUACIÓN\n")
cat("--------------------------------------------------------------------\n")

# Predicciones en validación (en escala log)
val_data <- X_val_split %>%
  bind_cols(log_price = y_val_split)

pred_log_val <- predict(model_lm, newdata = val_data)

# Transformar de vuelta a escala original
pred_val <- exp(pred_log_val)
actual_val <- exp(y_val_split)

# Calcular métricas
mae_val <- mean(abs(pred_val - actual_val))
rmse_val <- sqrt(mean((pred_val - actual_val)^2))
mape_val <- mean(abs((actual_val - pred_val) / actual_val)) * 100
r2_val <- cor(pred_val, actual_val)^2

# ============================================
# 12. REENTRENAR CON TODOS LOS DATOS
# ============================================
# Reentrenar con todo el conjunto de train
full_train_data <- X_train %>%
  bind_cols(log_price = y_train)

final_model_full <- lm(formula_lm, data = full_train_data)

cat("✓ Modelo final entrenado con todos los datos\n")
cat(sprintf("  R²: %.4f\n\n", summary(final_model_full)$r.squared))

# ============================================
# 13. PREDICCIONES KAGLLE
# ============================================
# Predicciones en escala log
pred_log_test <- predict(final_model_full, newdata = X_test)

# Transformar a escala original
pred_test <- exp(pred_log_test)

# REDONDEAR A 100,000 (como mencionaste)
cat("✓ Aplicando redondeo a 100,000\n")
pred_test_rounded <- round(pred_test / 100000) * 100000

# Crear submission
submission <- tibble(
  property_id = test_clean$property_id,
  price = pred_test_rounded
)

# Guardar submission
write_csv(submission, "stores/models/lineal_reg.csv" )