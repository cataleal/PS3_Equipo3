# ============================================
# LINEAR REGRESSION EN R
# Problem Set 3 - Equipo 03
# ===========================================

rm(list = ls())
gc()

set.seed(123)
# ============================================
# 1. CARGAR LIBRERÍAS

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
train <- read_csv("stores/train_final.csv")
test <- read_csv("stores/test_final.csv")

# ============================================
# 3. descriptivas

# Estadísticas de precio
cat("DISTRIBUCIÓN DE PRECIO (Variable Objetivo):\n")
price_stats <- train %>%
  summarise(
    Media = mean(price, na.rm = TRUE),
    Mediana = median(price, na.rm = TRUE),
    Desv_Est = sd(price, na.rm = TRUE),
    Min = min(price, na.rm = TRUE),
    Max = max(price, na.rm = TRUE),
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE)
  )

print(price_stats)

# Verificar na's
missing_train <- colSums(is.na(train))
if (any(missing_train > 0)) {
  cat("⚠ Valores faltantes encontrados:\n")
  print(missing_train[missing_train > 0])
  cat("\n")
}

# ============================================
# 4. DEFINIR FEATURES

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

# ============================================
# 5. PREPARAR DATOS

# Imputar NAs con 0
train_clean <- train %>%
  mutate(across(all_of(available_features), ~replace_na(.x, 0)))

test_clean <- test %>%
  mutate(across(all_of(available_features), ~replace_na(.x, 0)))

# TRANSFORMACIÓN LOGARÍTMICA DE PRECIO
cat("✓ Aplicando transformación log a precio\n")
train_clean <- train_clean %>%
  mutate(log_price = log(price))

# Verificar que no haya -Inf o NA
if (any(is.infinite(train_clean$log_price)) || any(is.na(train_clean$log_price))) {
  cat("⚠ Ajustando precios <= 0 antes de aplicar log\n")
  train_clean <- train_clean %>%
    mutate(log_price = log(pmax(price, 1)))
}

cat(sprintf("✓ Rango de log(price): [%.2f, %.2f]\n", 
            min(train_clean$log_price), max(train_clean$log_price)))

# Crear matriz de features
X_train <- train_clean %>%
  dplyr::select(all_of(available_features)) %>%
  as.data.frame()

y_train <- train_clean$log_price  # ← Variable objetivo transformada

X_test <- test_clean %>%
  dplyr::select(all_of(available_features)) %>%
  as.data.frame()

# ============================================
# 6. CORRELACIÓN CON VARIABLE OBJETIVO

# Calcular correlaciones
correlations <- X_train %>%
  bind_cols(log_price = y_train) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  dplyr::select(log_price) %>%
  arrange(desc(abs(log_price)))

# Guardar gráfico de correlación
if (ncol(X_train) <= 50) {  # Solo si hay pocas variables
  png("correlation_matrix.png", width = 1200, height = 1000, res = 150)
  correlation_matrix <- cor(X_train, use = "pairwise.complete.obs")
  corrplot(correlation_matrix, method = "color", type = "upper", 
           tl.cex = 0.6, tl.col = "black")
  dev.off()
}

# ============================================
# 7. SPLIT TRAIN/VALIDATION
# ============================================

cat("7. SPLIT INTERNO (TRAIN/VALIDATION)\n")
cat("--------------------------------------------------------------------\n")

# Split 80/20
train_index <- createDataPartition(y_train, p = 0.8, list = FALSE)

X_train_split <- X_train[train_index, ]
y_train_split <- y_train[train_index]

X_val_split <- X_train[-train_index, ]
y_val_split <- y_train[-train_index]

# ============================================
# 8.REGRESIÓN LINEAL SIMPLE
# Crear fórmula
formula_lm <- as.formula(paste("log_price ~", paste(available_features, collapse = " + ")))

# Data frame para modelo
train_data <- X_train_split %>%
  bind_cols(log_price = y_train_split)

# Entrenar modelo
model_lm <- lm(formula_lm, data = train_data)

# Top coeficientes
coef_df <- tidy(model_lm) %>%
  arrange(desc(abs(estimate))) %>%
  filter(term != "(Intercept)")
print(head(coef_df %>% dplyr::select(term, estimate, p.value), 10))

# ============================================
# 9. PREDICCIONES Y EVALUACIÓN

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
# Reentrenar con todo el conjunto de train

final_model_full <- lm(formula_final, data = full_train_data)

cat("✓ Modelo final entrenado con todos los datos\n")
cat(sprintf("  R²: %.4f\n\n", summary(final_model_full)$r.squared))

# ============================================
# 13. PREDICCIONES EN TEST
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
write_csv(submission, "lineal_regression_model.csv")
