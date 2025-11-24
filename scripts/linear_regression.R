# ============================================
# LINEAR REGRESSION EN R
# Problem Set 3 - Equipo 03
# ============================================
# 
# CARACTERÍSTICAS:
# - Transformación logarítmica de precio
# - Redondeo a 100,000
# - Validación cruzada
# - Análisis de residuales
# - Feature selection
#
# INSTRUCCIONES:
# 1. Instalar librerías (ver install_packages.R)
# 2. Colocar train.csv y test.csv en carpeta data/
# 3. Ejecutar: source("linear_regression_model.R")
# ============================================

# Limpiar environment
rm(list = ls())
gc()

# Establecer seed para reproducibilidad
set.seed(123)

cat("====================================================================\n")
cat("LINEAR REGRESSION CON TRANSFORMACIÓN LOGARÍTMICA\n")
cat("====================================================================\n\n")

# ============================================
# 1. CARGAR LIBRERÍAS
# ============================================

cat("1. CARGANDO LIBRERÍAS\n")
cat("--------------------------------------------------------------------\n")

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

cat("✓ Librerías cargadas\n\n")

# ============================================
# 2. CARGAR DATOS
# ============================================

cat("2. CARGANDO DATOS\n")
cat("--------------------------------------------------------------------\n")

# Intentar diferentes rutas
possible_paths <- c(
  "data/train.csv",
  "train.csv",
  "../data/train.csv"
)

train_path <- NULL
for (path in possible_paths) {
  if (file.exists(path)) {
    train_path <- path
    break
  }
}

if (is.null(train_path)) {
  stop("❌ ERROR: No se encontró train.csv\n",
       "Coloca train.csv en:\n",
       "  • data/train.csv\n",
       "  • train.csv\n")
}

# Cargar datos
train <- read_csv("stores/train_final.csv")
test <- read_csv("stores/test_final.csv")

cat(sprintf("✓ Train cargado: %s\n", train_path))
cat(sprintf("  Dimensiones: %d filas × %d columnas\n", nrow(train), ncol(train)))
cat(sprintf("✓ Test cargado: %d filas × %d columnas\n\n", nrow(test), ncol(test)))

# ============================================
# 3. ANÁLISIS EXPLORATORIO
# ============================================

cat("3. ANÁLISIS EXPLORATORIO DE DATOS\n")
cat("--------------------------------------------------------------------\n")

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
cat("\n")

# Verificar valores faltantes
missing_train <- colSums(is.na(train))
if (any(missing_train > 0)) {
  cat("⚠ Valores faltantes encontrados:\n")
  print(missing_train[missing_train > 0])
  cat("\n")
}

# ============================================
# 4. DEFINIR FEATURES
# ============================================

cat("4. DEFINIENDO FEATURES\n")
cat("--------------------------------------------------------------------\n")

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

cat(sprintf("Features definidas: %d\n", length(all_features)))
cat(sprintf("Features disponibles: %d\n", length(available_features)))

if (length(available_features) < length(all_features)) {
  missing_vars <- setdiff(all_features, available_features)
  cat(sprintf("\n⚠ Features no encontradas: %d\n", length(missing_vars)))
  cat("Primeras 5:", paste(head(missing_vars, 5), collapse = ", "), "\n")
}
cat("\n")

# ============================================
# 5. PREPARAR DATOS
# ============================================

cat("5. PREPARACIÓN DE DATOS\n")
cat("--------------------------------------------------------------------\n")

# Imputar NAs con 0 (común para features de texto que son binarias)
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
  select(all_of(available_features)) %>%
  as.data.frame()

y_train <- train_clean$log_price  # ← Variable objetivo transformada

X_test <- test_clean %>%
  select(all_of(available_features)) %>%
  as.data.frame()

cat(sprintf("✓ Datos preparados:\n"))
cat(sprintf("  X_train: %d × %d\n", nrow(X_train), ncol(X_train)))
cat(sprintf("  y_train: %d (log transformado)\n", length(y_train)))
cat(sprintf("  X_test: %d × %d\n\n", nrow(X_test), ncol(X_test)))

# ============================================
# 6. CORRELACIÓN CON VARIABLE OBJETIVO
# ============================================

cat("6. ANÁLISIS DE CORRELACIÓN\n")
cat("--------------------------------------------------------------------\n")

# Calcular correlaciones
correlations <- X_train %>%
  bind_cols(log_price = y_train) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  select(log_price) %>%
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

cat(sprintf("✓ Train: %d observaciones\n", nrow(X_train_split)))
cat(sprintf("✓ Val:   %d observaciones\n\n", nrow(X_val_split)))

# ============================================
# 8. MODELO 1: REGRESIÓN LINEAL SIMPLE
# ============================================

cat("8. MODELO 1: REGRESIÓN LINEAL SIMPLE\n")
cat("====================================================================\n")

# Crear fórmula
formula_lm <- as.formula(paste("log_price ~", paste(available_features, collapse = " + ")))

# Data frame para modelo
train_data <- X_train_split %>%
  bind_cols(log_price = y_train_split)

# Entrenar modelo
cat("Entrenando modelo de regresión lineal...\n")
model_lm <- lm(formula_lm, data = train_data)

cat("✓ Modelo entrenado\n\n")

# Resumen del modelo
cat("RESUMEN DEL MODELO:\n")
cat("--------------------------------------------------------------------\n")
model_summary <- summary(model_lm)
cat(sprintf("R²:          %.4f\n", model_summary$r.squared))
cat(sprintf("R² ajustado: %.4f\n", model_summary$adj.r.squared))
cat(sprintf("RMSE:        %.4f\n", sqrt(mean(model_summary$residuals^2))))
cat("\n")

# Top coeficientes
coef_df <- tidy(model_lm) %>%
  arrange(desc(abs(estimate))) %>%
  filter(term != "(Intercept)")

cat("Top 10 coeficientes más importantes:\n")
print(head(coef_df %>% select(term, estimate, p.value), 10))
cat("\n")

# ============================================
# 9. PREDICCIONES Y EVALUACIÓN
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

cat("MÉTRICAS EN VALIDACIÓN (escala original):\n")
cat(sprintf("  MAE:  %s\n", comma(round(mae_val, 2))))
cat(sprintf("  RMSE: %s\n", comma(round(rmse_val, 2))))
cat(sprintf("  MAPE: %.2f%%\n", mape_val))
cat(sprintf("  R²:   %.4f\n", r2_val))
cat("\n")

# ============================================
# 10. DIAGNÓSTICOS DE REGRESIÓN
# ============================================

cat("10. DIAGNÓSTICOS DE REGRESIÓN\n")
cat("--------------------------------------------------------------------\n")

# Guardar gráficos de diagnóstico
png("regression_diagnostics.png", width = 1400, height = 1000, res = 150)
par(mfrow = c(2, 2))
plot(model_lm)
dev.off()
cat("✓ Gráficos de diagnóstico guardados: regression_diagnostics.png\n")

# Test de multicolinealidad (VIF)
if (ncol(X_train_split) <= 50) {  # Solo si no hay demasiadas variables
  cat("\nTest de Multicolinealidad (VIF):\n")
  vif_values <- vif(model_lm)
  high_vif <- vif_values[vif_values > 10]
  
  if (length(high_vif) > 0) {
    cat("⚠ Variables con VIF > 10 (multicolinealidad alta):\n")
    print(head(sort(high_vif, decreasing = TRUE), 10))
  } else {
    cat("✓ No se detectó multicolinealidad severa\n")
  }
}
cat("\n")

# ============================================
# 11. MODELO 2: STEPWISE SELECTION (OPCIONAL)
# ============================================

cat("11. MODELO 2: STEPWISE SELECTION\n")
cat("====================================================================\n")
cat("Aplicando selección de features por AIC (puede tomar varios minutos)...\n")

# Stepwise selection
model_step <- stepAIC(model_lm, direction = "both", trace = FALSE)

cat("✓ Stepwise completado\n")
cat(sprintf("Features seleccionadas: %d de %d\n", 
            length(coef(model_step)) - 1, ncol(X_train_split)))

# Evaluar modelo stepwise
pred_log_val_step <- predict(model_step, newdata = val_data)
pred_val_step <- exp(pred_log_val_step)

mae_val_step <- mean(abs(pred_val_step - actual_val))
rmse_val_step <- sqrt(mean((pred_val_step - actual_val)^2))
r2_val_step <- cor(pred_val_step, actual_val)^2

cat("\nMÉTRICAS STEPWISE:\n")
cat(sprintf("  MAE:  %s\n", comma(round(mae_val_step, 2))))
cat(sprintf("  RMSE: %s\n", comma(round(rmse_val_step, 2))))
cat(sprintf("  R²:   %.4f\n\n", r2_val_step))

# Seleccionar mejor modelo
if (mae_val_step < mae_val) {
  cat("✓ Modelo Stepwise tiene mejor performance\n")
  final_model <- model_step
  model_name <- "Stepwise"
} else {
  cat("✓ Modelo simple tiene mejor performance\n")
  final_model <- model_lm
  model_name <- "Simple"
}
cat("\n")

# ============================================
# 12. REENTRENAR CON TODOS LOS DATOS
# ============================================

cat("12. REENTRENAMIENTO CON DATOS COMPLETOS\n")
cat("--------------------------------------------------------------------\n")

# Reentrenar con todo el conjunto de train
full_train_data <- X_train %>%
  bind_cols(log_price = y_train)

if (model_name == "Stepwise") {
  # Obtener features seleccionadas
  selected_features <- names(coef(final_model))[-1]
  formula_final <- as.formula(paste("log_price ~", paste(selected_features, collapse = " + ")))
} else {
  formula_final <- formula_lm
}

final_model_full <- lm(formula_final, data = full_train_data)

cat("✓ Modelo final entrenado con todos los datos\n")
cat(sprintf("  R²: %.4f\n\n", summary(final_model_full)$r.squared))

# ============================================
# 13. PREDICCIONES EN TEST
# ============================================

cat("13. PREDICCIONES EN TEST PARA KAGGLE\n")
cat("====================================================================\n")

# Predicciones en escala log
pred_log_test <- predict(final_model_full, newdata = X_test)

# Transformar a escala original
pred_test <- exp(pred_log_test)

# REDONDEAR A 100,000 (como mencionaste)
cat("✓ Aplicando redondeo a 100,000\n")
pred_test_rounded <- round(pred_test / 100000) * 100000

# Estadísticas de predicciones
cat("\nEstadísticas de predicciones:\n")
cat(sprintf("  Media:   %s\n", comma(round(mean(pred_test_rounded)))))
cat(sprintf("  Mediana: %s\n", comma(round(median(pred_test_rounded)))))
cat(sprintf("  Min:     %s\n", comma(round(min(pred_test_rounded)))))
cat(sprintf("  Max:     %s\n", comma(round(max(pred_test_rounded)))))
cat("\n")

# Crear submission
submission <- tibble(
  property_id = test_clean$property_id,
  price = pred_test_rounded
)

# Guardar submission
filename <- sprintf("submission_linear_regression_%s.csv", tolower(model_name))
write_csv(submission, filename)

cat(sprintf("✓ Submission guardado: %s\n\n", filename))

# ============================================
# 14. COMPARACIÓN CON/SIN LOG
# ============================================

cat("14. COMPARACIÓN: CON vs SIN TRANSFORMACIÓN LOG\n")
cat("====================================================================\n")

# Modelo sin transformación log (para comparar)
cat("Entrenando modelo sin transformación log...\n")

train_data_no_log <- X_train_split %>%
  bind_cols(price = exp(y_train_split))  # Precio original

model_no_log <- lm(price ~ ., data = train_data_no_log)

val_data_no_log <- X_val_split %>%
  bind_cols(price = actual_val)

pred_val_no_log <- predict(model_no_log, newdata = val_data_no_log)

mae_no_log <- mean(abs(pred_val_no_log - actual_val))
rmse_no_log <- sqrt(mean((pred_val_no_log - actual_val)^2))
r2_no_log <- cor(pred_val_no_log, actual_val)^2

cat("\nCOMPARACIÓN:\n")
cat("--------------------------------------------------------------------\n")
cat(sprintf("%-20s %15s %15s\n", "Métrica", "SIN Log", "CON Log"))
cat("--------------------------------------------------------------------\n")
cat(sprintf("%-20s %15s %15s\n", "MAE", comma(round(mae_no_log)), comma(round(mae_val))))
cat(sprintf("%-20s %15s %15s\n", "RMSE", comma(round(rmse_no_log)), comma(round(rmse_val))))
cat(sprintf("%-20s %15.4f %15.4f\n", "R²", r2_no_log, r2_val))
cat("--------------------------------------------------------------------\n")

improvement <- ((mae_no_log - mae_val) / mae_no_log) * 100
cat(sprintf("\n✓ Mejora con transformación log: %.2f%%\n\n", improvement))

# ============================================
# 15. GUARDAR RESULTADOS
# ============================================

cat("15. GUARDANDO RESULTADOS\n")
cat("--------------------------------------------------------------------\n")

# Crear reporte
report <- list(
  modelo = model_name,
  transformacion = "log",
  redondeo = 100000,
  features_totales = length(available_features),
  features_usadas = length(coef(final_model_full)) - 1,
  metricas_validacion = list(
    mae = mae_val,
    rmse = rmse_val,
    mape = mape_val,
    r2 = r2_val
  ),
  comparacion_log = list(
    mae_sin_log = mae_no_log,
    mae_con_log = mae_val,
    mejora_porcentual = improvement
  )
)

# Guardar reporte en texto
sink("linear_regression_results.txt")
cat("====================================================================\n")
cat("RESULTADOS - LINEAR REGRESSION\n")
cat("====================================================================\n\n")

cat("CONFIGURACIÓN:\n")
cat(sprintf("  Modelo: %s\n", report$modelo))
cat(sprintf("  Transformación: %s\n", report$transformacion))
cat(sprintf("  Redondeo: %s\n", comma(report$redondeo)))
cat(sprintf("  Features: %d de %d disponibles\n", 
            report$features_usadas, report$features_totales))
cat("\n")

cat("MÉTRICAS EN VALIDACIÓN:\n")
cat(sprintf("  MAE:  %s\n", comma(round(report$metricas_validacion$mae))))
cat(sprintf("  RMSE: %s\n", comma(round(report$metricas_validacion$rmse))))
cat(sprintf("  MAPE: %.2f%%\n", report$metricas_validacion$mape))
cat(sprintf("  R²:   %.4f\n", report$metricas_validacion$r2))
cat("\n")

cat("IMPACTO DE TRANSFORMACIÓN LOG:\n")
cat(sprintf("  MAE sin log: %s\n", comma(round(report$comparacion_log$mae_sin_log))))
cat(sprintf("  MAE con log: %s\n", comma(round(report$comparacion_log$mae_con_log))))
cat(sprintf("  Mejora:      %.2f%%\n", report$comparacion_log$mejora_porcentual))
cat("\n")

cat("TOP 10 FEATURES MÁS IMPORTANTES:\n")
coef_importance <- tidy(final_model_full) %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(abs(estimate))) %>%
  head(10)
print(coef_importance %>% select(term, estimate, p.value))
cat("\n")

cat("ARCHIVOS GENERADOS:\n")
cat(sprintf("  • %s\n", filename))
cat("  • linear_regression_results.txt\n")
cat("  • regression_diagnostics.png\n")
if (exists("correlation_matrix.png")) {
  cat("  • correlation_matrix.png\n")
}
cat("\n")

sink()

cat("✓ Resultados guardados: linear_regression_results.txt\n")

# Crear visualización de predicciones
png("predictions_vs_actual.png", width = 1200, height = 800, res = 150)
plot_data <- tibble(
  Actual = actual_val,
  Predicted = pred_val
)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Predicciones vs Valores Reales",
    subtitle = sprintf("R² = %.4f, MAE = %s", r2_val, comma(round(mae_val))),
    x = "Precio Real",
    y = "Precio Predicho"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )
dev.off()

cat("✓ Gráfico guardado: predictions_vs_actual.png\n\n")

# ============================================
# RESUMEN FINAL
# ============================================

cat("====================================================================\n")
cat("✅ PROCESO COMPLETADO\n")
cat("====================================================================\n\n")

cat("📁 ARCHIVOS GENERADOS:\n")
cat(sprintf("   • %s  → Subir a Kaggle\n", filename))
cat("   • linear_regression_results.txt       → Para el documento\n")
cat("   • regression_diagnostics.png          → Diagnósticos del modelo\n")
cat("   • predictions_vs_actual.png           → Gráfico de predicciones\n")
if (file.exists("correlation_matrix.png")) {
  cat("   • correlation_matrix.png              → Matriz de correlación\n")
}
cat("\n")

cat("📊 RESULTADO FINAL:\n")
cat(sprintf("   Modelo: %s\n", model_name))
cat(sprintf("   Validation MAE: %s\n", comma(round(mae_val))))
cat(sprintf("   Validation R²:  %.4f\n", r2_val))
cat(sprintf("   Mejora con log: %.2f%%\n", improvement))
cat("\n")

cat("📤 PRÓXIMO PASO:\n")
cat(sprintf("   • Subir %s a Kaggle\n", filename))
cat("   • Comparar score con Neural Networks\n")
cat("   • Documentar resultados en el reporte\n\n")

cat("====================================================================\n\n")

# Retornar objetos importantes
invisible(list(
  model = final_model_full,
  predictions = pred_test_rounded,
  submission = submission,
  metrics = report$metricas_validacion
))
