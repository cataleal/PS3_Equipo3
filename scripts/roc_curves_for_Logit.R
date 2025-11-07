# === Establecer el directorio de trabajo ===
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

# === Cargar paquetes ===
library(pacman)
p_load(
  tidyverse,
  caret,
  pROC
)

# === Lectura de datos ===
train <- read_csv("stores/train.csv")
train <- train %>% na.omit()
train$Pobre <- factor(train$Pobre, levels = c("No", "Yes"))

# === Función para entrenar modelo logit y obtener curva ROC ===
entrenar_y_roc_logit <- function(sampling_method = NULL, label) {
  
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = TRUE,
    sampling = sampling_method
  )
  
  set.seed(2025)
  modelo <- train(
    Pobre ~ .,
    data = train,
    method = "glm",
    family = binomial,
    metric = "ROC",
    trControl = ctrl
  )
  
  preds <- modelo$pred
  
  roc_obj <- roc(
    response = preds$obs,
    predictor = preds$Yes,
    levels = c("No", "Yes"),
    direction = "<"
  )
  
  roc_df <- data.frame(
    TPR = roc_obj$sensitivities,
    FPR = 1 - roc_obj$specificities,
    Modelo = label,
    AUC = round(auc(roc_obj), 3)
  )
  
  return(roc_df)
}

# === Entrenar tres versiones del modelo ===
roc_base <- entrenar_y_roc_logit(NULL, "Sin muestreo")
roc_up   <- entrenar_y_roc_logit("up", "Upsampling")
roc_down <- entrenar_y_roc_logit("down", "Downsampling")

# === Combinar resultados ===
roc_data <- bind_rows(roc_base, roc_up, roc_down)

# === Crear figura comparativa ===
fig_roc <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Modelo)) +
  geom_line(size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50") +
  labs(
    title = "Curvas ROC del modelo Logit con diferentes esquemas de muestreo",
    x = "Tasa de Falsos Positivos (1 - Especificidad)",
    y = "Tasa de Verdaderos Positivos (Sensibilidad)",
    color = "Método"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# === Crear carpeta si no existe ===
if (!dir.exists("figures")) dir.create("figures")

# === Guardar figura ===
ggsave("views/ROC_Logit_sampling.png", fig_roc, width = 8, height = 6, dpi = 300)

# === Mostrar AUC promedio en consola ===
roc_data %>%
  group_by(Modelo) %>%
  summarise(AUC = unique(AUC))