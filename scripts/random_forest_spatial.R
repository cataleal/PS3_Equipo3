# === Working directory
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

library(tidymodels)
library(sf)
library(spatialsample)
library(doParallel)
library(dplyr)
library(ggplot2)

set.seed(2025)

# ==========================================
# 1. Load data
# ==========================================
train <- read.csv("stores/train_final_lasso.csv") %>% select(-X)
train$price <- log(train$price)

test  <- read.csv("stores/test_final_lasso.csv") %>% select(-X)

# ==========================================
# 1b. Convert geometry → lon, lat
# ==========================================
if ("geometry" %in% names(train)) {
  geo_sf <- st_as_sfc(train$geometry)
  geo_mat <- st_coordinates(geo_sf)
  
  train$lon <- geo_mat[, "X"]
  train$lat <- geo_mat[, "Y"]
  
  train$geometry <- NULL
}

# ==========================================
# 2. VALIDACIÓN CRUZADA ESPACIAL
# ==========================================
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

set.seed(2025)
folds <- spatial_block_cv(train_sf, v = 5)   # ⬅ ESPACIAL

# Convertir a tibble sin geometría
train_nogeo <- st_drop_geometry(train_sf)

# ==========================================
# 3. Recipe
# ==========================================
rec_rf <- recipe(price ~ ., data = train_nogeo %>% select(- property_id))

# ==========================================
# 4. Random Forest model
# ==========================================
spec_rf <- rand_forest(
  mtry  = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

wf_rf <- workflow() %>%
  add_recipe(rec_rf) %>%
  add_model(spec_rf)

# ==========================================
# 5. Grid (lo mismo que antes)
# ==========================================
grid_rf <- crossing(
  mtry  = c(3, 6, 9),
  trees = c(200, 500),
  min_n = c(2, 5)
)

# ==========================================
# 6. Parallel computing
# ==========================================
ncores <- parallel::detectCores()
registerDoParallel(ncores)

# ==========================================
# 7. Tune with spatial cross-validation
# ==========================================
tune_rf <- tune_grid(
  wf_rf,
  resamples = folds,         # ⬅ ESPACIAL
  grid = grid_rf,
  metrics = metric_set(mae),
  control = control_grid(save_pred = FALSE, verbose = FALSE)
)

# ==========================================
# 8. Best model
# ==========================================
best_rf <- select_best(tune_rf, metric = "mae")
best_rf

rf_final <- finalize_workflow(
  wf_rf,
  best_rf
)

rf_fit <- fit(rf_final, data = train_nogeo)

# ==========================================
# 9. Predict on test
# ==========================================
test$price <- exp(predict(rf_fit, new_data = test)$.pred)
test$price <- round(test$price, -6)

predict <- test %>% select(property_id, price)

write.csv(predict, "stores/models/random_forest_cv_spatial_6_200_2.csv", row.names = FALSE)

# ==========================================
# 10. Variable Importance
# ==========================================

rf_ranger <- rf_fit$fit$fit

imp_df <- data.frame(
  variable = names(rf_ranger$fit$variable.importance),
  importance = rf_ranger$fit$variable.importance
)

imp_df <- imp_df %>%
  arrange(desc(importance)) %>% 
  filter(variable != "X") %>%
  mutate(importance = 100 * importance / importance[1])

plot_importance <- ggplot(imp_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Variable Importance (Spatial CV)",
    x = "",
    y = ""
  )

ggsave("views/plot_vi_rf_spatial.png", plot_importance, dpi = 100,
       width = 16, height = 9, unit = "in")