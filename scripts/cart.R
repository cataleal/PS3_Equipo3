# === Working directory
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

library(tidymodels)
library(sf)
library(spatialsample)
library(rpart)
library(rpart.plot)
library(doParallel)
library(dplyr)

# ==========================================
# 1. Load data
# ==========================================
train <- read.csv("stores/train_final.csv")
train$price <- log(train$price)

test  <- read.csv("stores/test_final.csv")

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
# 2. Create spatial folds
# ==========================================
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

set.seed(2025)
block_folds <- spatial_block_cv(train_sf, v = 5)

# ==========================================
# 3. Convert back to regular df
# ==========================================
train_nogeo <- st_drop_geometry(train_sf)

# ==========================================
# 4. Recipe
# ==========================================
rec_cart <- recipe(price ~ ., data = train_nogeo)

# ==========================================
# 5. CART model
# ==========================================
spec_cart <- decision_tree(
  cost_complexity = tune(),
  tree_depth      = tune(),
  min_n           = tune()
) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

wf_cart <- workflow() %>%
  add_recipe(rec_cart) %>%
  add_model(spec_cart)

# ==========================================
# 6. FASTEST: Manual grid (3 combinations)
# ==========================================
grid_cart <- tibble(
  cost_complexity = c(0.0001, 0.001, 0.01,0.1),
  tree_depth      = c(6, 8, 10,15,20),
  min_n           = c(50, 60, 100, 200, 500)
)

# ==========================================
# 7. Parallel computing
# ==========================================
ncores <- parallel::detectCores()
registerDoParallel(ncores)

# ==========================================
# 8. Spatial CV
# ==========================================
tune_cart <- tune_grid(
  wf_cart,
  resamples = block_folds,
  grid = grid_cart,
  metrics = metric_set(mae),
  control = control_grid(save_pred = FALSE, verbose = FALSE)
)

# ==========================================
# 9. Best results
# ==========================================
best_cart <- select_best(tune_cart, metric = "mae")
best_cart

cart_final <- finalize_workflow(
  wf_cart,
  best_cart
)

cart_fit <- fit(cart_final, data = train_nogeo)

test$price <- exp(predict(cart_fit, new_data = test)$.pred)

test$price <- round(test$price,-6)

predict <- test %>% select(property_id, price)

write.csv(predict,"stores/models/cart_0.01_10_20.csv", row.names = FALSE)