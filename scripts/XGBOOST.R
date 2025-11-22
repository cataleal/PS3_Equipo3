#---------#
#-XGBOOST-#
#---------#

setwd(choose.dir())

library(pacman)

p_load(rio,       # Import/export data.
       tidyverse, # Tidy-data.
       caret,     # For predictive model assessment.
       sf,           # Manejo de datos espaciales.
       spatialsample, # Validación cruzada espacial
       tidymodels,
       leaps)     # For subset  model selection

train <- read_csv("stores/train_final.csv")
test  <- read_csv("stores/test_final.csv")

train_lasso <- read_csv("stores/train_final_lasso.csv")
test_lasso  <- read_csv("stores/test_final_lasso.csv")

####Limpieza (factors y missings)

vars <- c("property_type", "LocNombre")
train[vars] <- lapply(train[vars], as.factor)
test[vars] <- lapply(test[vars], as.factor)

train <- train |>
  mutate(across(
    where(~ is.numeric(.x) && all(.x %in% c(0,1))), 
    ~ factor(.x)
  ))

test <- test |>
  mutate(across(
    where(~ is.numeric(.x) && all(.x %in% c(0,1))), 
    ~ factor(.x)
  ))


train$...1 <- NULL
test$...1 <- NULL
train <- train %>% tidyr::drop_na()
train <- train |>
  select(where(~ n_distinct(.x) > 1))

test <- test %>%
  mutate(
    area = ifelse(is.na(area), median(area, na.rm = TRUE), area)
  )

#----------------#
#---Tidymodels---#
#----------------#

train <- train %>%
  mutate(log_price = log(price))

train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3116)

set.seed(2025)
block_folds <- spatial_block_cv(train_sf, v = 5)
autoplot(block_folds)

espec_modelo_xgb <- as.formula(
  paste("log_price ~ property_type + Locnombre + habitaciones + distnearestlibrary +
  distnearestschool + distnearestmuseum + distnearesttransmi + banios + parqueadero_cubierto +
  zona_humeda + walking_closet + zona_verde + chimenea + jacuzzi + piscina + gimnasio + 
  balcon + parqueadero_comunal + terraza")
)

rec_xgb <- recipes::recipe(
  espec_modelo_xgb  , data = train) %>%
  step_novel(all_nominal_predictors()) %>% # Categoría para las clases no vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # Variables binarias de las categóricas.
  step_zv(all_predictors()) %>%   #  Elimina predictores con varianza cero (constantes).
  step_normalize(all_predictors())  # Estandariza los predictores.

xgb_spec <- boost_tree(
  mode           = "regression",
  trees          = tune(),  # nrounds
  tree_depth     = tune(),  # max_depth
  learn_rate     = tune(),  # eta
  loss_reduction = tune(),  # gamma
  min_n          = tune(),  # min_child_weight
  sample_size    = tune(),  # subsample
  mtry           = tune()   # colsample_bytree
) %>%
  set_engine("xgboost")


xgb_wf <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(rec_xgb)


# definimos rangos razonables
trees_range      <- trees(c(100, 500))          # número de árboles
tree_depth_range <- tree_depth(c(2L, 8L))        # profundidad del árbol
learn_rate_range <- learn_rate(c(-4, -1))         # log10 scale: 10^-3 a 10^0
loss_red_range   <- loss_reduction(c(0, 5))     # gamma (log10)
min_n_range      <- min_n(c(20, 100))            # min_child_weight
sample_range     <- sample_prop(c(0.3, 0.8))     # subsample
mtry_range       <- mtry(c(5L, 25L))             # depende de # de predictores

grid_xgb <- grid_latin_hypercube(
  trees_range,
  tree_depth_range,
  learn_rate_range,
  loss_red_range,
  min_n_range,
  sample_range,
  mtry_range,
  size = 50  # número de combinaciones
)

install.packages("xgboost")
library(xgboost)

set.seed(123)

xgb_res <- tune_grid(
  xgb_wf,
  resamples = block_folds,              
  grid      = grid_xgb,
  metrics   = metric_set(mae),
  control   = control_grid(verbose = TRUE)
)
xgb_res
mae_espacial <- collect_metrics(xgb_res) %>% 
  filter(.metric == "mae") %>%
  select(mean, std_err) %>%
  mutate(tipo = "CV espacial")

best_xgb <- select_best(xgb_res, metric="mae")   
best_xgb

# Al finalizar el flujo de trabajo, retenemos la especificación óptima únicamente.
xgb_final_wf <- finalize_workflow(xgb_wf, best_xgb)
xgb_final_fit <- fit(xgb_final_wf, data = train_sf)

#predicciones
xgb_pred_test <- augment(xgb_final_fit, new_data = test) %>%
  mutate(
    price_hat       = exp(.pred),
    price_hat_round = round(price_hat / 100000) * 100000
  ) %>%
  select(property_id, price_hat_round)

write_csv(xgb_pred_test, "XGB_mtry10_tree707_min_n18_depth7_lrate_0027_cvspatial.csv")

##########################################
#----------------------------------------#
#---CON VALIDACIÓN CRUZADA TRADICIONAL---#
#----------------------------------------#
##########################################

#La receta y la grilla sigue igual, solo cambio cómo saco los folds
set.seed(2025)

folds_normal <- vfold_cv(
  train_sf,
  v = 5,                 # número de folds (como el v de spatial_block_cv)
  strata = log_price     # opcional pero recomendado para regresión
)

set.seed(123)

xgb_res_normal <- tune_grid(
  xgb_wf,
  resamples = folds_normal,              
  grid      = grid_xgb,
  metrics   = metric_set(mae),
  control   = control_grid(verbose = TRUE)
)
xgb_res_normal

mae_normal <- collect_metrics(xgb_res_normal) %>% 
  filter(.metric == "mae") %>%
  select(mean, std_err) %>%
  mutate(tipo = "CV tradicional")

best_xgb_normal <- select_best(xgb_res_normal, metric="mae")   
best_xgb_normal

# Al finalizar el flujo de trabajo, retenemos la especificación óptima únicamente.
xgb_final_wf_n <- finalize_workflow(xgb_wf, best_xgb_normal)
xgb_final_fit_n <- fit(xgb_final_wf_n, data = train_sf)

#predicciones
xgb_pred_test_norm <- augment(xgb_final_fit_n, new_data = test) %>%
  mutate(
    price_hat       = exp(.pred),
    price_hat_round = round(price_hat / 100000) * 100000
  ) %>%
  select(property_id, price_hat_round)

write_csv(xgb_pred_test_norm, "XGB_mtry15_tree276_min_n41_depth4_lrate_0023_loss1.34_size0654_cvnormal.csv")

#############################################
#---COMPARACIÓN DEL MAE ENTRE TIPOS DE CV---#
#############################################
comparacion_mae <- bind_rows(mae_espacial, mae_normal)
comparacion_mae

mae_normal%>%
  summarise(mae_promedio = mean(mean))

saveRDS(comparacion_mae, "comp_mae_xgboost.rds")



