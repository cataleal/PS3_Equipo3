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

####Limpieza (factors y missings)

vars <- c("property_type", "LocNombre", "SCANOMBRE", "CODIGO_UPZ")
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


####1. Por validación cruzada estándar####
ctrl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = TRUE
)

espec_modelo <- log(price) ~ property_type + cocina_americana + cocina_integral + gimnasio + 
  balcon + chimenea + terraza + ascensor + sauna + jacuzzi + piscina + deposito + 
  walking_closet + duplex + zona_verde + bbq + conjunto_residencial + altillo + 
  vigilancia_24h + porteria + cctv + parqueadero_cubierto + parqueadero_comunal + 
  zona_infantil + salon_comunal + zona_humeda + terraza_comunal + pet_friendly + 
  remodelado + piso_madera + piso_porcelanato + n_parqueaderos + banios + area +
  habitaciones + LocNombre + EPE + EPT + EPCC + distnearestlibrary + distnearestschool + distnearestmuseum +
  distnearesttransmi + recaudo_predial + lon + lat


set.seed(1234)
model1 <- train(
  espec_modelo,
  data = train,
  metric = "MAE",
  method = "glmnet",
  trControl = ctrl,
  family = "gaussian",
  tuneGrid = expand.grid(
    alpha  = seq(0, 1, by= 0.1),
    lambda = 10^seq(-3, 3, length = 10)
  )   
)

model1

predictSample <- test |>
  mutate(
    log_price_hat = predict(model1, newdata = test),
    price_hat = exp(log_price_hat),
    price_hat = round(price_hat / 100000) * 100000
  ) |>
  select(property_id, price_hat)

head(predictSample)


write.csv(predictSample,"EN_lambda_0,001_alpha_0,1.csv", row.names = FALSE)




####2. Por validación cruzada espacial####
train <- train %>%
  mutate(log_price = log(price))

train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3116)

set.seed(2025)
block_folds <- spatial_block_cv(train_sf, v = 5)
autoplot(block_folds)

espec_modelo2 <- as.formula(
  paste("log_price ~ property_type + cocina_americana + cocina_integral + gimnasio + 
  balcon + chimenea + terraza + ascensor + sauna + jacuzzi + piscina + deposito + 
  walking_closet + duplex + zona_verde + bbq + conjunto_residencial + altillo + 
  vigilancia_24h + porteria + cctv + parqueadero_cubierto + parqueadero_comunal + 
  zona_infantil + salon_comunal + zona_humeda + terraza_comunal + pet_friendly + 
  remodelado + piso_madera + piso_porcelanato + n_parqueaderos + banios + area +
  habitaciones + LocNombre + EPE + EPT + EPCC + distnearestlibrary + distnearestschool + distnearestmuseum +
  distnearesttransmi + recaudo_predial")
)

rec <- recipes::recipe(
  espec_modelo2  , data = train) %>%
  step_novel(all_nominal_predictors()) %>% # Categoría para las clases no vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # Variables binarias de las categóricas.
  step_zv(all_predictors()) %>%   #  Elimina predictores con varianza cero (constantes).
  step_normalize(all_predictors())  # Estandariza los predictores.

elastic_spec <- linear_reg(
  penalty = tune(),   # lambda
  mixture = tune()    # alpha (0 = ridge, 1 = lasso)
) %>%
  set_engine("glmnet")

wf <- workflow() %>%
  add_model(elastic_spec) %>%
  add_recipe(rec)


grid_elastic <- crossing(
  mixture = seq(0, 1, by = 0.1),              # alpha
  penalty = 10^seq(-3, 3, length.out = 10)    # lambda
)

set.seed(123)
elastic_res <- tune_grid(
  wf,
  resamples = block_folds,          # <--- AQUÍ entra la CV espacial
  grid      = grid_elastic,
  metrics   = metric_set(mae)
)

best_tune <- select_best(elastic_res, metric = "mae")
best_tune

# Al finalizar el flujo de trabajo, retenemos la especificación óptima únicamente.
en_tuned_final <- finalize_workflow(wf, best_tune)
en_tuned_final_fit <- fit(en_tuned_final, data = train)

# Predicción final. 
pred_test <- augment(en_tuned_final_fit, new_data = test) %>%
  # pasar del log al precio original
  mutate(price_hat = exp(.pred)) %>%
  # redondear al múltiplo de 100.000 más cercano
  mutate(price_hat_round = round(price_hat / 100000) * 100000)

final_submit <- pred_test %>%
  select(property_id, price_hat_round)

write_csv(final_submit, "EN_lambda_0,001_alpha_0,2_spatialcv.csv")
