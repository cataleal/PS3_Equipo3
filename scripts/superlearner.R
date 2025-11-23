library(pacman) 

p_load(rio,          # Lectura de datos.
       tidyverse,    # Manipulación de datos.
       sf,           # Manejo de datos espaciales.
       nnls,         # Uso de mínimos cuadrados no-negativos.
       xgboost,      # XBGoost.
       tidymodels,   # Modelos de Macuine Learning.
       spatialsample # Validación cruzada espacial
) 

# Instalamos la versión más reciente de sl3 de GitHub. 
if (!require(sl3)) {
  remotes::install_github("tlverse/sl3")
  library(sl3)
  library(origami) # Validación cruzada diseñada para sl3.
}

setwd(choose.dir())
train <- read_csv("stores/train_final.csv")
test  <- read_csv("stores/test_final.csv")

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

##########MODELO###############
#0. Hacer los folds ()
set.seed(2025)
folds <- origami::make_folds(n = nrow(train), V = 5)

train <- train %>%
  mutate(log_price = log(price))

# Paso 1: Definir el problema de predicción.
task <- sl3::sl3_Task$new(
  data = train,
  covariates = c(
    "property_type", "habitaciones", "distnearestlibrary",
    "distnearestschool", "distnearestmuseum", "distnearesttransmi", "banios",
    "parqueadero_cubierto", "zona_humeda", "walking_closet", "zona_verde",
    "chimenea", "jacuzzi", "piscina", "gimnasio", "balcon",
    "parqueadero_comunal", "terraza", "area"), 
  outcome = "log_price",
  folds = folds
)

# Paso 2: Definir los learners individuales y agruparlos.
learners <- Stack$new(
  Lrnr_glmnet$new(alpha = 0.1, nlambda = 50, family = "gaussian"), 
  Lrnr_xgboost$new(eta = 0.262, nrounds = 941, max_depth = 5, min_child_weight = 3), 
  Lrnr_nnet$new(size = 10, decay = 0.001, linout = TRUE, maxit = 500, trace = FALSE),
  Lrnr_ranger$new(num.trees = 500, mtry = 9, min.node.size = 5)
)

# Paso 3: Definir el metalearner. En este caso, mínimos cuadrados no-negativos.
metalearner <- Lrnr_nnls$new()

# Paso 4: Definir el superlearner. Este involucra a los learners y al metalearner.
sl <- Lrnr_sl$new(learners = learners,
                  metalearner = metalearner)

# Paso 5: Estimar el superlearner. Como algunos algoritmos pueden depender de
# valores aleatorios, definimos una semilla antes del entrenamiento.
set.seed(2025)
sl_fit <- sl$train(task = task)

sl_fit$coefficients
sl_fit$coefficients %>%
  enframe(name = "learner", value = "peso") %>%
  ggplot(aes(x = reorder(learner, peso), y = peso, fill = learner)) +
  geom_col() +
  coord_flip() +
  labs(title = "Pesos del Super Learner", y = "Peso", x = "Modelo") +
  theme_minimal()



test_task <- sl3_Task$new(data = test, 
                                covariates = c("property_type", "habitaciones", "distnearestlibrary",
                                               "distnearestschool", "distnearestmuseum", "distnearesttransmi", "banios",
                                               "parqueadero_cubierto", "zona_humeda", "walking_closet", "zona_verde",
                                               "chimenea", "jacuzzi", "piscina", "gimnasio", "balcon",
                                               "parqueadero_comunal", "terraza", "area"))
preds_sl_log <- sl_fit$predict(task = test_task)
sl_pred_price <- exp(preds_sl_log)
sl_pred_price_round <- round(sl_pred_price / 100000) * 100000

submission <- data.frame(
  property_id    = test$property_id,
  price = sl_pred_price_round
)
write.csv(submission, "SL_0.35XGB_0.64RF.csv", row.names = FALSE)

##########CON CV ESPACIAL############
train <- train %>%
  mutate(log_price = log(price))

train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3116)

set.seed(2025)
block_folds <- spatial_block_cv(train_sf, v = 5)
autoplot(block_folds)

#extraer ID de cada fold
num_obs <- nrow(train)
vec_obs <- 1:nrow(train)
fold_id <- data.frame(ID = integer(),
                      num_fold = integer())

for (k in 1:length(block_folds$splits)) {
  temp_id <- setdiff(vec_obs, block_folds$splits[[k]][['in_id']]) #número de fila en ese split
  temp_db <- data.frame(ID = temp_id,
                        num_fold = k)
  fold_id <- fold_id |> bind_rows(temp_db)
}

fold_id <- fold_id |> arrange(ID)
fold_id <- fold_id$num_fold

folds <- origami::make_folds(fold_id)