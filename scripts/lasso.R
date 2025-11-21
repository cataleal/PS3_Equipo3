setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
library(pacman)

p_load(
  rio, tidyverse, caret, sf, spatialsample, glmnet
)

# ========================================
# 1. Load data
# ========================================

train <- read_csv("stores/train_final.csv")
test  <- read_csv("stores/test_final.csv")

vars <- c("property_type", "LocNombre", "SCANOMBRE", "CODIGO_UPZ")
train[vars] <- lapply(train[vars], as.factor)
test[vars]  <- lapply(test[vars],  as.factor)

train <- train |>
  mutate(across(where(~ is.numeric(.x) && all(.x %in% c(0,1))), ~ factor(.x)))

test <- test |>
  mutate(across(where(~ is.numeric(.x) && all(.x %in% c(0,1))), ~ factor(.x)))

train$...1 <- NULL
test$...1  <- NULL

train <- train %>% drop_na()
train <- train |> select(where(~ n_distinct(.x) > 1))

test <- test %>%
  mutate(area = ifelse(is.na(area), median(area, na.rm = TRUE), area))

# ========================================
# 2. Define formula
# ========================================

espec_modelo <- log(price) ~ property_type + cocina_americana + cocina_integral +
  gimnasio + balcon + chimenea + terraza + ascensor + sauna + jacuzzi +
  piscina + deposito + walking_closet + duplex + zona_verde + bbq +
  conjunto_residencial + altillo + vigilancia_24h + porteria + cctv +
  parqueadero_cubierto + parqueadero_comunal + zona_infantil + 
  salon_comunal + zona_humeda + terraza_comunal + pet_friendly + 
  remodelado + piso_madera + piso_porcelanato + n_parqueaderos + 
  banios + area + habitaciones + LocNombre + EPE + EPT + EPCC + 
  distnearestlibrary + distnearestschool + distnearestmuseum +
  distnearesttransmi + recaudo_predial + lon + lat

# ========================================
# 3. Cross-validation (standard)
# ========================================

ctrl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = TRUE
)

# ========================================
# 4. LASSO model (alpha = 1)
# ========================================

set.seed(1234)

lasso_model <- train(
  espec_modelo,
  data = train,
  metric = "MAE",
  method = "glmnet",
  trControl = ctrl,
  family = "gaussian",
  tuneGrid = expand.grid(
    alpha  = 1,                                # LASSO
    lambda = 10^seq(-3, 3, length = 50)        # wide lambda grid
  )
)

lasso_model

# Extract best lambda
best_lambda <- lasso_model$bestTune$lambda
best_lambda

# Extract coefficients
coef_lasso <- coef(lasso_model$finalModel, s = best_lambda)

# Convert to tibble
coef_df <- as.matrix(coef_lasso) %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  rename(coef = 2) %>%
  filter(variable != "(Intercept)") %>%
  mutate(abs_coef = abs(coef)) %>%
  arrange(desc(abs_coef)) %>% 
  filter(!if_any(everything(), ~ grepl("Loc", .x)))

variables_selected <- coef_df$variable[1:15] 
variables_selected <- gsub("1","",variables_selected)
variables_selected <- gsub("Casa","",variables_selected)

variables_lasso <- c("property_id","price","LocNombre","habitaciones","distnearestlibrary","distnearestschool",
                     "distnearestmuseum","distnearesttransmi",variables_selected)

predictSample <- test |>
  mutate(log_price_hat = predict(lasso_model, newdata = test),
         price_hat = exp(log_price_hat)) |>
  select(property_id, price_hat) |> mutate(price = round(price_hat,-6)) |>
  select(property_id, price)

# Saving the predictions

write.csv(predictSample,"stores/models/lasso_0.01.csv", row.names = FALSE)

# Computing Selected Variables

train_lasso <- train %>% select(all_of(variables_lasso))
test_lasso <- test %>% select(all_of(variables_lasso))

write.csv(train_lasso,"stores/train_final_lasso.csv")
write.csv(test_lasso,"stores/test_final_lasso.csv")
