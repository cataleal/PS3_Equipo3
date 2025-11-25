
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

library(pacman)
p_load(
  readr,      # read_csv
  dplyr,      # filter, mutate, pipes
  sf,         # st_read, st_as_sf, st_transform
  leaflet,    # leaflet maps
  ggplot2,    # plotting
  stargazer
)


train <- read_csv("stores/train_final_lasso.csv")
test  <- read_csv("stores/test_final_lasso.csv")


train_continuous <- train %>% select(habitaciones, distnearestlibrary,distnearestschool,distnearestmuseum,
                                     distnearesttransmi,banios) %>%
  mutate(across(everything(), as.numeric))

stargazer(
  as.data.frame(train_continuous),
  type = "latex",
  summary = TRUE,
  title = "Estadísticas descriptivas de las variables continuas",
  out = "views/frecuencia_continuas.tex",   # <── AQUÍ SE GUARDA
  summary.stat = c("n", "mean", "sd", "min", "max"),
  covariate.labels = c(
    "Habitaciones",
    "Distancia a biblioteca más cercana",
    "Distancia a colegio más cercano",
    "Distancia a museo más cercano",
    "Distancia a Transmilenio más cercano",
    "Baños"
  ),
  digits = 1,
  label = "tab:resumen_continuas"
)

test_continuous <- test %>% select(habitaciones, distnearestlibrary,distnearestschool,distnearestmuseum,
                                     distnearesttransmi,banios) %>%
  mutate(across(everything(), as.numeric))

stargazer(
  as.data.frame(test_continuous),
  type = "latex",
  summary = TRUE,
  title = "Estadísticas descriptivas de las variables continuas - Test",
  out = "views/frecuencia_continuas_test.tex",   # <── AQUÍ SE GUARDA
  summary.stat = c("n", "mean", "sd", "min", "max"),
  covariate.labels = c(
    "Habitaciones",
    "Distancia a biblioteca más cercana",
    "Distancia a colegio más cercano",
    "Distancia a museo más cercano",
    "Distancia a Transmilenio más cercano",
    "Baños"
  ),
  digits = 1,
  label = "tab:resumen_continuas"
)

train_categorical <- train %>% select(property_type,parqueadero_cubierto,zona_humeda,walking_closet,zona_verde,
                                      chimenea,jacuzzi, piscina,balcon, gimnasio, parqueadero_cubierto,parqueadero_comunal,terraza) %>%
  mutate(across(everything(), as.factor))

stargazer(train_categorical,summary = TRUE)

freq_list <- lapply(names(train_categorical), function(varname) {
  data.frame(
    Variable = varname,
    Categoria = names(table(train_categorical[[varname]])),
    Frecuencia = as.numeric(table(train_categorical[[varname]]))
  )
})

freq_df <- do.call(rbind, freq_list) %>% mutate(Frecuencia = 100*Frecuencia/nrow(train))

stargazer(
  freq_df,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  title = "Frecuencia de variables categóricas",
  out = "views/frecuencias_categoricas.tex",
  label = "tab:frecuencias_categoricas",
  covariate.labels = c("Variable", "Categoría", "Frecuencia"),
  digits = 1
)


test_categorical <- test %>% select(property_type,parqueadero_cubierto,zona_humeda,walking_closet,zona_verde,
                                      chimenea,jacuzzi, piscina,balcon, gimnasio, parqueadero_cubierto,parqueadero_comunal,terraza) %>%
  mutate(across(everything(), as.factor))

stargazer(test_categorical,summary = TRUE)

freq_list <- lapply(names(test_categorical), function(varname) {
  data.frame(
    Variable = varname,
    Categoria = names(table(test_categorical[[varname]])),
    Frecuencia = as.numeric(table(test_categorical[[varname]]))
  )
})

freq_df <- do.call(rbind, freq_list) %>% mutate(Frecuencia = 100*Frecuencia/nrow(test))

stargazer(
  freq_df,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  title = "Frecuencia de variables categóricas",
  out = "views/frecuencias_categoricas_test.tex",
  label = "tab:frecuencias_categoricas_test",
  covariate.labels = c("Variable", "Categoría", "Frecuencia"),
  digits = 1
)



plot <- ggplot(train, aes(x = price*10^-9)) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(title = "Todas Propriedades",
       x = "Precio",
       y = "Frecuencia")

plot_casa <- ggplot(train %>% filter(property_type== "Casa"), aes(x = price*10^-9)) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(title = "Casa",
       x = "Precio",
       y = "Frecuencia")

plot_apt <- ggplot(train %>% filter(property_type== "Apartamento"), aes(x = price*10^-9)) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(title = "Apartamento",
       x = "Precio",
       y = "Frecuencia")

plots_hist <- plot_grid(plot_casa,plot_apt,plot,
                       ncol = 3,
                       align = "hv")

ggsave("views/price_hist.png", plots_hist,
       width = 16, height = 4, units = "in",dpi = 200, bg = "white") 


# ================================
# 1. VARIABLES CONTINUAS
# ================================

train_continuous <- train %>% 
  select(habitaciones, distnearestlibrary, distnearestschool,
         distnearestmuseum, distnearesttransmi, banios) %>%
  mutate(across(everything(), as.numeric))

test_continuous <- test %>%
  select(habitaciones, distnearestlibrary, distnearestschool,
         distnearestmuseum, distnearesttransmi, banios) %>%
  mutate(across(everything(), as.numeric))

mean_diff_row <- function(var) {
  x <- train_continuous[[var]]
  y <- test_continuous[[var]]
  
  ttest <- t.test(x, y)
  
  data.frame(
    Variable   = var,
    Media_Train = mean(x, na.rm = TRUE),
    Media_Test  = mean(y, na.rm = TRUE),
    Diff_Media  = mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE),
    P_value     = ttest$p.value
  )
}

diff_continuas <- do.call(rbind, lapply(names(train_continuous), mean_diff_row))

diff_continuas <- diff_continuas %>%
  mutate(across(-Variable, round, 3))

# Exportar a LaTeX
stargazer(
  diff_continuas,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  title = "Diferencias de medias entre Train y Test (Variables continuas)",
  label = "tab:diff_continuas",
  out = "views/diff_continuas.tex",
  digits = 2
)

# ======# ================================
# 2. VARIABLES CATEGÓRICAS — VERSIÓN CORRECTA
# ================================

train_categorical <- train %>%
  select(property_type, parqueadero_cubierto, zona_humeda, walking_closet,
         zona_verde, chimenea, jacuzzi, piscina, balcon, gimnasio,
         parqueadero_comunal, terraza) %>%
  mutate(across(everything(), as.factor))

test_categorical <- test %>%
  select(property_type, parqueadero_cubierto, zona_humeda, walking_closet,
         zona_verde, chimenea, jacuzzi, piscina, balcon, gimnasio,
         parqueadero_comunal, terraza) %>%
  mutate(across(everything(), as.factor))

prop_diff_list <- lapply(names(train_categorical), function(varname) {
  
  # conteos absolutos en train y test
  ct_train <- table(train_categorical[[varname]])
  ct_test  <- table(test_categorical[[varname]])
  
  # unificar categorías
  all_cats <- union(names(ct_train), names(ct_test))
  
  ct_train_full <- sapply(all_cats, function(c) ifelse(c %in% names(ct_train), ct_train[c], 0))
  ct_test_full  <- sapply(all_cats, function(c) ifelse(c %in% names(ct_test),  ct_test[c], 0))
  
  # proporciones para la tabla (no para el test)
  pt <- round(ct_train_full / sum(ct_train_full), 3)
  ps <- round(ct_test_full  / sum(ct_test_full), 3)
  
  # test chi-cuadrado usa CONTEOS, no proporciones
  pval <- suppressWarnings(chisq.test(rbind(ct_train_full, ct_test_full))$p.value)
  
  data.frame(
    Variable   = varname,
    Categoria  = all_cats,
    Prop_Train = pt,
    Prop_Test  = ps,
    Diff_Prop  = round(pt - ps, 3),
    P_value    = round(pval, 4)
  )
})

diff_categoricas <- do.call(rbind, prop_diff_list)

stargazer(
  diff_categoricas,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  title = "Diferencias de proporciones entre Train y Test (Variables categóricas)",
  label = "tab:diff_categoricas",
  out = "views/diff_categoricas.tex",
  digits = 2
)