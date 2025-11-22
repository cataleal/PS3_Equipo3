
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
  out = "views/tablas_continuas.tex",   # <── AQUÍ SE GUARDA
  summary.stat = c("n", "mean", "sd", "min", "max"),
  covariate.labels = c(
    "Habitaciones",
    "Distancia a biblioteca más cercana",
    "Distancia a colegio más cercano",
    "Distancia a museo más cercano",
    "Distancia a Transmilenio más cercano",
    "Baños"
  ),
  digits = 3,
  label = "tab:resumen_continuas"
)