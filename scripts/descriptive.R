
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