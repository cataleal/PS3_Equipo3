
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

library(pacman)
p_load(
  readr,      # read_csv
  dplyr,      # filter, mutate, pipes
  sf,         # st_read, st_as_sf, st_transform
  leaflet,    # leaflet maps
  ggplot2,    # plotting
  cowplot     # plotting
)


train <- read_csv("stores/train_final.csv")
test  <- read_csv("stores/test_final.csv")


train <- train |>
  filter(!is.na(lat) & !is.na(lon))

leaflet() |>
  addTiles() |>
  addCircles(lng = train$lon, 
             lat = train$lat)

shp <- st_read("stores/Localidad/Loca.shp") %>% filter(LocNombre != "SUMAPAZ")  %>% filter(LocNombre != "BOSA") %>%
       filter(LocNombre != "USME")  %>% filter(LocNombre != "CIUDAD BOLIVAR")

shp2 <- st_read("stores/Indicador UPZ/IndUPZ.shp") %>% filter(!CODIGO_UPZ %in% c(61,62,63,64,67,68,70,66,86,87,84,85,53,34,32,54,32,89,50,51,60,54,59,57,1,2,3,52,55,56,58))
train_sf <- st_as_sf(train, 
                     coords = c("lon", "lat"), 
                     crs = 4326) %>% mutate(price = 10^(-9)*price)

test_sf <- st_as_sf(test,
                    coords = c("lon", "lat"),
                    crs = 4326)  

train_sf <- st_as_sf(train, 
                     coords = c("lon", "lat"), 
                     crs = 4326) %>%
  mutate(price = 1e-9 * price,
         price_cat = cut(price,
                         breaks = c(0.3,0.4, 0.6, 0.8, 1.0, 1.2, 1.4, Inf),
                         labels = c("0.3–0.4",
                                    "0.4–0.6",
                                    "0.6–0.8",
                                    "0.8–1.0",
                                    "1.0–1.2",
                                    "1.2–1.4",
                                    "> 1.4"),
                         include.lowest = TRUE))

price_colors <- c(
  "0.3–0.4" = "lightblue",   # pale yellow
  "0.4–0.6" = "lightgreen",
  "0.6–0.8" = "forestgreen",
  "0.8–1.0" = "yellow",
  "1.0–1.2" = "orange",
  "1.2–1.4" = "indianred",
  "> 1.4"   = "darkred"    # black (high price)
)

shp <- st_transform(shp, 4326)
shp2 <- st_transform(shp2, 4326)

plot_all_prop <- ggplot() +
  geom_sf(data = shp2, fill = "grey90", color = "black") +
  geom_sf(data = train_sf,
          aes(color = price_cat),
          size = 1) +
  scale_color_manual(values = price_colors,
                     name = "Price range") +
  guides(color = guide_legend(override.aes = list(shape = 15, size = 6))) +
  labs(title = "All Properies") +
  theme_minimal()

plot_casa <- ggplot() +
  geom_sf(data = shp2, fill = "grey90", color = "black") +
  geom_sf(data = train_sf %>% filter(property_type == "Casa"),
          aes(color = price_cat),
          size = 1) +
  scale_color_manual(values = price_colors,
                     name = "Precio") +
  guides(color = guide_legend(override.aes = list(shape = 15, size = 6))) +
  labs(title = "Casa") +
  theme_minimal()

plot_apt <- ggplot() +
  geom_sf(data = shp2, fill = "grey90", color = "black") +
  geom_sf(data = train_sf %>% filter(property_type == "Apartamento"),
          aes(color = price_cat),
          size = 1) +
  scale_color_manual(values = price_colors,
                     name = "Precio") +
  guides(color = guide_legend(override.aes = list(shape = 15, size = 6))) +
  labs(title = "Apartamento") +
  theme_minimal()

plot_test <- ggplot() +
  geom_sf(data = shp2, fill = "grey90", color = "black") +
  geom_sf(data = test_sf,
          size = 1) +
    labs(title = "Test") +
  theme_minimal()

plots_map <- plot_grid(plot_casa,plot_apt,plot_test,
                         ncol = 3,
                         align = "hv")

ggsave("views/maps.png", plots_map,
       width = 16, height = 4, units = "in",dpi = 100, bg = "white") 