# Packages ----

library(rvest)
library(ggplot2)


# Fonction & Données ----

source("Warming stripes/Fonction_get_temperatures.R")


dijon <-
  get_temperatures("https://www.historique-meteo.net/france/bourgogne/dijon/")

marseille <-
  get_temperatures("https://www.historique-meteo.net/france/provence-alpes-c-te-d-azur/marseille/")

sainte <-
  get_temperatures("https://www.historique-meteo.net/france/lyonnais/saint-etienne/")

lille <-
  get_temperatures("https://www.historique-meteo.net/france/nord-pas-de-calais/lille/")

temperatures <-
  rbind(
    dplyr::mutate(marseille,
                  ville = "Marseille",
                  indic = 1),
    dplyr::mutate(dijon,
                  ville = "Dijon",
                  indic = 1),
    dplyr::mutate(sainte,
                  ville = "Saint-Étienne",
                  indic = 1),
    dplyr::mutate(lille,
                  ville = "Lille",
                  indic = 1)
  )


# Graphique ----


mes_couleurs <-
  RColorBrewer::brewer.pal(11, "RdBu")


ggplot() +
  geom_col(data = temperatures,
           aes(x = date2,
               y = indic,
               color = temp,
               fill = temp)) +
  scale_color_gradientn(colors = rev(mes_couleurs)) +
  scale_fill_gradientn(colors = rev(mes_couleurs)) +
  facet_wrap(~ville,
             nrow = 4) +
  theme_void() +
  theme(legend.position = "none")




