# Packages ----

library(rvest)
library(ggplot2)


# Fonction & Données ----

source("Warming stripes/Fonction_get_temperatures.R")


dijon <-
  get_temperatures("https://www.historique-meteo.net/france/bourgogne/dijon/",
                   2022)

marseille <-
  get_temperatures("https://www.historique-meteo.net/france/provence-alpes-c-te-d-azur/marseille/",
                   2022)

sainte <-
  get_temperatures("https://www.historique-meteo.net/france/lyonnais/saint-etienne/",
                   2022)

lille <-
  get_temperatures("https://www.historique-meteo.net/france/nord-pas-de-calais/lille/",
                   2022)

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


# Graphiques ----


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


ggplot() +
  geom_col(data = dplyr::mutate(dijon,
                                indic = 1),
           aes(x = date2,
               y = indic,
               color = temp,
               fill = temp)) +
  scale_color_gradientn(colors = rev(mes_couleurs)) +
  scale_fill_gradientn(colors = rev(mes_couleurs)) +
  theme_void() +
  theme(legend.position = "none")


ggsave("Warming stripes/Figures/dijon.png",
       height = 10, width = 18, units = "cm")


ggplot() +
  geom_col(data = dplyr::mutate(marseille,
                                indic = 1),
           aes(x = date2,
               y = indic,
               color = temp,
               fill = temp)) +
  scale_color_gradientn(colors = rev(mes_couleurs)) +
  scale_fill_gradientn(colors = rev(mes_couleurs)) +
  theme_void() +
  theme(legend.position = "none")


ggsave("Warming stripes/Figures/marseille.png",
       height = 10, width = 18, units = "cm")



# Pour faire de l'historique
ggplot() +
  geom_col(data = dplyr::mutate(dijon,
                                date3 = lubridate::year(date2) |> 
                                  as.numeric(),
                                date2 = lubridate::`year<-`(date2,2020),
                                indic = 1),
           aes(x = date2,
               y = indic,
               color = temp,
               fill = temp)) +
  scale_color_gradientn(colors = rev(mes_couleurs)) +
  scale_fill_gradientn(colors = rev(mes_couleurs)) +
  facet_wrap(~date3,
             nrow = 7,
             ncol = 2,
             dir = "v") +
  theme_void() +
  theme(legend.position = "none")

