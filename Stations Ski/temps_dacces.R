# Packages ----

library(tidyverse)
library(data.table)
library(sf)



transform_shapefile <- function(shp){
  
  
  continent <-
    shp %>% dplyr::filter(REG != "94")
  
  corse <- shp %>% dplyr::filter(REG == "94") %>%
    dplyr::mutate(geometry = geometry + c(-166400,0)) %>%
    sf::st_set_crs(2154)
  
  
  shp_dep_trans = continent %>%
    dplyr::bind_rows(corse)
  
  return(shp_dep_trans)
}


# Données ----

matrice_des_distances <-
  fread("Stations Ski/station_la_plus_proche.csv",
        encoding = "UTF-8") |>
  mutate(duree = floor(duree / 60),
         duree = ifelse(duree >= 6,
                        6,
                        duree),
         duree = factor(duree,
                        levels = 0:6,
                        ordered = T))





communes <-
  st_read("...") |> 
  select(code) |> 
  arrange(code)



communes <-
  communes |> 
  left_join(matrice_des_distances,
            by = "code"); rm(matrice_des_distances)






# http://www.res.sports.gouv.fr/Accueil_Part.aspx

ski <-
  fread("Stations Ski/ski.csv",
        encoding = "UTF-8") |> 
  mutate(REG = ifelse(X > 1100000,
                      "94",
                      "11")) |> 
  st_as_sf(coords = c("X",
                      "Y"),
           crs = 2154) |> 
  transform_shapefile()



# Fonds de carte ----



france <- st_read("...") |> 
  st_union() |> 
  smoothr::smooth(method = "ksmooth",
                  smoothness = 10)




regions <- st_read("...") |> 
  smoothr::smooth(method = "ksmooth",
                  smoothness = 5)


lignes <-
  st_read("...")



# Carte ----


camcorder::gg_record(width = 21, height = 21, units = "cm")


ggplot() +
  # France
  geom_sf(data = st_buffer(france,10000),
          size = .2,
          fill = NA,
          color = alpha("#868686",.5)) +
  geom_sf(data = st_buffer(france,5000),
          size = .2,
          fill = NA,
          color = alpha("#868686",.8)) +
  # Communes
  geom_sf(data = communes,
          aes(fill = duree,
              color = duree),
          size = .5) +
  scale_fill_manual(values = grDevices::colorRampPalette(c("#F6AC56","#F3575A"))(7),
                    na.value = "#B0AEAE") +
  scale_color_manual(values = grDevices::colorRampPalette(c("#F6AC56","#F3575A"))(7),
                     na.value = "#B0AEAE",
                     guide = "none") +
  # Habillage
  geom_sf(data = lignes,
          color = "#868686",
          size = .4) +
  geom_sf(data = regions,
          fill = NA,
          color = "#262626",
          size = .2) +
  # Stations de ski
  ggnewscale::new_scale_color() +
  geom_sf(data = ski,
          aes(color = alt),
          alpha = .8,
          shape = 16) +
  NatParksPalettes::scale_color_natparks_c("Glacier",-1,
                                           guide = "none") +
  theme_void() +
  theme(legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.position = c(.215,.9),
        legend.title.align = .5,
        legend.key.width = unit(1.3,"cm"),
        legend.key.height = unit(.4,"cm"),
        plot.background = element_rect(fill = "#F2F2F2",
                                       color = "#F2F2F2"),
        panel.background = element_rect(fill = "#F2F2F2",
                                        color = "#F2F2F2")) +
  guides(fill = guide_colorsteps(title.position = "top"),
         color = "none") +
  coord_sf(st_bbox(france)[c(1,3)],
           st_bbox(france)[c(2,4)]) +
  labs(fill = "")


ggsave("Stations Ski/Figures/duree_trajet.png",
       width = 21, height = 21, units = "cm")  
