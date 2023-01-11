# Packages ----

library(tidyverse)
library(sf)
library(data.table)

# Données ----

temps_acces <-
  fread("Littoral/Temps_acces_littoral.csv") |> 
  mutate(duree = floor(duree / 60),
         duree = ifelse(duree >= 6,
                        6,
                        duree),
         duree = factor(duree,
                        levels = 0:6))



communes <-
  st_read("...") |> 
  select(code) |> 
  arrange(code) |> 
  left_join(temps_acces,
            by = "code") |> 
  group_by(duree) |> 
  summarise(geometry = st_union(geometry))


# Fonds de carte ----



france <- st_read("...") |> 
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
  scale_fill_manual(values = grDevices::colorRampPalette(c("#F2E6D8", "#99D0F2", "#04B2D9", "#049DD9","#0487D9"))(7),
                    na.value = "#B0AEAE") +
  scale_color_manual(values = grDevices::colorRampPalette(c("#F2E6D8", "#99D0F2", "#04B2D9", "#049DD9","#0487D9"))(7),
                     na.value = "#B0AEAE") +
  # Habillage
  geom_sf(data = lignes,
          color = "#868686",
          size = .4) +
  geom_sf(data = regions,
          fill = NA,
          color = "#262626",
          size = .2) +
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

ggsave("Littoral/Figures/acces_plage.png",
       width = 21, height = 21, units = "cm")  




